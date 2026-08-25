(ns datahike.pg.numeric-format
  "PostgreSQL-compatible numeric format pictures for to_char/to_number.

   PostgreSQL's numeric pictures are a small language rather than Java
   DecimalFormat patterns. Keeping the parser and both directions together is
   important: D/G, V, signs, fill mode, Roman numerals and quoted literals must
   mean the same thing when formatting and parsing."
  (:require [clojure.string :as str]
            [datahike.pg.errors :as errors]
            [datahike.pg.types :as types]))

(def ^:private token-specs
  [["EEEE" :eeee] ["FM" :fm] ["MI" :mi] ["PL" :pl] ["PR" :pr]
   ["RN" :rn] ["SG" :sg] ["SP" :sp] ["TH" :th]
   ["9" :nine] ["0" :zero] ["." :decimal] ["," :comma]
   ["B" :blank] ["C" :currency-code] ["D" :locale-decimal]
   ["G" :locale-group] ["L" :currency] ["S" :sign] ["V" :scale]])

(defn- token-at [^String picture i]
  (let [tail (.substring picture i)]
    (some (fn [[text kind]]
            (when (and (<= (count text) (count tail))
                       (= (str/upper-case text)
                          (str/upper-case (.substring tail 0 (count text)))))
              {:kind kind :text (.substring tail 0 (count text))
               :size (count text)}))
          token-specs)))

(defn parse-picture
  "Tokenize a PostgreSQL numeric format picture. Double-quoted regions are
   literal; a backslash only escapes characters while inside such a region,
   matching formatting.c's format parser."
  [picture]
  (let [^String picture (str picture)
        n (.length picture)]
    (loop [i 0 quoted? false nodes []]
      (if (>= i n)
        nodes
        (let [ch (.charAt picture i)]
          (cond
            (= ch \") (recur (inc i) (not quoted?) nodes)

            (and (not quoted?) (= ch \\) (< (inc i) n)
                 (= \" (.charAt picture (inc i))))
            (recur (+ i 2) quoted? (conj nodes {:kind :literal :text "\""}))

            (and quoted? (= ch \\) (< (inc i) n))
            (recur (+ i 2) quoted?
                   (conj nodes {:kind :literal
                                :text (str (.charAt picture (inc i)))}))

            quoted?
            (recur (inc i) quoted? (conj nodes {:kind :literal :text (str ch)}))

            :else
            (if-let [{:keys [size] :as token} (token-at picture i)]
              (recur (+ i size) quoted? (conj nodes (dissoc token :size)))
              (recur (inc i) quoted? (conj nodes {:kind :literal :text (str ch)})))))))))

(defn- picture-error
  ([message] (picture-error message nil))
  ([message detail]
   (throw (errors/pg-error :invalid-parameter-value
                           (cond-> {:message message} detail (assoc :detail detail))))))

(defn- roman-error [message]
  (throw (ex-info message {:error :invalid-text-representation
                           :sqlstate "22P02"})))

(def ^:private roman-pairs
  [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"]
   [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]])

(defn- int->roman [n]
  (when (<= 1 n 3999)
    (loop [n (long n) pairs roman-pairs out (StringBuilder.)]
      (if-let [[value letters] (first pairs)]
        (if (>= n value)
          (do (.append out ^String letters) (recur (- n value) pairs out))
          (recur n (next pairs) out))
        (str out)))))

(defn- roman->int [input]
  (let [trimmed (str/triml (str input))
        word (or (re-find #"(?i)^[MDCLXVI]+" trimmed) "")
        upper (str/upper-case word)]
    (when (empty? word) (roman-error "invalid Roman numeral"))
    (let [value (loop [s upper pairs roman-pairs total 0]
                  (if (empty? s)
                    total
                    (if-let [[v letters] (some #(when (str/starts-with? s (second %)) %)
                                               pairs)]
                      (recur (subs s (count letters)) pairs (+ total v))
                      (roman-error "invalid Roman numeral"))))]
      (when (not= upper (int->roman value)) (roman-error "invalid Roman numeral"))
      value)))

(defn- numeric-special-kind [value]
  (cond
    (types/numeric-special? value) (:kind value)
    (and (or (instance? Double value) (instance? Float value))
         (Double/isNaN (double value))) :nan
    (and (or (instance? Double value) (instance? Float value))
         (Double/isInfinite (double value)))
    (if (neg? (double value)) :-inf :inf)
    :else nil))

(defn- decimal-value ^java.math.BigDecimal [value]
  (cond
    (instance? java.math.BigDecimal value) value
    (instance? java.math.BigInteger value) (java.math.BigDecimal. ^java.math.BigInteger value)
    (integer? value) (java.math.BigDecimal/valueOf (long value))
    :else (java.math.BigDecimal/valueOf (double value))))

(defn- ordinal-suffix [n lower?]
  (let [n (Math/abs (long n))
        suffix (if (<= 11 (mod n 100) 13)
                 "TH"
                 (case (mod n 10) 1 "ST" 2 "ND" 3 "RD" "TH"))]
    (if lower? (str/lower-case suffix) suffix)))

(defn- analyze [nodes]
  (let [kinds (mapv :kind nodes)
        decimal-idx (first (keep-indexed #(when (#{:decimal :locale-decimal} %2) %1) kinds))
        scale-idx (first (keep-indexed #(when (= :scale %2) %1) kinds))
        split-idx (or decimal-idx scale-idx (count nodes))
        digit? #{:nine :zero}
        pre-indexes (vec (keep-indexed #(when (and (< %1 split-idx) (digit? %2)) %1) kinds))
        post-indexes (vec (keep-indexed #(when (and (> %1 split-idx) (digit? %2)) %1) kinds))]
    {:fill? (boolean (some #{:fm} kinds))
     :roman-nodes (keep-indexed #(when (= :rn %2) %1) kinds)
     :scientific? (boolean (some #{:eeee} kinds))
     :decimal-idx decimal-idx :scale-idx scale-idx
     :pre-indexes pre-indexes :post-indexes post-indexes
     :explicit-sign? (boolean (some #{:sign :sg :mi :pl :pr} kinds))
     :pr? (boolean (some #{:pr} kinds))}))

(defn- validate-roman-picture! [nodes]
  (let [roman-count (count (filter #(= :rn (:kind %)) nodes))
        other (remove #(#{:rn :fm :literal} (:kind %)) nodes)]
    (when (> roman-count 1) (picture-error "cannot use \"RN\" twice"))
    (when (and (= roman-count 1) (seq other))
      (picture-error "\"RN\" is incompatible with other formats"
                     "\"RN\" may only be used together with \"FM\"."))))

(defn- scientific-string [^java.math.BigDecimal value decimals]
  (let [negative? (neg? (.signum value))
        a (.abs value)
        exponent (if (zero? (.signum a)) 0 (dec (- (.precision a) (.scale a))))
        mantissa (if (zero? (.signum a)) java.math.BigDecimal/ZERO (.movePointLeft a exponent))
        mantissa (.setScale mantissa decimals java.math.RoundingMode/HALF_UP)
        [mantissa exponent] (if (>= (.compareTo mantissa java.math.BigDecimal/TEN) 0)
                              [(.movePointLeft mantissa 1) (inc exponent)]
                              [mantissa exponent])
        exp-sign (if (neg? exponent) "-" "+")
        exp-digits (format "%02d" (Math/abs (long exponent)))]
    (str (if negative? "-" " ") (.toPlainString mantissa) "e" exp-sign exp-digits)))

(defn- special-output
  [kind nodes {:keys [fill? scientific? pre-indexes decimal-idx float4?]}]
  (let [label (case kind :nan "NaN" :inf "Infinity" :-inf "-Infinity")
        negative? (= kind :-inf)
        sign-kind (some #(when (#{:sign :sg :mi :pl :pr} (:kind %)) (:kind %)) nodes)
        sign-text (case sign-kind
                    (:sign :sg) (if negative? "-" "+")
                    :mi (if negative? "-" (if fill? "" " "))
                    :pl (if negative? (if fill? "" " ") "+")
                    :pr (if negative? "<" " ")
                    (if negative? "-" (if fill? "" " ")))
        finite-label (if negative? "Infinity" label)]
    (cond
      scientific?
      (str " " (apply str
                      (map (fn [{:keys [kind text]}]
                             (case kind
                               (:nine :zero) "#"
                               (:decimal :locale-decimal) "."
                               :eeee "####"
                               :fm ""
                               text)) nodes)))
      (and fill? (= kind :nan)) label
      (<= (count finite-label) (count pre-indexes))
      (let [field (format (str "%" (count pre-indexes) "s") finite-label)]
        (if (= sign-kind :pr)
          (if negative? (str "<" (str/triml field) ">") (str " " field " "))
          (str sign-text field)))
      :else
      (let [body (apply str
                        (map-indexed
                         (fn [i {node-kind :kind text :text}]
                           (case node-kind
                             (:nine :zero) (if (and float4?
                                                    (#{:inf :-inf} kind)
                                                    decimal-idx
                                                    (> i decimal-idx))
                                             "" "#")
                             (:decimal :locale-decimal) "."
                             (:comma :locale-group) (if fill? "" " ")
                             :fm ""
                             :eeee "####"
                             (:sign :sg) (if negative? "-" "+")
                             :mi (if negative? "-" (if fill? "" " "))
                             :pl (if negative? (if fill? "" " ") "+")
                             :pr ""
                             text))
                         nodes))]
        (if sign-kind
          (if (= sign-kind :pr)
            (if negative? (str "<" body ">") (str " " body " "))
            body)
          (str (if negative? "-" (if fill? "" " ")) body))))))

(defn to-char
  "Format a numeric value with a PostgreSQL numeric picture. Locale-sensitive
  tokens currently use the C-locale spellings, which is also what the
  regression campaign fixes with lc_numeric = C."
  [value picture]
  (let [nodes (parse-picture picture)
        desc (assoc (analyze nodes) :float4? (instance? Float value))
        {:keys [fill? roman-nodes scientific? decimal-idx scale-idx
                pre-indexes post-indexes explicit-sign? pr?]} desc]
    (if (seq roman-nodes)
      (do
        (validate-roman-picture! nodes)
        (let [n (when-not (numeric-special-kind value)
                  (.longValue (decimal-value value)))
              roman (and n (int->roman n))
              upper? (= "RN" (:text (nth nodes (first roman-nodes))))
              out (if roman (if upper? roman (str/lower-case roman)) "###############")]
          (if fill? out (format "%15s" out))))
      (if-let [special (numeric-special-kind value)]
        (special-output special nodes desc)
        (let [^java.math.BigDecimal original (decimal-value value)
              negative? (neg? (.signum original))
              ^java.math.BigDecimal absolute (.abs original)
              scale-digits (count post-indexes)]
          (if scientific?
            (scientific-string original scale-digits)
            (let [^java.math.BigDecimal scaled (if scale-idx
                                                 (.movePointRight absolute scale-digits)
                                                 absolute)
                  decimals (if decimal-idx scale-digits 0)
                  ^java.math.BigDecimal rounded (.setScale scaled decimals
                                                           java.math.RoundingMode/HALF_UP)
                  plain (.toPlainString rounded)
                  [integer fraction] (let [[a b] (str/split plain #"\." 2)] [a (or b "")])
                ;; float4/float8 reach numeric formatting through PG's
                ;; six/fifteen-significant-digit conversion. Once the
                ;; integer part consumes that budget, fractional picture
                ;; slots disappear (and an overflowing picture hashes only
                ;; the remaining budget). NUMERIC itself has no such cap.
                  float-frac-limit (when (or (instance? Float value)
                                             (instance? Double value))
                                     (max 0 (- (if (instance? Float value) 6 15)
                                               (count integer))))
                  integer-indexes (if scale-idx
                                    (vec (concat pre-indexes post-indexes))
                                    pre-indexes)
                  fractional-indexes (if decimal-idx
                                       (cond->> post-indexes
                                         float-frac-limit (take float-frac-limit)
                                         true vec)
                                       [])
                  overflow? (> (count integer) (count integer-indexes))
                  zero-start (first (keep-indexed
                                     (fn [pos idx]
                                       (when (= :zero (:kind (nth nodes idx))) pos))
                                     integer-indexes))
                  integer-digits (if (and (zero? (.signum rounded))
                                          (nil? zero-start)
                                          (not fill?))
                                   (if decimal-idx [] [\0])
                                   (if (and (zero? (.signum rounded)) fill?)
                                     (if (and (seq fractional-indexes)
                                              (= :zero (:kind (nth nodes
                                                                   (first fractional-indexes)))))
                                       [] [\0])
                                     (reverse integer)))
                  int-map (loop [idxs (reverse integer-indexes) digits integer-digits out {}]
                            (if-let [idx (first idxs)]
                              (let [digit (first digits)
                                    pos (.indexOf ^java.util.List integer-indexes idx)
                                    ch (cond overflow? "#"
                                             digit (str digit)
                                             (and (some? zero-start) (>= pos zero-start)) "0"
                                             :else (if fill? "" " "))]
                                (recur (next idxs) (if digit (next digits) digits)
                                       (assoc out idx ch)))
                              out))
                  first-digit (first integer-indexes)
                  [int-map sign-prefix]
                  (if explicit-sign?
                    [int-map ""]
                    (if negative?
                      (if-let [blank-idx (last (filter #(= " " (get int-map %)) integer-indexes))]
                        [(assoc int-map blank-idx "-") " "]
                        [int-map "-"])
                      [int-map (if fill? "" " ")]))
                  pre-sign-index (first (keep-indexed
                                         (fn [i node]
                                           (when (and (< i (or first-digit Integer/MAX_VALUE))
                                                      (= :sign (:kind node)))
                                             i)) nodes))
                  sign-char (if negative? "-" "+")
                  sign-blank-index
                  (when pre-sign-index
                    (last (filter #(= " " (get int-map %)) integer-indexes)))
                  deferred-sign-index
                  (when (and pre-sign-index (nil? sign-blank-index))
                    (first (filter #(seq (get int-map %)) integer-indexes)))
                  [int-map sign-node-map]
                  (if (and pre-sign-index (not pr?))
                    (cond
                      sign-blank-index
                      [(assoc int-map sign-blank-index sign-char)
                       {pre-sign-index
                        (if (and (< sign-blank-index (dec (count nodes)))
                                 (str/blank? (:text (nth nodes (inc sign-blank-index)))))
                          "  " " ")}]

                      deferred-sign-index
                      [int-map {pre-sign-index ""}]

                      :else [int-map {pre-sign-index sign-char}])
                    [int-map {}])
                  frac-map (into {}
                                 (let [required-through
                                       (max (or (last (keep-indexed
                                                       (fn [i ch] (when (not= ch \0) i))
                                                       fraction)) -1)
                                            (or (last (keep-indexed
                                                       (fn [i idx]
                                                         (when (= :zero (:kind (nth nodes idx))) i))
                                                       fractional-indexes)) -1))]
                                   (map-indexed
                                    (fn [i idx]
                                      (let [digit (nth fraction i \0)
                                            zero-slot? (= :zero (:kind (nth nodes idx)))
                                            shown? (or (not fill?) zero-slot?
                                                       (<= i required-through))]
                                        [idx (cond overflow? "#"
                                                   shown? (str digit)
                                                   :else "")]))
                                    fractional-indexes)))
                  rendered
                  (apply str
                         (map-indexed
                          (fn [i {:keys [kind text]}]
                            (case kind
                              :nine (str (when (= i first-digit) sign-prefix)
                                         (when (= i deferred-sign-index) sign-char)
                                         (or (int-map i) (frac-map i) ""))
                              :zero (str (when (= i first-digit) sign-prefix)
                                         (when (= i deferred-sign-index) sign-char)
                                         (if (and decimal-idx (> i decimal-idx)
                                                  (not (contains? frac-map i)))
                                           ""
                                           (or (int-map i) (frac-map i) "0")))
                              :decimal (if (or (and fill? (empty? post-indexes))
                                               (and (zero? (or float-frac-limit 1))
                                                    (not overflow?))) "" ".")
                              :locale-decimal (if (or (and fill? (empty? post-indexes))
                                                      (and (zero? (or float-frac-limit 1))
                                                           (not overflow?))) "" ".")
                              (:comma :locale-group)
                              (cond
                                (and decimal-idx (> i decimal-idx))
                                (if (= kind :locale-group) "," text)
                                fill? ""
                                (not-any? #(not= " " (get int-map % " "))
                                          (filter #(< % i) pre-indexes)) " "
                                :else (if (= kind :locale-group) "," text))
                              :currency (if fill? "$" " ")
                              :currency-code (if fill? "USD" "   ")
                              :blank (if (zero? (.signum rounded)) " " "")
                              :fm ""
                              (:eeee :scale) ""
                              (:sign :sg) (or (sign-node-map i) sign-char)
                              :mi (or (sign-node-map i)
                                      (if negative? "-"
                                          (if (or fill?
                                                  (and (zero? (or float-frac-limit 1))
                                                       (> i (or (last integer-indexes) -1))))
                                            "" " ")))
                              :pl (or (sign-node-map i)
                                      (if negative? (if fill? "" " ") "+"))
                              :pr ""
                              :th (if (and (not negative?) (zero? decimals))
                                    (ordinal-suffix (.longValue rounded)
                                                    (= text (str/lower-case text))) "")
                              :rn ""
                              (if (and sign-blank-index
                                       (= i (inc sign-blank-index))
                                       (str/blank? text))
                                ""
                                text)))
                          nodes))
                  rendered (if pr?
                             (if negative?
                               (let [out (str "<" (str/trim rendered) ">")]
                                 (if fill?
                                   out
                                   (format (str "%" (+ (count pre-indexes)
                                                       (count post-indexes)
                                                       (if decimal-idx 1 0) 2) "s") out)))
                               (if fill? (str/trim rendered) (str " " rendered " ")))
                             rendered)]
              rendered)))))))

(defn- roman-picture? [nodes] (some #(= :rn (:kind %)) nodes))

(defn to-number
  "Parse text according to a PostgreSQL numeric picture. The numeric parser is
   intentionally permissive about punctuation and spacing, as PostgreSQL's
   NUM_processor is; the picture determines decimal placement and V scaling."
  [input picture]
  (let [input (str input)
        nodes (parse-picture picture)]
    (if (roman-picture? nodes)
      (do (validate-roman-picture! nodes)
          (when (empty? input)
            (throw (errors/pg-error :invalid-text-representation
                                    {:type "numeric" :value " "})))
          (java.math.BigDecimal/valueOf (long (roman->int input))))
      (let [{:keys [decimal-idx scale-idx post-indexes]} (analyze nodes)
            negative? (or (str/includes? input "-")
                          (and (str/includes? input "<") (str/includes? input ">")))
            decimal? (some? decimal-idx)
            decimal-char (when decimal? \.)
            trailing-group? (= :locale-group
                               (:kind (last (remove #(#{:fm :literal} (:kind %)) nodes))))
            input (if (and trailing-group? (str/includes? input ","))
                    (subs input 0 (.indexOf ^String input ","))
                    input)
            cleaned (apply str
                           (keep (fn [ch]
                                   (cond
                                     (Character/isDigit ^char ch) ch
                                     (and decimal-char (= ch decimal-char)) ch
                                     :else nil))
                                 input))
            cleaned (cond
                      (empty? cleaned) "0"
                      (str/starts-with? cleaned ".") (str "0" cleaned)
                      :else cleaned)
            value (try (java.math.BigDecimal. cleaned)
                       (catch NumberFormatException _
                         (throw (errors/pg-error :invalid-text-representation
                                                 {:type "numeric" :value input}))))
            value (if scale-idx
                    (.setScale (.movePointLeft value (count post-indexes)) 18
                               java.math.RoundingMode/UNNECESSARY)
                    value)]
        (if negative? (.negate value) value)))))
