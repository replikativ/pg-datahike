(ns datahike.pg.datetime.lex
  "`ParseDateTime` (datetime.c:753-977): the datetime LEXER.

   It splits a literal into fields and tags each with a TYPE, without
   deciding what any of them means. That split -- lex, then interpret --
   is the whole reason this exists rather than a regex: the meaning of a
   field depends on the other fields present and on DateStyle, and a
   pattern cannot carry that. A bare three-digit number is a day-of-year
   if a year is already set and a month otherwise; `01/02/03` is three
   different dates; `Mon Feb 10 17:32:01 1997 PST` has a word that is a
   month, a word that is a weekday to discard, and a word that is a zone.

   Six field types, and the doc comment at datetime.c:740-752 is worth
   repeating because it is the only place that says the quiet part --
   several of them hold things their names do not suggest:

     :number   digits and possibly a decimal point. ALSO holds a date:
               `yy.ddd`.
     :string   text with no digits or punctuation. ALSO holds months
               (`january`) and zone abbreviations (`pst`).
     :date     digits with two delimiters, or digits and text. ALSO
               holds zone NAMES: `america/new_york`, `gmt-8`.
     :time     digits with colon delimiters, possibly a decimal point.
     :tz       a leading `+` or `-` then digits (and it eats `:`, `.`
               and `-` as well -- see the note on that below).
     :special  a leading `+` or `-` then text.

   Alpha runs are lowercased; digits and punctuation are not. The fold is
   `pg_tolower`, which is ASCII-only, so it goes through
   `tokens/ascii-lower` rather than `clojure.string/lower-case`.

   Punctuation that is not part of a field is DISCARDED as a delimiter
   (datetime.c:929-934). So a double-quoted zone in a literal is not a
   quoted anything: `'2000-01-01 12:00 \"PST\"'` lexes exactly like
   `… PST`, because `\"` is punctuation. There is no quoted-zone concept
   at the literal level at all.

   Returns a vector of `{:text :type}`, or throws `bad-format`."
  (:require [datahike.pg.datetime.tokens :as tokens]))

(def ^:const max-fields
  "MAXDATEFIELDS (datetime.h:202). More fields than this is a format
   error, not a truncation."
  25)

(defn bad-format
  "DTERR_BAD_FORMAT. The caller turns it into 22007 with the type name
   and the original string, which only it knows."
  []
  (ex-info "invalid datetime format" {::dterr :bad-format}))

(defn- digit? [c] (and (>= (int c) 48) (<= (int c) 57)))

(defn- alpha? [c]
  (let [i (int c)]
    (or (and (>= i 65) (<= i 90)) (and (>= i 97) (<= i 122)))))

(defn- alnum? [c] (or (digit? c) (alpha? c)))

(defn- space? [c] (Character/isWhitespace c))

(defn- punct?
  "`ispunct` in the C locale: a printable, non-alphanumeric,
   non-space ASCII character."
  [c]
  (let [i (int c)]
    (and (> i 32) (< i 127) (not (alnum? c)))))

(defn- at ^long [^String s ^long i]
  (if (< i (.length s)) (int (.charAt s i)) -1))

(defn- ch [^String s ^long i] (.charAt s i))

(defn- take-while-from
  "The index after the run of characters from `i` satisfying `pred`."
  ^long [^String s ^long i pred]
  (let [n (.length s)]
    (loop [i i] (if (and (< i n) (pred (ch s i))) (recur (inc i)) i))))

(defn tokenize
  "`ParseDateTime`. The literal as `[{:text :type} …]`.

   The scanner's branch order is PostgreSQL's and the order matters: a
   leading digit is examined by what FOLLOWS it, and three of the five
   sub-cases hinge on whether the character after a delimiter is itself
   a digit."
  [^String input]
  (let [s input
        n (.length s)
        lower (fn [^String t] (tokens/ascii-lower t))]
    (loop [i 0, out []]
      (cond
        (>= i n) out

        (space? (ch s i)) (recur (inc i) out)

        (>= (count out) max-fields) (throw (bad-format))

        (digit? (ch s i))
        (let [j (take-while-from s i digit?)
              c (at s j)]
          (cond
            ;; `hh:mm…` -- and the run then eats digits, colons AND dots,
            ;; so `04:05:06.5` is ONE field.
            (= c (int \:))
            (let [k (take-while-from s (inc j)
                                     #(or (digit? %) (= \: %) (= \. %)))]
              (recur k (conj out {:text (subs s i k) :type :time})))

            ;; A delimiter. What follows it decides everything.
            (or (= c (int \-)) (= c (int \/)) (= c (int \.)))
            (let [delim (ch s j)
                  k (inc j)]
              (if (and (< k n) (digit? (ch s k)))
                ;; digits-delim-digits. A `.` leaves it a NUMBER -- which
                ;; is how `1997.038` stays a day-of-year candidate -- and
                ;; only a MATCHING second delimiter promotes it to a date.
                ;; That is why `2000.01`, `2000.01.01` and `2000.01-01`
                ;; take three different paths (datetime.c:815-818).
                (let [m (take-while-from s k digit?)]
                  (if (and (< m n) (= (ch s m) delim))
                    (let [e (take-while-from s (inc m)
                                             #(or (digit? %) (= delim %)))]
                      (recur e (conj out {:text (subs s i e) :type :date})))
                    (recur m (conj out {:text (subs s i m)
                                        :type (if (= delim \.) :number :date)}))))
                ;; digits-delim-TEXT: a zone name like `gmt-8`, lowercased.
                (let [e (take-while-from s k #(or (alnum? %) (= delim %)))]
                  (recur e (conj out {:text (lower (subs s i e)) :type :date})))))

            :else (recur j (conj out {:text (subs s i j) :type :number}))))

        (= (ch s i) \.)
        (let [j (take-while-from s (inc i) digit?)]
          (recur j (conj out {:text (subs s i j) :type :number})))

        (alpha? (ch s i))
        (let [j (take-while-from s i alpha?)
              word (lower (subs s i j))
              c (at s j)
              ;; A word followed by a delimiter is a date. A word
              ;; followed by `+` or a digit is a date ONLY IF the word is
              ;; not a known token -- which is what makes `gmt+8` one
              ;; field while `jan 8` stays two.
              date? (or (= c (int \-)) (= c (int \/)) (= c (int \.))
                        (and (or (= c (int \+)) (and (>= c 48) (<= c 57)))
                             (nil? (tokens/special-token word))))]
          (if date?
            ;; `do … while`: the first character is consumed before the
            ;; test, so the delimiter itself is always taken. The run then
            ;; includes `/ _ . :` as well as `+ -` and alnum, because a
            ;; zone NAME contains them -- `america/new_york` is one field,
            ;; and without `_` it would split and the zone would not
            ;; resolve (datetime.c:894-897).
            (let [e (take-while-from s (inc j)
                                     #(or (= \+ %) (= \- %) (= \/ %)
                                          (= \_ %) (= \. %) (= \: %)
                                          (alnum? %)))]
              (recur e (conj out {:text (lower (subs s i e)) :type :date})))
            (recur j (conj out {:text word :type :string}))))

        (or (= (ch s i) \+) (= (ch s i) \-))
        (let [sign (ch s i)
              j (take-while-from s (inc i) space?)]
          (cond
            (and (< j n) (digit? (ch s j)))
            ;; The run eats `:`, `.` and `-` as well. That is a guard,
            ;; not a feature: `DecodeTimezone` then REJECTS every one of
            ;; them but `:`. Without the greedy eat, `+05.5` would lex as
            ;; a zone and a separate `.5`, and the `.5` would land in
            ;; fsec -- a silently wrong value instead of an error. The
            ;; `-` is there for DecodeInterval's signed year-month.
            (let [e (take-while-from s (inc j)
                                     #(or (digit? %) (= \: %) (= \. %) (= \- %)))]
              (recur e (conj out {:text (str sign (subs s j e)) :type :tz})))

            (and (< j n) (alpha? (ch s j)))
            (let [e (take-while-from s j alpha?)]
              (recur e (conj out {:text (str sign (lower (subs s j e)))
                                  :type :special})))

            :else (throw (bad-format))))

        ;; Other punctuation is DISCARDED and acts as a delimiter.
        (punct? (ch s i)) (recur (inc i) out)

        :else (throw (bad-format))))))
