(ns datahike.test.pg-numeric-transcendentals-test
  "sqrt / exp / ln / log / log10 / power over `numeric`.

   PostgreSQL declares BOTH a float8 and a numeric overload of each, and
   function resolution picks the numeric one whenever an argument is
   numeric. We answered float8 for all six -- wrong in two ways at once:
   the reported type, and the precision. `2.0 ^ 10` is
   1024.0000000000000 in PostgreSQL, a scale that says how many digits
   are meaningful; we answered 1024.

   The scale is not the operands'. Each function has its own rule in
   numeric.c, all of the shape \"enough digits for
   NUMERIC_MIN_SIG_DIGITS significant figures, but never fewer than the
   input already shows\" -- numeric_sqrt halves the weight,
   numeric_exp scales by log10(e), numeric_ln uses estimate_ln_dweight,
   power_var_int uses exp*log10(base).

   Expectations are a PostgreSQL 17 oracle's."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [java.sql Connection DriverManager SQLException]))

(def ^:dynamic *port* nil)

(defn nt-fixture [f]
  (pg/reset-lock-registry!)
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)} :max-string-length 0
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)
          {:keys [server]} (pg/start-server {"t" conn} {:port 0})]
      (try (binding [*port* (.getPort server)] (f))
           (finally (.stop server) (d/release conn) (d/delete-database cfg))))))

(use-fixtures :each nt-fixture)

(defn- ^Connection jdbc []
  (DriverManager/getConnection
   (str "jdbc:postgresql://127.0.0.1:" *port*
        "/t?user=x&password=x&sslmode=disable&binaryTransfer=false")))

(defn- one [^Connection c sql]
  (with-open [st (.createStatement c) rs (.executeQuery st sql)]
    (when (.next rs) (.getString rs 1))))

(deftest a-numeric-argument-selects-the-numeric-overload
  (with-open [c (jdbc)]
    (is (= "numeric" (one c "SELECT pg_typeof(sqrt(2.0))")))
    (is (= "double precision" (one c "SELECT pg_typeof(sqrt(2))"))
        "all-integer arguments stay float8 -- float8 is the preferred
         type in the NUMERIC category")
    (is (= "numeric" (one c "SELECT pg_typeof(power(2.0,3))"))
        "ONE numeric argument is enough")
    (is (= "numeric" (one c "SELECT pg_typeof(exp(1.0))")))))

(deftest each-function-carries-its-own-scale
  (with-open [c (jdbc)]
    (is (= "1.414213562373095" (one c "SELECT sqrt(2.0)")))
    (is (= "2.000000000000000" (one c "SELECT sqrt(4.0)")))
    (is (= "2.7182818284590452" (one c "SELECT exp(1.0)")))
    (is (= "0.6931471805599453" (one c "SELECT ln(2.0)")))
    (is (= "2.0000000000000000" (one c "SELECT log(100.0)")))
    (is (= "3.0000000000000000" (one c "SELECT log10(1000.0)")))
    (is (= "1024.0000000000000" (one c "SELECT power(2.0,10)")))))

(deftest the-power-operator-resolves-the-same-way
  (with-open [c (jdbc)]
    (is (= "1024.0000000000000" (one c "SELECT 2.0 ^ 10"))
        "`^` is numeric_power when an operand is numeric, like power()")
    (is (= "1024" (one c "SELECT 2 ^ 10")) "and float8 otherwise")
    (is (= "2.0000000000000000" (one c "SELECT power(4.0,0.5)"))
        "a non-integer exponent goes through exp(y*ln(x))")))

(deftest two-argument-log-is-always-numeric
  (with-open [c (jdbc)]
    (is (= "6.0000000000000000" (one c "SELECT log(2,64)"))
        "PostgreSQL has no float8 two-argument log, so even integer
         arguments coerce to numeric")
    (is (= "numeric" (one c "SELECT pg_typeof(log(2,64))")))))

(deftest domain-errors-keep-their-own-sqlstates
  (with-open [c (jdbc)]
    (is (thrown-with-msg? SQLException #"cannot take logarithm of zero"
                          (one c "SELECT ln(0.0)")))
    (is (thrown-with-msg? SQLException #"cannot take logarithm of a negative number"
                          (one c "SELECT ln(-1.0)")))
    (is (thrown-with-msg? SQLException #"cannot take logarithm of zero"
                          (one c "SELECT log(0.0,10.0)"))
        "the BASE is what is at fault here, and the message has to say so")
    (is (thrown-with-msg? SQLException #"division by zero"
                          (one c "SELECT log(1.0,10.0)"))
        "ln(1) is 0, so log base 1 divides by zero rather than erroring
         as a logarithm")
    (is (thrown-with-msg? SQLException #"cannot take square root of a negative"
                          (one c "SELECT sqrt(-1.0)")))
    (is (thrown-with-msg? SQLException #"zero raised to a negative power"
                          (one c "SELECT power(0.0,-1.0)")))
    (is (thrown-with-msg? SQLException #"negative number raised to a non-integer"
                          (one c "SELECT power(-1.0,0.5)")))))

(deftest finite-power-range-boundaries
  (with-open [c (jdbc)]
    (testing "PostgreSQL numeric.sql lines 1284-1285 round far underflow to zero"
      (doseq [exponent [-2147483648 -2147483647]]
        (is (= "t" (one c (str "SELECT 10.0 ^ " exponent " = 0"))))
        (is (= "1000" (one c (str "SELECT scale(10.0 ^ " exponent ")"))))))
    (testing "lines 1286-1287 detect overflow before BigInteger allocation"
      (doseq [sql ["SELECT 10.0 ^ 2147483647"
                   "SELECT 117743296169.0 ^ 1000000000"]]
        (try
          (one c sql)
          (is false sql)
          (catch SQLException e
            (is (= "22003" (.getSQLState e)))
            (is (re-find #"value overflows numeric format" (.getMessage e)))))))
    (testing "arbitrary-size exponents of -1 need only parity"
      (is (= "-1.0000000000000000" (one c "SELECT (-1.0) ^ 2147483647")))
      (is (= "1.0000000000000000" (one c "SELECT (-1.0) ^ 2147483648"))))))

(deftest formerly-inaccurate-power-results
  (with-open [c (jdbc)]
    (testing "PostgreSQL numeric.sql lines 1290-1301 retain every displayed digit"
      (doseq [[sql expected]
              [["SELECT 3.789 ^ 21.0000000000000000"
                "1409343026052.8716016316022141"]
               ["SELECT 3.789 ^ 35.0000000000000000"
                "177158169650516670809.3820586142670135"]
               ["SELECT 1.2 ^ 345"
                "2077446682327378559843444695.6"]
               ["SELECT 0.12 ^ (-20)"
                "2608405330458882702.55"]
               ["SELECT 1.000000000123 ^ (-2147483648)"
                "0.7678656556403084"]
               ["SELECT coalesce(nullif(0.9999999999 ^ 23300000000000, 0), 0)"
                "0"]
               [(str "SELECT round(((1 - 1.500012345678e-1000)"
                     " ^ 1.45e1003) * 1e1000)")
                "25218976308958387188077465658068501556514992509509282366"]
               ["SELECT 0.12 ^ (-25)"
                "104825960103961013959336.50"]
               ["SELECT 0.5678 ^ (-85)"
                "782333637740774446257.7719"]
               ["SELECT coalesce(nullif(0.9999999999 ^ 70000000000000, 0), 0)"
                "0"]]]
        (is (= expected (one c sql)) sql)))))

(deftest non-integral-power-precision
  (with-open [c (jdbc)]
    (testing "PostgreSQL numeric.sql lines 1336-1342"
      (doseq [[input expected]
              [["32.1 ^ 9.8" "580429286790711.10"]
               ["32.1 ^ (-9.8)" "0.000000000000001722862754788209"]
               ["12.3 ^ 45.6"
                "50081010321492803393171165777624533697036806969694.9"]
               ["12.3 ^ (-45.6)"
                (str "0.0000000000000000000000000000000000000000000000000"
                     "1996764828785491")]
               ["1.234 ^ 5678"
                (str "307239295662090741644584872593956173493568238595074141254349565406661439636598896798876823220904084953233015553994854875890890858118656468658643918169805277399402542281777901029346337707622181574346585989613344285010764501017625366742865066948856161360224801370482171458030533346309750557140549621313515752078638620714732831815297168231790779296290266207315344008883935010274044001522606235576584215999260117523114297033944018699691024106823438431754073086813382242140602291215149759520833200152654884259619588924545324"
                     ".597")]]]
        (is (= expected (one c (str "SELECT " input))) input)))))

(deftest exponential-working-precision
  (with-open [c (jdbc)]
    (testing "PostgreSQL numeric.sql lines 1359-1363"
      (doseq [[input expected]
              [["32.999" "214429043492155.053"]
               ["-32.999" "0.000000000000004663547361468248"]
               ["123.456"
                "413294435277809344957685441227343146614594393746575438.725"]
               ["-123.456"
                (str "0.00000000000000000000000000000000000000000000000000000"
                     "2419582541264601")]
               ["1234.5678"
                (str "146549072930959479983482138503979804217622199675223653966270157446954995433819741094410764947112047906012815540251009949604426069672532417736057033099274204598385314594846509975629046864798765888104789074984927709616261452461385220475510438783429612447831614003668421849727379202555580791042606170523016207262965336641214601082882495255771621327088265411334088968112458492660609809762865582162764292604697957813514621259353683899630997077707406305730694385703091201347848855199354307506425820147289848677003277208302716466011827836279231"
                     ".9667")]]]
        (is (= expected (one c (str "SELECT exp(" input ")"))) input)))))

(deftest square-root-rounding-thresholds
  (with-open [c (jdbc)]
    (doseq [[input expected]
            [["1.000000000000003" "1.000000000000001"]
             ["1.000000000000004" "1.000000000000002"]
             ["96627521408608.56340355805" "9829929.87811248648"]
             ["96627521408608.56340355806" "9829929.87811248649"]
             ["515549506212297735.073688290367" "718017761.766585921184"]
             ["515549506212297735.073688290368" "718017761.766585921185"]
             ["8015491789940783531003294973900306" "89529278953540017"]
             ["8015491789940783531003294973900307" "89529278953540018"]]]
      (is (= expected (one c (str "SELECT sqrt(" input "::numeric)")))))))
