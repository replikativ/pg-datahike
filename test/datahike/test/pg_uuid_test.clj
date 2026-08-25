(ns datahike.test.pg-uuid-test
  "PostgreSQL UUID input, generation, and extraction compatibility."
  (:require [clojure.test :refer [deftest is]]
            [datahike.pg.sql.coerce :as coerce]
            [datahike.pg.sql.expr :as expr]
            [datahike.pg.sql.fns :as fns]))

(deftest generated-uuid-versions
  (let [v7s (repeatedly 8 coerce/generate-uuid-v7)]
    (is (= 8 (count (set v7s))))
    (is (every? #(= 7 (.version ^java.util.UUID %)) v7s))
    (is (every? #(= 2 (.variant ^java.util.UUID %)) v7s))
    (is (= v7s (sort v7s)))))

(deftest uuid-version-extraction
  (is (= 5 (fns/sql-uuid-extract-version
            "11111111-1111-5111-8111-111111111111")))
  (is (= :__null__ (fns/sql-uuid-extract-version
                    "11111111-1111-1111-1111-111111111111")))
  (is (= 7 (fns/sql-uuid-extract-version (coerce/generate-uuid-v7)))))

(deftest uuid-timestamp-extraction
  (let [expected (java.util.Date/from
                  (java.time.Instant/parse "2022-02-22T19:22:22Z"))]
    (is (= expected
           (expr/parse-timestamp-string
            "Tuesday, February 22, 2022 2:22:22.00 PM GMT+05:00")))
    (is (= expected
           (fns/sql-uuid-extract-timestamp
            "C232AB00-9414-11EC-B3C8-9F6BDECED846")))
    (is (= expected
           (fns/sql-uuid-extract-timestamp
            "017F22E2-79B0-7CC3-98C4-DC0C0C07398F"))))
  (is (= :__null__
         (fns/sql-uuid-extract-timestamp (java.util.UUID/randomUUID)))))
