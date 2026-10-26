(ns datahike.test.pg-assignment-marker-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.pg.sql.params :as params]))

(deftest assignment-markers-share-the-existing-effect-identity-map
  (let [calls (atom 0)
        assignment {:fn :assignment
                    :assignment-fn
                    (fn [resolve-value]
                      (resolve-value {:fn :nextval :seq-name "s"}))}]
    (is (params/call-marker? assignment))
    (is (identical? assignment (params/substitute-params assignment (fn [_] 99))))
    (is (= [1 1] (params/resolve-nextvals! [assignment assignment]
                                           (fn [_] (swap! calls inc)))))
    (is (= 1 @calls))
    (is (= 2 (params/resolve-nextvals! assignment (fn [_] (swap! calls inc)))))
    (is (= 2 @calls) "the identity map lasts one execution, not across calls")))

(deftest assignment-errors-do-not-resolve-unreachable-effects
  (let [calls (atom [])
        nextval (fn [name] (swap! calls conj name) (count @calls))
        assignment {:fn :assignment
                    :assignment-fn
                    (fn [resolve-value]
                      (resolve-value {:fn :nextval :seq-name "upper"})
                      (resolve-value {:fn :nextval :seq-name "lower"})
                      (throw (ex-info "null subscript" {:sqlstate "22004"}))
                      (resolve-value {:fn :nextval :seq-name "rhs"}))}]
    (is (= "22004" (try (params/resolve-nextvals! assignment nextval)
                        nil
                        (catch clojure.lang.ExceptionInfo e (:sqlstate (ex-data e))))))
    (is (= ["upper" "lower"] @calls))))
