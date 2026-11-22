(ns datahike.test.pg-mirror-protocol-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.pg.mirror :as mirror])
  (:import [java.nio ByteBuffer]))

(deftest ordinary-transaction-limit-precedes-change-decode
  (let [begin (doto (ByteBuffer/allocate 21)
                (.put (byte (int \B)))
                (.putLong 16)
                (.putLong 0)
                (.putInt 7)
                (.flip))
        change (doto (ByteBuffer/allocate 64)
                 (.put (byte (int \I)))
                 (.position 0))
        frames (atom [begin change])]
    ;; The oversized frame deliberately lacks a valid tuple. Reaching the
    ;; decoder would raise a tuple error before the transaction limit.
    (with-redefs-fn {#'mirror/read-packet (fn [_] {:data (ffirst (swap-vals! frames subvec 1))})
                     #'mirror/feedback! (fn [& _])}
      (fn []
        (let [error (try
                      (#'mirror/stream! nil "0/1" "0/1" {:max-transaction-bytes 16}
                                        {:tick! (constantly nil) :check-running! (constantly nil)
                                         :select-change! identity})
                      nil
                      (catch clojure.lang.ExceptionInfo error error))]
          (is (= "Transaction exceeds the configured byte limit" (ex-message error)))
          (is (= {:xid 7 :limit 16} (ex-data error))))))))
