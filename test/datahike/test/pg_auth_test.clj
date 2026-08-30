(ns datahike.test.pg-auth-test
  "PostgreSQL password authentication and TLS interoperability tests."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.pg.server :as pg])
  (:import [datahike.pg PgWireServer]
           [java.io File]
           [java.nio.file Files]
           [java.sql DriverManager SQLException]))

(def ^:dynamic *conn* nil)

(defn- fixture [f]
  (Class/forName "org.postgresql.Driver")
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :max-string-length 0
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (binding [*conn* conn] (f))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

(use-fixtures :each fixture)

(defn- open-jdbc [port query]
  (DriverManager/getConnection
   (str "jdbc:postgresql://localhost:" port "/datahike?" query
        "&preferQueryMode=simple")))

(defn- start-test-server [opts]
  (pg/start-server {"datahike" *conn*} (merge {:port 0} opts)))

(defn- run-process! [args]
  (let [process (-> (ProcessBuilder. ^java.util.List args)
                    (.redirectErrorStream true)
                    (.start))
        output (slurp (.getInputStream process))
        exit (.waitFor process)]
    (when-not (zero? exit)
      (throw (ex-info "test process failed"
                      {:args args :exit exit :output output})))
    output))

(defn- test-keystore []
  (let [dir (Files/createTempDirectory
             "pg-datahike-tls-"
             (make-array java.nio.file.attribute.FileAttribute 0))
        keystore (.resolve dir "server.p12")
        certificate (.resolve dir "server.crt")
        keytool (str (System/getProperty "java.home") File/separator
                     "bin" File/separator "keytool")]
    (run-process!
     [keytool "-genkeypair" "-alias" "pg-datahike-test"
      "-keyalg" "RSA" "-keysize" "2048" "-dname" "CN=localhost"
      "-ext" "SAN=dns:localhost,ip:127.0.0.1" "-validity" "2"
      "-storetype" "PKCS12" "-keystore" (str keystore)
      "-storepass" "changeit" "-keypass" "changeit" "-noprompt"])
    (run-process!
     [keytool "-exportcert" "-rfc" "-alias" "pg-datahike-test"
      "-keystore" (str keystore) "-storepass" "changeit"
      "-file" (str certificate)])
    {:dir dir :keystore keystore :certificate certificate}))

(defn- delete-test-keystore! [{:keys [dir keystore certificate]}]
  (Files/deleteIfExists certificate)
  (Files/deleteIfExists keystore)
  (Files/deleteIfExists dir))

(deftest password-authentication-accepts-and-rejects-like-postgres
  (let [server (start-test-server {:users {"alice" "correct horse"}})
        port (.getPort ^PgWireServer (:server server))]
    (try
      (with-open [connection (open-jdbc
                              port
                              "user=alice&password=correct%20horse&sslmode=disable")
                  statement (.createStatement connection)
                  result (.executeQuery statement "SELECT 1")]
        (is (.next result))
        (is (= 1 (.getInt result 1))))
      (let [error (try
                    (open-jdbc port "user=alice&password=wrong&sslmode=disable")
                    nil
                    (catch SQLException e e))]
        (is (= "28P01" (.getSQLState ^SQLException error)))
        (is (= "FATAL: password authentication failed for user \"alice\""
               (.getMessage ^SQLException error))))
      (finally
        (pg/stop-server server)))))

(deftest authenticator-receives-context-and-password-is-cleared
  (let [seen (atom nil)
        server (start-test-server
                {:authenticator
                 (fn [connection password]
                   (reset! seen {:connection connection :password password})
                   (= "correct" (String. ^chars password)))})
        port (.getPort ^PgWireServer (:server server))]
    (try
      (with-open [connection
                  (open-jdbc port
                             "user=alice&password=correct&sslmode=disable")]
        (is (not (.isClosed connection))))
      (is (= {:user "alice" :database "datahike" :tls? false}
             (select-keys (:connection @seen) [:user :database :tls?])))
      (is (every? zero? (map int (:password @seen)))
          "wire layer clears the callback password buffer")
      (finally
        (pg/stop-server server)))))

(deftest tls-upgrade-supports-require-and-verify-full
  (let [files (test-keystore)
        server (start-test-server
                {:users {"alice" "correct"}
                 :tls {:keystore (str (:keystore files))
                       :keystore-password "changeit"}
                 :require-tls? true})
        port (.getPort ^PgWireServer (:server server))]
    (try
      (let [plaintext-error
            (try
              (open-jdbc port "user=alice&password=correct&sslmode=disable")
              nil
              (catch SQLException e e))]
        (is (= "28000" (.getSQLState ^SQLException plaintext-error))))
      (with-open [connection (open-jdbc
                              port
                              "user=alice&password=correct&sslmode=require")]
        (is (not (.isClosed connection))))
      (with-open [connection
                  (open-jdbc
                   port
                   (str "user=alice&password=correct&sslmode=verify-full"
                        "&sslrootcert=" (:certificate files)))]
        (is (not (.isClosed connection))))
      (let [wrong-password
            (try
              (open-jdbc port "user=alice&password=wrong&sslmode=require")
              nil
              (catch SQLException e e))]
        (is (= "28P01" (.getSQLState ^SQLException wrong-password))))
      (finally
        (pg/stop-server server)
        (delete-test-keystore! files)))))

(deftest public-bind-requires-authentication-and-tls
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"require password authentication and TLS"
       (start-test-server {:host "0.0.0.0"}))))
