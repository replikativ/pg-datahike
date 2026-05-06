(ns build
  "Build, package, and deploy tasks for pg-datahike.

   Public tasks:
     clojure -T:build compile-java   ;; javac → target/classes
     clojure -T:build clean
     clojure -T:build jar
     clojure -T:build uber           ;; standalone runnable jar
     clojure -T:build install        ;; ~/.m2
     clojure -T:build deploy         ;; Clojars (needs CLOJARS_USERNAME/_PASSWORD)"
  (:require [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'org.replikativ/pg-datahike)
;; Version derived from git — `0.1.<commit-count>`. Matches konserve /
;; datahike's convention: major.minor pins the API stability window,
;; patch ticks with every merged commit so Clojars / Maven get a
;; monotonic stream without manual bumps. `git-count-revs` reads the
;; working tree's HEAD, so CI must fetch full history (no shallow
;; clones) for the version to be stable across identical HEADs.
(def current-commit (b/git-process {:git-args "rev-parse HEAD"}))
(def version (format "0.1.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))
(def uber-file (format "target/%s-%s-standalone.jar" (name lib) version))

(defn- basis [] (b/create-basis {:project "deps.edn"}))

(defn clean [_]
  (b/delete {:path "target"}))

(defn compile-java [_]
  (b/javac {:src-dirs ["java"]
            :class-dir class-dir
            :basis (basis)
            :javac-opts ["-source" "17" "-target" "17"]}))

(defn- write-version-resource!
  "Embed the version string at `pg-datahike.version` on the classpath
   so the `--version` CLI flag (and any future telemetry) can read it
   without needing to recompute via b/git-count-revs at runtime."
  []
  (let [f (java.io.File. class-dir "pg-datahike.version")]
    (.mkdirs (.getParentFile f))
    (spit f version)))

(defn jar [_]
  (clean nil)
  (compile-java nil)
  (b/copy-dir {:src-dirs ["src"]
               :target-dir class-dir})
  (write-version-resource!)
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis (basis)
                :src-dirs ["src"]
                :scm {:url "https://github.com/replikativ/pg-datahike"}
                :pom-data [[:licenses
                            [:license
                             [:name "Eclipse Public License 2.0"]
                             [:url "https://www.eclipse.org/legal/epl-2.0/"]
                             [:distribution "repo"]]]]})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))

(defn uber
  "Build a standalone executable jar that bundles every dependency.

       java -jar target/pg-datahike-VERSION-standalone.jar [OPTIONS]

   AOT-compiles `datahike.pg.main` (the CLI entrypoint) and packages
   it with all transitive deps. End-users don't need a Clojure
   installation — only a JDK 17+."
  [_]
  (clean nil)
  (compile-java nil)
  (b/copy-dir {:src-dirs ["src"]
               :target-dir class-dir})
  (write-version-resource!)
  (b/compile-clj {:basis (basis)
                  :class-dir class-dir
                  :src-dirs ["src"]
                  :ns-compile '[datahike.pg.main]})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis (basis)
           :main 'datahike.pg.main})
  (println (str "Uberjar: " uber-file " (version " version ")")))

(defn install [_]
  (jar nil)
  (b/install {:basis (basis)
              :lib lib
              :version version
              :jar-file jar-file
              :class-dir class-dir}))

(defn deploy [_]
  (jar nil)
  (dd/deploy {:installer :remote
              :artifact jar-file
              :pom-file (b/pom-path {:lib lib :class-dir class-dir})}))
