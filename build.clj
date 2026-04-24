(ns build
  "Build, package, and deploy tasks for pg-datahike.

   Public tasks:
     clojure -T:build compile-java   ;; javac → target/classes
     clojure -T:build clean
     clojure -T:build jar
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

(defn- basis [] (b/create-basis {:project "deps.edn"}))

(defn clean [_]
  (b/delete {:path "target"}))

(defn compile-java [_]
  (b/javac {:src-dirs ["java"]
            :class-dir class-dir
            :basis (basis)
            :javac-opts ["-source" "17" "-target" "17"]}))

(defn jar [_]
  (clean nil)
  (compile-java nil)
  (b/copy-dir {:src-dirs ["src"]
               :target-dir class-dir})
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
