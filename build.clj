(ns build
  "Build + Clojars deploy for inet.data (tools.build + deps-deploy).

   Usage:
     clojure -T:build compile-java   ; javac src/java -> target/classes (needed before tests)
     clojure -T:build ragel          ; regenerate Java parsers from src/ragel
     clojure -T:build jar
     clojure -T:build deploy         ; needs CLOJARS_USERNAME / CLOJARS_PASSWORD"
  (:require [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'net.clojars.savya/inet.data)
(def version "1.0.0")
(def class-dir "target/classes")
(def basis (delay (b/create-basis {:project "deps.edn"})))
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn clean [_]
  (b/delete {:path "target"})
  (b/delete {:path "pom.xml"}))   ; drop stale lein-generated pom so :pom-data wins

(defn compile-java
  "Compile the ragel-generated Java parsers under src/java into target/classes.
   Run before `clojure -X:test` (the :test alias puts target/classes on the classpath)."
  [_]
  (b/javac {:src-dirs ["src/java"]
            :class-dir class-dir
            :basis @basis
            :javac-opts ["-target" "8" "-source" "8" "-Xlint:-options"]}))

(defn ragel
  "Regenerate the Java parsers from their Ragel grammars."
  [_]
  (doseq [[output input] [["src/java/inet/data/ip/IPParser.java"
                           "src/ragel/inet/data/ip/IPParser.java.rl"]
                          ["src/java/inet/data/dns/DNSDomainParser.java"
                           "src/ragel/inet/data/dns/DNSDomainParser.java.rl"]]]
    (b/process {:command-args ["ragel" "-J" "-o" output input]})))

(defn jar [_]
  (clean nil)
  (compile-java nil)
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis @basis
                :src-dirs ["src/clojure"]
                :scm {:url "https://github.com/jsavyasachi/inet.data"
                      :connection "scm:git:https://github.com/jsavyasachi/inet.data.git"
                      :developerConnection "scm:git:ssh://git@github.com/jsavyasachi/inet.data.git"
                      :tag (str "v" version)}
                :pom-data [[:description "Represent and manipulate various Internet entities as data."]
                           [:url "https://github.com/jsavyasachi/inet.data"]
                           [:licenses
                            [:license
                             [:name "Eclipse Public License 1.0"]
                             [:url "https://www.eclipse.org/legal/epl-v10.html"]
                             [:distribution "repo"]]]]})
  (b/copy-dir {:src-dirs ["src/clojure" "resources"] :target-dir class-dir})
  (b/jar {:class-dir class-dir :jar-file jar-file})
  (println "Wrote" jar-file))

(defn deploy [_]
  (jar nil)
  (dd/deploy {:installer :remote
              :artifact jar-file
              :pom-file (b/pom-path {:lib lib :class-dir class-dir})}))
