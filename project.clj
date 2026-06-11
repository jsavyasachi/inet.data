(defproject net.clojars.savya/inet.data "0.7.1"
  :description "Represent and manipulate various Internet entities as data."
  :url "https://github.com/jsavyasachi/inet.data"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :global-vars {*warn-on-reflection* true}
  :dependencies [[org.clojure/clojure "1.12.5"]
                 [net.clojars.savya/hier-set "1.2.0"]]
  :source-paths ["src/clojure"]
  ;; The IP and DNS parsers under inet/data/{ip,dns} are generated from the ragel
  ;; grammars in src/ragel/*.java.rl and committed directly, so building no
  ;; longer requires the ragel compiler. Regenerate with `ragel -J` only if you
  ;; edit a .rl grammar.
  :java-source-paths ["src/java"]
  :javac-options ["-target" "8" "-source" "8" "-Xlint:-options"]
  :aliases {"all" ["with-profile" ~(str "+clojure-1-10:"
                                        "+clojure-1-11:"
                                        "+clojure-1-12")]}
  :profiles {:provided {:dependencies
                        [[byteable "0.2.0"]
                         [com.damballa/abracad "0.4.12"]]}
             :clojure-1-10 {:dependencies [[org.clojure/clojure "1.10.3"]]}
             :clojure-1-11 {:dependencies [[org.clojure/clojure "1.11.4"]]}
             :clojure-1-12 {:dependencies [[org.clojure/clojure "1.12.5"]]}})
