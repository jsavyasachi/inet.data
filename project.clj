(defproject net.clojars.savya/inet.data "0.7.4"
  :plugins [[lein-tools-deps "0.4.5"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]
                           :aliases [:test]})
