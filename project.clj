(defproject geoutils "0.1.0"
  :description "A set of utilities and support files to help with geographic and navigational problems."
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [incanter "1.9.3"]]
  :plugins [[lein-codox "0.10.8"]]
  :main ^:skip-aot geoutils.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
