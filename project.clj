(defproject quoridor "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.562"]
                 [cljsjs/svgjs "2.2.5-0"]]
  :jvm-opts ^:replace ["-Xmx1g" "-server"]
  :main ^:skip-aot quoridor.core
  :plugins [[lein-npm "0.6.2"]]
  :npm {:dependencies [[source-map-support "0.4.0"]]}
  :source-paths ["src" "target/classes"]
  :profiles {:uberjar {:aot :all}}
  :clean-targets ["out" "release"]
  :target-path "target/%s")
