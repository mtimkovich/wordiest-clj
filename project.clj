(defproject wordiest "1.0"
  :description "Wordiest solver"
  :url "http://github.com/mtimkovich/wordiest-clj"
  :license {:name "MIT License"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :main ^:skip-aot wordiest.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
