(require '[cljs.build.api :as b])

(b/watch "src"
  {:main 'quoridor.core
   :output-to "out/quoridor.js"
   :output-dir "out"})
