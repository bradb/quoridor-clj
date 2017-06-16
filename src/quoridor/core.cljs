(ns quoridor.core
  (:require [clojure.browser.repl :as repl]
            [cljsjs.svgjs :as svg]))

;; (defonce conn
;;   (repl/connect "http://localhost:9000/repl"))

(let [draw (js/SVG "board")]
  (dotimes [r 8]
    (dotimes [n 8] (.move (.fill (.rect draw 50 50) "black") (* n 60) (* r 60)))))
