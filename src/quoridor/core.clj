(ns quoridor.core
  (:gen-class)
  (:require [quoridor.board :as board]
            [clojure.string :as s]))

(def black-white (cycle ["black" "white"]))

(defn -main
  [& args]
  (loop [current (first black-white)
         next (rest black-white)]
    (println (board/render))
    (println (str current "'s move: "))
    (if (= (s/trim (read-line)) "q")
      (println "Thanks for playing!")
      (recur (first next) (rest next)))))
