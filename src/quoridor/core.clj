(ns quoridor.core
  (:gen-class)
  (:require [quoridor.board :as board]
            [clojure.string :as s]))

(def black-white (cycle ["black" "white"]))

(defn -main
  [& args]
  (loop [current (first black-white)
         next (rest black-white)
         state { :black "e1" :white "e8" }]
    (println (board/render state))
    (println (str current "'s move: "))
    (let [move (s/trim (read-line))] 
      (if (= move "q")
        (println "Thanks for playing!")
        (recur (first next) (rest next) (assoc state (keyword current) move))))))
