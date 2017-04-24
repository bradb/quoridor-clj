(ns quoridor-clj.core
  (:require [clojure.string :as s])
  (:gen-class))

(def black-white (cycle ["black" "white"]))

(defn -main
  [& args]
  (loop [current (first black-white)
         next (rest black-white)]
    (println (str current "'s move: "))
    (if (= (s/trim (read-line)) "q")
      (println "Thanks for playing!")
      (recur (first next) (rest next)))))

