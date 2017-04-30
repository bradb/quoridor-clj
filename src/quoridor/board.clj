(ns quoridor.board
  (:require [clojure.string :as s]))

(def horiz-side "+---+")
(def blank-column "   ")
(def eight (partial repeat 8))

(defn- beside
  [left right]
  (let [left-lines (s/split-lines left)
        right-lines (s/split-lines right)]
    (s/join "\n" (map #(str %1 %2) (concat left-lines (repeat "")) right-lines))))

(defn- above
  [line1 line2]
  (str line1 "\n" line2))

(defn- square
  [state coords]
  (let [is-black-pos (= (state :black) coords)
        is-white-pos (= (state :white) coords)]
    (s/join "\n" [horiz-side
                  (cond is-black-pos "| B |"
                        is-white-pos "| W |"
                        :else        "|   |")
                  horiz-side])))

(defn- column
  [state coords]
  (reduce above (repeat 3 blank-column)))

(defn- row-separator
  [state n]
  (s/join (cons "     " (flatten (repeat 7 [blank-column "     "])))))

(defn- row
  [state n]
  (reduce beside (flatten (for [col (map char (range (int \a) (+ (int \h) 1)))
                                :let [coords (str col n)]]
                            [(square state coords) (column state coords)]))))

(defn render
  ([] (render { :black "e1" :white "e8" }))
  ([state]
   (let [rows (reduce above (for [x (range 8 0 -1)]
                              (above (row state x) (row-separator state x))))]
     rows)))
