(ns quoridor.board
  (:require [clojure.string :as s]))

(def ^{:private true} horiz-side "+---+")
(def ^{:private true} blank-column "   ")
(def ^{:private true} eight (partial repeat 8))

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

(defn- char-range
  [start end]
  (map char (range (int start) (+ (int end) 1))))

(defn- row
  [state n]
  (reduce beside (flatten (for [col (char-range \a \h)
                                :let [coords (str col n)]]
                            [(square state coords) (column state coords)]))))

(defn- column-headers
  []
  (s/join (cons blank-column (map #(str "  " % "  " blank-column)
                                    (char-range \A \H)))))

(defn- row-label
  [n]
  (let [padding (int (/ (count blank-column) 2))
        padding-str (s/join (repeat padding " "))]
    (above (above blank-column (s/join [padding-str n padding-str])) blank-column)))

(defn render
  ([] (render { :black "e1" :white "e8" }))
  ([state]
   (let [rows (reduce above (for [n (range 8 0 -1)]
                              (above (beside (row-label n) (row state n)) (row-separator state n))))]
     (above (column-headers) rows))))
