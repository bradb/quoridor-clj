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

(defn vertical-walls
  [walls]
  (filter (fn [w] (= (subs w 0 1) (subs w 2 3))) walls))

(defn horizontal-walls
  [walls]
  (clojure.set/difference walls (vertical-walls walls)))

(defn in-walls?
  [walls pos]
  (some (fn [w]
          (or (= (subs w 0 2) pos)
              (= (subs w 2 4) pos)))
        walls))

(defn- column
  [state coords]
  (let [v-walls (vertical-walls (state :walls))]
    (reduce above (if (in-walls? v-walls coords)
                    (repeat 3 " * ")
                    (repeat 3 blank-column)))))

(defn- horizontal-wall-at?
  [state pos]
  (some (fn [x] (or (= (subs x 0 2) pos)
                    (= (subs x 2 4) pos)))
        (horizontal-walls (state :walls))))

(defn char-range
  [start end]
  (set (map char (range (int start) (+ (int end) 1)))))

(defn- row-separator
  [state n]
  (beside blank-column
          (s/join blank-column (for [c (char-range \a \h)]
                                 (if (horizontal-wall-at? state (str c n))
                                   "*****"
                                   "     ")))))

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
  ([] (render { :black "e1" :white "e8" :current "black" :walls #{} }))
  ([state]
   (let [rows (reduce above (for [n (range 8 0 -1)]
                              (above (beside (row-label n) (row state n))
                                     (row-separator state (- n 1)))))]
     (above (column-headers) rows))))
