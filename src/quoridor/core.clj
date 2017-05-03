(ns quoridor.core
  (:gen-class)
  (:require [quoridor.board :as board]
            [clojure.string :as s]))

(def ^{:private true} black-white (cycle ["black" "white"]))

(defn- to-digit [n] (Character/digit n 10))

(defn- valid-move?
  [move]
  (let [col (first move)
        row (second move)]
    (and (contains? (board/char-range \a \h) col)
         (contains? (board/char-range \1 \8) row))))

(defn- right
  ([pos] (right pos 1))
  ([pos n]
   (let [right-pos (str (char (+ (int (first pos)) n)) (second pos))]
     (if (valid-move? right-pos)
       right-pos
       ""))))

(defn- left
  ([pos] (right pos -1))
  ([pos n] (right pos (- n))))

(defn- up
  ([pos] (up pos 1))
  ([pos n]
   (let [up-pos (str (first pos) (+ (to-digit (second pos)) n))]
     (if (valid-move? up-pos)
       up-pos
       ""))))

(defn- down
  ([pos] (up pos -1))
  ([pos n] (up pos (- n))))

(defn- above?
  [other-pos pos]
  (= (up pos) other-pos))

(defn- below?
  [other-pos pos]
  (= (down pos) other-pos))

(defn- right?
  [other-pos pos]
  (= (right pos) other-pos))

(defn- left?
  [other-pos pos]
  (= (left pos) other-pos))

(defn- jump
  [state]
  (let [current-player (state :current)
        other-player (if (= current-player "black") "white" "black")
        current-pos (state (keyword current-player))
        other-pos (state (keyword other-player))]
    (cond
      (above? other-pos current-pos) (up current-pos 2)
      (below? other-pos current-pos) (down current-pos 2)
      (right? other-pos current-pos) (right current-pos 2)
      (left? other-pos current-pos) (left current-pos 2))))

(defn- allowed-pawn-move?
  [state move]
  (let [current-position (state (keyword (state :current)))
        other-position (if (= (state :current) "black") (state :white) (state :black))
        col (first current-position)
        row (to-digit (second current-position))
        up-move (str col (+ row 1))
        down-move (str col (- row 1))
        left-move (str (-> col int (- 1) char) row)
        right-move (str (-> col int (+ 1) char) row)
        jump-move (jump state)]
    (and (contains? (set (filter valid-move? [up-move
                                              down-move
                                              left-move
                                              right-move
                                              jump-move]))
                    move)
         (not (contains? (set [current-position other-position]) move)))))

(defn- normalise-wall-move
  [move]
  (apply str (sort [(subs move 0 2) (subs move 2 4)])))

(defn- allowed-wall-move?
  [state move]
  (if (and (= (count move) 4)
           (not (contains? (state :walls) move)))
    true
    false))

(defn- black-won?
  [state]
  (= (Character/digit (second (state :black)) 10) 8))

(defn- white-won?
  [state]
  (= (Character/digit (second (state :white)) 10) 1))

(defn- game-over?
  [state]
  (or (black-won? state) (white-won? state)))

(defn- winner
  [state]
  (cond
    (black-won? state) "black"
    (white-won? state) "white"
    :else nil))

(defn- print-game-over
  [state]
  (println (str "Congrats " (winner state) ", you won!")))

(defn -main
  [& args]
  (loop [current (first black-white)
         next (rest black-white)
         state { :black "e1" :white "e8" :walls #{} :current current }]
    (println (board/render state))
    (if (game-over? state)
      (print-game-over state)
      (do (println (str current "'s move: "))
          (let [move (s/trim (read-line))] 
            (cond
              (= move "q") (println "Thanks for playing!")
              (allowed-pawn-move? state move) (let [next-player (first next)]
                                                (recur next-player
                                                       (rest next)
                                                       (-> state
                                                           (assoc (keyword current) move)
                                                           (assoc :current next-player))))
              :else (do (println (str "Sorry, " move " is not a valid move"))
                        (recur current next state))))))))
