(ns quoridor.core
  (:gen-class)
  (:require [quoridor.board :as board]
            [clojure.string :as s]
            [clojure.set]))

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

(defn- wall-between?
  [current-position move walls]
  (let [h-walls (board/horizontal-walls walls)
        v-walls (board/vertical-walls walls)
        up-move (up current-position)
        down-move (down current-position)
        right-move (right current-position)
        left-move (left current-position)
        jump-up-move (if (not-empty up-move) (up up-move))
        jump-right-move (if (not-empty right-move) (right right-move))
        jump-left-move (if (not-empty left-move) (left left-move))
        jump-down-move (if (not-empty down-move) (down down-move))]
    (cond
      (= move up-move) (board/in-walls? h-walls current-position)
      (= move down-move) (board/in-walls? h-walls move)
      (= move right-move) (board/in-walls? v-walls current-position)
      (= move left-move) (board/in-walls? v-walls move)
      (= move jump-up-move) (some #(board/in-walls? h-walls %) [current-position up-move])
      (= move jump-down-move) (some #(board/in-walls? h-walls %) [down-move jump-down-move])
      (= move jump-right-move) (some #(board/in-walls? v-walls %) [current-position right-move])
      (= move jump-left-move) (some #(board/in-walls? v-walls %) [current-position left-move]))))

(defn- neighbours
  [current-position state]
  (let [walls (state :walls)]
    (set (filter (fn [m] (and (valid-move? m)
                              (not (wall-between? current-position m walls))))
                 [(up current-position)
                  (down current-position)
                  (left current-position)
                  (right current-position)
                  (jump state)]))))

(defn- filter-seen
  [came-from current state]
  (filter #(not (contains? (set (keys came-from)) %))
          (neighbours current state)))

(defn- breadcrumbs
  [player state]
  (loop [frontier (list (state player))
         came-from { (first frontier) nil }]
    (if (empty? frontier)
      came-from
      (let [current (last frontier)
            remaining-frontier (butlast frontier)
            unseen-neighbours (filter-seen came-from current state)]
        (recur (concat unseen-neighbours remaining-frontier)
               (if (empty? unseen-neighbours)
                 came-from
                 (apply assoc came-from (flatten (map #(list % current) unseen-neighbours)))))))))

(defn- allowed-pawn-move?
  [state move]
  (let [current-position (state (keyword (state :current)))
        other-position (if (= (state :current) "black")
                         (state :white)
                         (state :black))]
    (and (contains? (neighbours current-position state) move)
         (not (contains? (set [current-position other-position]) move)))))

(defn- normalise-wall-move
  [move]
  (apply str (sort [(subs move 0 2) (subs move 2 4)])))

(defn- valid-wall-move?
  [move]
  (let [normalised-move (normalise-wall-move move)
          first-pos (subs normalised-move 0 2)
          second-pos (subs normalised-move 2 4)]
      (and (contains? (board/char-range \a \g) (first first-pos))
           (contains? (board/char-range \1 \7) (second first-pos))
           (or (= (right first-pos) second-pos)
               (= (up first-pos) second-pos)))))

(defn neither-player-boxed-in?
  [potential-state]
  (let [black-paths (breadcrumbs :black potential-state)
        white-paths (breadcrumbs :white potential-state)
        black-goals #{"a8" "b8" "c8" "d8" "e8" "f8" "g8" "h8"}
        white-goals #{"a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1"}]
    (and (not-empty (clojure.set/intersection (set (keys black-paths)) black-goals))
         (not-empty (clojure.set/intersection (set (keys white-paths)) white-goals)))))

(defn- allowed-wall-move?
  [state move]
  (and (= (count move) 4)
       (not (contains? (state :walls) move)) 
       (valid-wall-move? move)
       (let [walls (state :walls)
             potential-state (assoc-in state [:walls] (conj walls move))]
         (neither-player-boxed-in? potential-state))))

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

(defn- quit-move?
  [move]
  (= move "q"))

(defn -main
  [& args]
  (loop [current (first black-white)
         next (rest black-white)
         state { :black "e1" :white "e8" :walls #{} :current current }]
    (println (board/render state))
    (if (game-over? state)
      (print-game-over state)
      (do (println (str current "'s move: "))
          (let [move (s/trim (read-line))
                next-player (first next)]
            (cond
              (quit-move? move) (println "Thanks for playing!")
              (allowed-wall-move? state move) (recur next-player
                                                     (rest next)
                                                     (-> state
                                                         (assoc :walls (conj (state :walls) move))
                                                         (assoc :current next-player)))
              (allowed-pawn-move? state move) (recur next-player
                                                     (rest next)
                                                     (-> state
                                                         (assoc (keyword current) move)
                                                         (assoc :current next-player)))
              :else (do (println (str "Sorry, " move " is not a valid move"))
                        (recur current next state))))))))
