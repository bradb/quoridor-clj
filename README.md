# quoridor-clj

An implementation of [Quoridor](https://en.wikipedia.org/wiki/Quoridor) in Clojure.

Move your pawn to the other side of the board before your opponent does and you win!

Still very much a work in progress.

## Usage

Start it up with:

`$ lein run`

### Commands

Enter two characters to move your pawn, e.g. `e2`, `c7`, `a4`.

Enter four characters to place a wall _above_ those squares, e.g. `f2g2` will render a wall between `f2g2` and `f3g3`.

`q` quits.

## Todo

* cljs frontend
* 1-player mode (computer makes random moves)
* 2-player mode (share link to game)
