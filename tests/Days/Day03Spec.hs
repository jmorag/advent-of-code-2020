module Days.Day03Spec (spec) where

import Days.Day03
import Test

testInput :: ByteString
testInput =
  [r|..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#|]

a :: OutputA -> Expectation
a = (runA testInput ===)

b :: OutputB -> Expectation
b = (runB testInput ===)

spec :: Spec
spec = do
  describe "Part a" do
    it "should encounter 7 trees when traversing the slope by right 3, down 1" $ a 7
  describe "Part b" do
    it
      ( "should encounter 2,7,3,4, and 2 trees when traversing slopes"
          <> [r|
- Right 1, down 1.
- Right 3, down 1. (This is the slope you already checked.)
- Right 5, down 1.
- Right 7, down 1.
- Right 1, down 2.|]
      )
      $ b 336
