module Days.Day11Spec (spec) where

import Days.Day11
import Test

testInput :: ByteString
testInput =
  [r|L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL
|]

visibilityTest1, visibilityTest2, visibilityTest3 :: ByteString
visibilityTest1 =
  [r|.......#.
...#.....
.#.......
.........
..#L....#
....#....
.........
#........
...#.....
|]
visibilityTest2 =
  [r|.##.##.
#.#.#.#
##...##
...L...
##...##
#.#.#.#
.##.##.
|]
visibilityTest3 =
  [r|.............
.L.L.#.#.#.#.
.............
|]

a :: OutputA -> Expectation
a = (runA testInput ===)

b :: OutputB -> Expectation
b = (runB testInput ===)

p :: ByteString -> Either String Input
p = parseOnly inputParser

spec :: Spec
spec = do
  describe "Part a" do
    it "should count seats after converging" $ a 37
  describe "Part b" do
    it "should see 8 occupied seats" $ fmap (`visible` (4, 3)) (p visibilityTest1) === 8
    it "should see 0 occupied seats" $ fmap (`visible` (3, 3)) (p visibilityTest2) === 0
    it "should see 0 occupied seats" $ fmap (`visible` (1, 1)) (p visibilityTest3) === 0
    it "should count seats after converging with new rules" $ b 26
