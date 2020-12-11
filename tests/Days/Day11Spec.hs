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

a :: OutputA -> Expectation
a = (runA testInput ===)

b :: OutputB -> Expectation
b = (runB testInput ===)

spec :: Spec
spec = do
  describe "Part a" do
    it "should count seats after converging" $ a 37
  describe "Part b" do
    xit "" $ b undefined
