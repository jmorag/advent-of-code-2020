module Days.Day12Spec (spec) where

import Days.Day12
import Test

testInput :: ByteString
testInput =
  [r|F10
N3
F7
R90
F11
|]

a :: OutputA -> Expectation
a = (runA testInput ===)

b :: OutputB -> Expectation
b = (runB testInput ===)

spec :: Spec
spec = do
  describe "Part a" do
    it "should find the manhattan distance from the origin after moving" $ a 25
  describe "Part b" do
    it "should find the manhattan distance with new instructions" $ b 286
