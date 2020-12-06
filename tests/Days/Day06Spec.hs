module Days.Day06Spec (spec) where

import Days.Day06
import Test

testInput :: ByteString
testInput =
  [r|abc

a
b
c

ab
ac

a
a
a
a

b
|]

a :: OutputA -> Expectation
a = (runA testInput ===)

b :: OutputB -> Expectation
b = (runB testInput ===)

spec :: Spec
spec = do
  describe "Part a" do
    it "should return 11 total questions to which someone answered yes" $ a 11
  describe "Part b" do
    it "should return 6 total questions to which everyone answered yes" $ b 6
