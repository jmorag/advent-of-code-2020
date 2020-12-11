module Days.Day09Spec (spec) where

import Days.Day09
import Test

testInput :: ByteString
testInput =
  [r|35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576
|]

a :: OutputA -> Expectation
a = (runATest testInput ===)

b :: OutputB -> Expectation
b = (runBTest testInput ===)

spec :: Spec
spec = do
  describe "Part a" do
    it "should find the first invalid input" $ a 127
  describe "Part b" do
    it "should find the sum of the smallest and largest numbers in the contiguous run of numbers summing to the goal" $ b 62
