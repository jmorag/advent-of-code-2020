module Days.Day01Spec (spec) where

import Days.Day01
import Test

testInput :: ByteString
testInput =
  [r|1721
979
366
299
675
1456|]

spec :: Spec
spec = do
  describe "Part a" do
    it "should find the two numbers summing to 2020 and return their product" $
      runA testInput === 514579
  describe "Part b" do
    it "should find the three numbers summing to 2020 and return their product" $
      runB testInput === 241861950
