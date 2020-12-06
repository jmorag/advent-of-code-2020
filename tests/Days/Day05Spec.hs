module Days.Day05Spec (spec) where

import Days.Day05
import Test

testInput :: ByteString
testInput =
  [r|BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL|]

a :: OutputA -> Expectation
a = (runA testInput ===)

spec :: Spec
spec = do
  describe "Part a" do
    it "should be seat 70, 7" $
      runPart "BFFFBBFRRR" boardingPassParser findSeat === Seat 70 7
    it "should be seat 14, 7" $
      runPart "FFFBBBFRRR" boardingPassParser findSeat === Seat 14 7
    it "should be seat 102, 4" $
      runPart "BBFFBBFRLL" boardingPassParser findSeat === Seat 102 4
    it "should find the maximum seat id of 820" $ a 820
  describe "Part b" do
    pass
