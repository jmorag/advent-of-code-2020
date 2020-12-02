module Days.Day05Spec (spec) where

import Days.Day05
import Test

testInput :: ByteString
testInput = [r||]

a :: OutputA -> Expectation
a = (runA testInput ===)

b :: OutputB -> Expectation
b = (runB testInput ===)

spec :: Spec
spec = do
  describe "Part a" do
    xit "" $ a undefined
  describe "Part b" do
    xit "" $ b undefined
