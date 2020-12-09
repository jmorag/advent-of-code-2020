module Days.Day08Spec (spec) where

import Days.Day08
import Test

testInput :: ByteString
testInput =
  [r|nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
|]

a :: OutputA -> Expectation
a = (runA testInput ===)

b :: OutputB -> Expectation
b = (runB testInput ===)

spec :: Spec
spec = do
  describe "Part a" do
    it "last acc value before looping should be 5" $ a 5
  describe "Part b" do
    it "terminates with 8 in acc" $ b 8
