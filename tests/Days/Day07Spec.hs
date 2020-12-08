module Days.Day07Spec (spec) where

import Days.Day07
import Test

testInput :: ByteString
testInput =
  [r|light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
|]

a :: OutputA -> Expectation
a = (runA testInput ===)

b :: OutputB -> Expectation
b = (runB testInput ===)

testInput2 :: ByteString
testInput2 =
  [r|shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.
|]

spec :: Spec
spec = do
  describe "Part a" do
    it "should return 4 bags which can eventually contain a shiny gold bag" $
      a 4
  describe "Part b" do
    it "a shiny gold bag needs to contain 32 other bags" $ b 32
    it "second example" $ runB testInput2 === 126
