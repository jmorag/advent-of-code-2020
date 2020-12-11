module Days.Day10Spec (spec) where

import Days.Day10
import Test

testInput1, testInput2 :: ByteString
testInput1 =
  [r|16
10
15
5
1
11
7
19
6
12
4
|]
testInput2 =
  [r|28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3
|]

a1, a2 :: OutputA -> Expectation
a1 = (runA testInput1 ===)
a2 = (runA testInput2 ===)

b1, b2 :: OutputB -> Expectation
b1 = (runB testInput1 ===)
b2 = (runB testInput2 ===)

spec :: Spec
spec = do
  describe "Part a" do
    it "should find 7 1-jolt diffs and 5 3-jolt diffs" $ a1 35
    it "should find 22 1-jolt diffs and 10 3-jolt diffs" $ a2 220
  describe "Part b" do
    it "should find 8 configurations" $ b1 8
    it "should find 19208 configurations" $ b2 19208
