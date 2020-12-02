module Test (module Test.Hspec, r, (===)) where

import Test.Hspec
import Text.RawString.QQ

(===) :: (Show a, Show b, Eq a, Eq b) => Either a b -> b -> Expectation
result === expected = result `shouldBe` Right expected
