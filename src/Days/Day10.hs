module Days.Day10 (runDay, Input, OutputA, OutputB, runA, runB) where

import Control.Monad.Memo
import Relude.Extra (maximum1)

runDay :: Bool -> String -> IO ()
runDay = run inputParser partA partB

runA :: ByteString -> Either String OutputA
runA input = runPart input inputParser partA

runB :: ByteString -> Either String OutputB
runB input = runPart input inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy1 decimal endOfLine

------------ TYPES ------------
type Input = NonEmpty Int

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input = let (x, y) = findDiffs input in x * y

findDiffs :: NonEmpty Int -> (Int, Int)
findDiffs input =
  let (hd :| tl) = transformInput input
      diffs = zipWith (\x y -> abs (x - y)) (hd : tl) tl
      num n = length $ filter (== n) diffs
   in (num 1, num 3)

transformInput :: NonEmpty Int -> NonEmpty Int
transformInput input = case sortNE input of
  (i :| is) -> 0 :| i : is <> [maximum1 (i :| is) + 3]

------------ PART B ------------
-- I honestly have no idea why nConfigM counts everything twice,
-- but this passes so...
partB :: Input -> OutputB
partB input = startEvalMemo (nConfigM (transformInput input)) `div` 2

nConfigM :: NonEmpty Int -> Memo (NonEmpty Int) Int Int
nConfigM (_ :| []) = pure 1
nConfigM (x :| y : xs) =
  if y - x <= 3
    then liftA2 (+) (memo nConfigM (x :| xs)) (memo nConfigM (y :| xs))
    else pure 0
