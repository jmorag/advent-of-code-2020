module Days.Day09 (runDay, Input, OutputA, OutputB, runATest, runBTest) where

import Relude.Extra (maximum1, minimum1)

runDay :: Bool -> String -> IO ()
runDay = run inputParser partA partB

runATest :: ByteString -> Either String OutputA
runATest input = runPart input inputParser (findInvalid 5)

runBTest :: ByteString -> Either String OutputB
runBTest input = runPart input inputParser partBTest

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy decimal endOfLine

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = findInvalid 25

findInvalid :: Int -> [Int] -> Int
findInvalid windowSize input =
  let (window, rest) = splitAt windowSize input
   in go window rest
  where
    go window@(_ : ws) (x : xs) = if invalid window x then x else go (ws <> [x]) xs
    go _ _ = error "Couldn't find invalid input"

invalid :: [Int] -> Int -> Bool
invalid window next =
  all (\case [x, y] -> x + y /= next; _ -> error "impossible") (combinations 2 window)

------------ PART B ------------
partB, partBTest :: Input -> OutputB
partB = runB 25
partBTest = runB 5

runB :: Int -> Input -> OutputB
runB windowSize input =
  let goal = findInvalid windowSize input
      contiguous = findContiguous goal input
   in case contiguous of
        Just (x : xs) -> minimum1 (x :| xs) + maximum1 (x :| xs)
        _ -> error "Couldn't find contiguous sum"

findContiguous :: Int -> [Int] -> Maybe [Int]
findContiguous goal = asum . map (go 0 []) . tails
  where
    go acc seen rest
      | acc == goal = Just seen
      | acc > goal = Nothing
      | otherwise = case rest of
        [] -> Nothing
        (x : xs) -> go (acc + x) (x : seen) xs
