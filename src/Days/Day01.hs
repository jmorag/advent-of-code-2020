module Days.Day01 (runDay, Input, OutputA, OutputB, runA, runB) where

runDay :: Bool -> String -> IO ()
runDay = run inputParser partA partB

runA :: ByteString -> Either String OutputA
runA input = runPart input inputParser partA

runB :: ByteString -> Either String OutputB
runB input = runPart input inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy decimal endOfLine

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = comboProd 2020 2

------------ PART B ------------
partB :: Input -> OutputB
partB = comboProd 2020 3

comboProd :: (Eq a, Num a) => a -> Int -> [a] -> a
comboProd goal n input =
  (\case [x] -> x; _ -> error "Expect one working combination") do
    combo <- combinations n input
    guard (sum combo == goal)
    pure (product combo)
