module Days.Day03 (runDay, Input, OutputA, OutputB, runA, runB) where

import Data.Array

runDay :: Bool -> String -> IO ()
runDay = run inputParser partA partB

runA :: ByteString -> Either String OutputA
runA input = runPart input inputParser partA

runB :: ByteString -> Either String OutputB
runB input = runPart input inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = gridParser [('.', Open), ('#', Tree)]

------------ TYPES ------------
data Square = Open | Tree
  deriving (Show)

data Slope = Slope {right :: Int, down :: Int}

-- 0 based row-major matrix indexing
type Input = Array (Int, Int) Square

type OutputA = Int

type OutputB = Product Int

------------ PART A ------------
partA :: Input -> OutputA
partA input = nTrees input (Slope 3 1)

nTrees :: Array (Int, Int) Square -> Slope -> Int
nTrees grid Slope {..} = go 0 (0, 0)
  where
    (_, (rows, cols)) = bounds grid
    go !acc (i, j) = if i > rows then acc else go acc' (i + down, j + right)
      where
        acc' = acc + case grid ! (i, j `mod` (cols + 1)) of Tree -> 1; Open -> 0

------------ PART B ------------
partB :: Input -> OutputB
partB grid =
  foldMap
    (Product . nTrees grid)
    [Slope 1 1, Slope 3 1, Slope 5 1, Slope 7 1, Slope 1 2]
