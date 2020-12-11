module Days.Day11 (runDay, Input, OutputA, OutputB, runA, runB) where

import Data.Array
import GHC.Show

runDay :: Bool -> String -> IO ()
runDay = run inputParser partA partB

runA :: ByteString -> Either String OutputA
runA input = runPart input inputParser partA

runB :: ByteString -> Either String OutputB
runB input = runPart input inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = gridParser [('L', Empty), ('#', Occupied), ('.', Floor)]

------------ TYPES ------------
data Cell = Empty | Occupied | Floor
instance Show Cell where
  show = \case
    Empty -> "L"
    Occupied -> "#"
    Floor -> "."

type Input = Array (Int, Int) Cell

type OutputA = Int

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA = error "Not implemented yet!"

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
