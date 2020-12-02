module Days.Day17 (runDay, Input, OutputA, OutputB, runA, runB) where

import Data.Attoparsec.Text

runDay :: Bool -> String -> IO ()
runDay = run inputParser partA partB

runA :: Text -> Either String OutputA
runA input = runPart input inputParser partA

runB :: Text -> Either String OutputB
runB input = runPart input inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = error "Not implemented yet!"

------------ TYPES ------------
type Input = Void

type OutputA = Void

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA = error "Not implemented yet!"

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
