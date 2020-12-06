module Days.Day06 (runDay, Input, OutputA, OutputB, runA, runB) where

import Data.Attoparsec.ByteString.Char8
import qualified Data.Set as S
import Relude.Extra.Foldable1

runDay :: Bool -> String -> IO ()
runDay = run inputParser partA partB

runA :: ByteString -> Either String OutputA
runA input = runPart input inputParser partA

runB :: ByteString -> Either String OutputB
runB input = runPart input inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy groupParser endOfLine

groupParser :: Parser [[Char]]
groupParser = some $ some (satisfy isAlpha_ascii) <* endOfLine

------------ TYPES ------------
type Input = [[String]]

type OutputA = Sum Int

type OutputB = Sum Int

------------ PART A ------------
partA :: Input -> OutputA
partA = foldMap (Sum . S.size . fromList . concat)

------------ PART B ------------
partB :: Input -> OutputB
partB = foldMap (Sum . S.size . mkSet)
  where
    mkSet :: [String] -> Set Char
    mkSet [] = S.empty
    mkSet (g : gs) = intersections (fmap fromList (g :| gs))

intersections :: (Ord a, Foldable1 t) => t (Set a) -> Set a
intersections = foldl1' S.intersection . toNonEmpty
