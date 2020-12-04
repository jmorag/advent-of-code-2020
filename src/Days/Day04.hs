module Days.Day04 (runDay, Input, OutputA, OutputB, runA, runB) where

import Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.Map as M

runDay :: Bool -> String -> IO ()
runDay = run inputParser partA partB

runA :: ByteString -> Either String OutputA
runA input = runPart input inputParser partA

runB :: ByteString -> Either String OutputB
runB input = runPart input inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy1 passport endOfLine
  where
    passport =
      M.fromList <$> some do
        key <- Atto.take 3
        char ':'
        val <- Atto.takeTill isSpace
        space
        pure (key, val)

------------ TYPES ------------
requiredKeys :: [ByteString]
requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

type Passport = Map ByteString ByteString
type Input = [Passport]

type OutputA = Int

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA = length . filter validPassport

validPassport :: Passport -> Bool
validPassport passport = all (`M.member` passport) requiredKeys

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
