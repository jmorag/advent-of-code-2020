module Days.Day04 (runDay, Input, OutputA, OutputB, runA, runB) where

import Control.Error

import qualified Data.ByteString.Char8 as B
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
    passport :: Parser Passport
    passport =
      fromList <$> some do
        key <- take 3 <* char ':'
        val <- takeTill isSpace <* space
        pure (key, val)

------------ TYPES ------------
requiredKeys :: [ByteString]
requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

type Passport = Map ByteString ByteString
type Input = NonEmpty Passport

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = length . filter validPassport . toList

validPassport :: Passport -> Bool
validPassport passport = all (`M.member` passport) requiredKeys

------------ PART B ------------
partB :: Input -> OutputB
partB = length . filter validPassportB . toList

validPassportB :: Passport -> Bool
validPassportB passport = and [byr, iyr, eyr, hgt, hcl, ecl, pid]
  where
    p key parser =
      case note "missing key" (passport M.!? key)
        >>= parseOnly (parser *> endOfInput) of
        Left _ -> False
        Right _ -> True
    range :: Int -> Int -> Int -> Bool
    range low high n = n >= low && n <= high
    year low high = guard . range low high =<< decimal
    byr = p "byr" $ year 1920 2002
    iyr = p "iyr" $ year 2010 2020
    eyr = p "eyr" $ year 2020 2030
    hgt = p "hgt" do
      n <- decimal
      ("cm" *> guard (range 150 193 n)) <|> ("in" *> guard (range 59 76 n))
    hcl = p "hcl" do
      void $ char '#'
      color <- take 6
      guard (B.all (inClass "0-9a-f") color)
    ecl = p "ecl" do
      void $ "amb" <|> "blu" <|> "brn" <|> "gry" <|> "grn" <|> "hzl" <|> "oth"
    pid = p "pid" do
      n <- take 9
      guard (B.all (inClass "0-9") n)
