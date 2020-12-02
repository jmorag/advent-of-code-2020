module Days.Day02 (runDay, Input, OutputA, OutputB, runA, runB) where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B

runDay :: Bool -> String -> IO ()
runDay = run inputParser partA partB

runA :: ByteString -> Either String OutputA
runA input = runPart input inputParser partA

runB :: ByteString -> Either String OutputB
runB input = runPart input inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy pwd endOfLine
  where
    pwd =
      Password
        <$> (decimal <* char '-')
        <*> (decimal <* space)
        <*> (anyChar <* string ": ")
        <*> takeWhile1 (inClass "a-z")

------------ TYPES ------------
data Password = Password
  { _minChar :: Int
  , _maxChar :: Int
  , _char :: Char
  , _password :: ByteString
  }
  deriving (Show, Eq)
type Input = [Password]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = length . filter validPassword

validPassword :: Password -> Bool
validPassword Password {..} =
  let nChar = B.length (B.filter (== _char) _password)
   in nChar >= _minChar && nChar <= _maxChar

------------ PART B ------------
partB :: Input -> OutputB
partB = length . filter validPasswordB

-- One of _minChar or _maxChar indices must contain _char
validPasswordB :: Password -> Bool
validPasswordB Password {..} =
  (_password `B.index` (_minChar - 1) == _char)
    /= (_password `B.index` (_maxChar - 1) == _char)
