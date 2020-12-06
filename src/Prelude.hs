module Prelude (module X, run, runPart, V.Vector, many1, sepBy1) where

import Data.Attoparsec.ByteString.Char8 as X hiding (many1, sepBy1)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.Vector as V
import Relude as X hiding (take, takeWhile)
import System.Directory (doesFileExist)

run :: (Show a, Show b, Show i) => Parser i -> (i -> a) -> (i -> b) -> Bool -> FilePath -> IO ()
run inputParser partA partB verbose inputFile = do
  inputFileExists <- doesFileExist inputFile
  unless inputFileExists do
    fail $ "I couldn't read the input! I was expecting it to be at " <> inputFile
  input <- Atto.parseOnly inputParser <$> readFileBS inputFile
  case input of
    Left e -> fail $ "Parser failed to read input. Error " <> e
    Right i -> do
      when verbose $ putTextLn "Parser output:" >> print i
      putTextLn "Part A:"
      print (partA i)
      putTextLn "Part B:"
      print (partB i)

-- | Pure version for tests which generally have small inputs
-- (hopefully we never need a any parts to run in IO ...)
runPart :: (Show i, Show o) => ByteString -> Parser i -> (i -> o) -> Either String o
runPart input parser part = fmap part (Atto.parseOnly parser input)

many1 :: Parser a -> Parser (NonEmpty a)
many1 p =
  Atto.many1 p <&> \case
    x : xs -> x :| xs
    [] -> error "impossible - some returned empty list"

sepBy1 :: Parser a -> Parser sep -> Parser (NonEmpty a)
sepBy1 p sep =
  Atto.sepBy1 p sep <&> \case
    x : xs -> x :| xs
    [] -> error "impossible - sepBy1 returned empty list"
