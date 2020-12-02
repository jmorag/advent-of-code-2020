module Prelude (module X, run, runPart, V.Vector) where

import Relude as X

import Data.Attoparsec.ByteString
import qualified Data.Vector as V
import System.Directory (doesFileExist)

run :: (Show a, Show b, Show i) => Parser i -> (i -> a) -> (i -> b) -> Bool -> FilePath -> IO ()
run inputParser partA partB verbose inputFile = do
  inputFileExists <- doesFileExist inputFile
  unless inputFileExists do
    fail $ "I couldn't read the input! I was expecting it to be at " <> inputFile
  input <- parseOnly inputParser <$> readFileBS inputFile
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
runPart input parser part = fmap part (parseOnly parser input)
