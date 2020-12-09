module Days.Day08 (runDay, Input, OutputA, OutputB, runA, runB) where

import qualified Data.IntSet as S
import qualified Data.Vector as V

runDay :: Bool -> String -> IO ()
runDay = run inputParser partA partB

runA :: ByteString -> Either String OutputA
runA input = runPart input inputParser partA

runB :: ByteString -> Either String OutputB
runB input = runPart input inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy instr endOfLine
  where
    instr = do
      kind <- take 3
      void space
      n <- signed decimal
      case kind of
        "acc" -> pure (Acc n)
        "jmp" -> pure (Jmp n)
        "nop" -> pure (Nop n)
        _ -> fail $ "invalid instruction " <> show kind

------------ TYPES ------------
data Instruction = Acc Int | Jmp Int | Nop Int
  deriving (Show)
type Input = [Instruction]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA program' = case runProgram program' of
  Left acc -> acc
  Right _ -> error "part a should loop"

runProgram :: [Instruction] -> Either Int Int
runProgram program' = go S.empty 0 0
  where
    program = fromList program'
    go seen acc counter
      | counter == V.length program = Right acc
      | counter `S.member` seen = Left acc
      | otherwise =
        let seen' = S.insert counter seen
         in case program V.! counter of
              Nop _ -> go seen' acc (counter + 1)
              Acc n -> go seen' (acc + n) (counter + 1)
              Jmp n -> go seen' acc (counter + n)

------------ PART B ------------
partB :: Input -> OutputB
partB program =
  let allFlips = enumerateFlips program
      result = rights $ map runProgram allFlips
   in case result of
        [acc] -> acc
        _ -> error "Should have gotten exactly 1 right result"

enumerateFlips :: [Instruction] -> [[Instruction]]
enumerateFlips = go []
  where
    go _ [] = []
    go left (i@(Nop n) : is) =
      flipfoldl' (:) (Jmp n : is) left : go (i : left) is
    go left (i@(Jmp n) : is) =
      flipfoldl' (:) (Nop n : is) left : go (i : left) is
    go left (Acc n : is) = go (Acc n : left) is
