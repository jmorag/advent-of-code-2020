module Days.Day05 (
  runDay,
  Input,
  OutputA,
  OutputB,
  runA,
  runB,
  findSeat,
  Seat (..),
  boardingPassParser,
) where

import Data.Foldable
import qualified Data.List.NonEmpty as NE

runDay :: Bool -> String -> IO ()
runDay = run inputParser partA partB

runA :: ByteString -> Either String OutputA
runA input = runPart input inputParser partA

runB :: ByteString -> Either String OutputB
runB input = runPart input inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy1 boardingPassParser endOfLine

boardingPassParser :: Parser BoardingPass
boardingPassParser = do
  rowDirs <- count 7 $ Lower <$ char 'F' <|> Upper <$ char 'B'
  colDirs <- count 3 $ Lower <$ char 'L' <|> Upper <$ char 'R'
  pure $ BoardingPass {..}

------------ TYPES ------------
data Seat = Seat {row :: Int, col :: Int}
  deriving (Show, Eq)

seatId :: Seat -> Int
seatId Seat {..} = row * 8 + col

data BinTree = Node BinTree BinTree | Leaf Int
  deriving (Show)

data Half = Upper | Lower
  deriving (Show)

data BoardingPass = BoardingPass {rowDirs :: [Half], colDirs :: [Half]}
  deriving (Show)

type Input = NonEmpty BoardingPass

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = maximum . fmap (seatId . findSeat)

findSeat :: BoardingPass -> Seat
findSeat BoardingPass {..} = Seat (search rowDirs) (search colDirs)

search :: [Half] -> Int
search dirs = go dirs (makeBinTree (length dirs))
  where
    go [] (Leaf x) = x
    go (d : ds) (Node l r) = go ds case d of Upper -> r; Lower -> l
    go _ _ = error "impossible tree"

makeBinTree :: Int -> BinTree
makeBinTree nBits | nBits < 1 = error "makeBinTree called with less than 1 bit"
makeBinTree nBits = go 0 (2 ^ nBits - 1)
  where
    go low high
      | high - low == 1 = Node (Leaf low) (Leaf high)
      | otherwise =
        let mid = ((high - low) `div` 2) + low
         in Node (go low mid) (go (mid + 1) high)

------------ PART B ------------
partB :: Input -> OutputB
partB ps =
  let (i :| ids) = NE.sort (fmap (seatId . findSeat) ps)
   in case find (\(sId, ix) -> ix < sId) (zip (i : ids) [i ..]) of
        Just (_, ix) -> ix
        Nothing -> error "Couldn't find seat"
