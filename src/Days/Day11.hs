module Days.Day11 (runDay, Input, OutputA, OutputB, runA, runB, visible, inputParser) where

import Data.Array
import GHC.Show
import Relude.Extra.Enum

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
  deriving (Eq)

instance Show Cell where
  show = \case
    Empty -> "L"
    Occupied -> "#"
    Floor -> "."

type Input = Array (Int, Int) Cell

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = simulate step

adjacent :: Array (Int, Int) a -> (Int, Int) -> [a]
adjacent arr (i, j) = do
  i' <- [i, i + 1, i -1]
  j' <- [j, j + 1, j -1]
  let ix = (i', j')
  guard $ ix /= (i, j) && inRange (bounds arr) ix
  pure $ arr ! ix

step :: Array (Int, Int) Cell -> Array (Int, Int) Cell
step arr = imap go arr
  where
    go ix = \case
      Empty | null occupied -> Occupied
      Occupied | length occupied >= 4 -> Empty
      seat -> seat
      where
        occupied = adjacent arr ix & filter (== Occupied)

imap :: Ix i => (i -> e -> e') -> Array i e -> Array i e'
imap f arr = map (\(i, e) -> (i, f i e)) (assocs arr) & array (bounds arr)

converge :: (Eq a) => (a -> a) -> a -> a
converge f x = let x' = f x in if x' == x then x else converge f x'

simulate :: Ix i => (Array i Cell -> Array i Cell) -> Array i Cell -> Int
simulate stepper input = converge stepper input & elems & filter (== Occupied) & length

------------ PART B ------------
partB :: Input -> OutputB
partB = simulate step2

visible :: Array (Int, Int) Cell -> (Int, Int) -> Int
visible arr ix = map (march ix) directions & filter id & length
  where
    directions :: [(Int, Int) -> (Int, Int)]
    directions =
      map
        (uncurry bimap)
        [ (id, next)
        , (id, prev)
        , (next, id)
        , (next, next)
        , (next, prev)
        , (prev, next)
        , (prev, id)
        , (prev, prev)
        ]
    march i move =
      let ix' = move i
       in inRange (bounds arr) ix' && case arr ! ix' of
            Occupied -> True
            Empty -> False
            Floor -> march ix' move

step2 :: Input -> Input
step2 arr = imap go arr
  where
    go ix = \case
      Occupied | visible arr ix >= 5 -> Empty
      Empty | visible arr ix == 0 -> Occupied
      seat -> seat
