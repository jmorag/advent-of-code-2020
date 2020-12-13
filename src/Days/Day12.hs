{-# LANGUAGE DuplicateRecordFields #-}

module Days.Day12 (runDay, Input, OutputA, OutputB, runA, runB) where

import GHC.Show (Show (..))
import Relude.Extra.Lens
import qualified Relude.String.Conversion as CS

runDay :: Bool -> String -> IO ()
runDay = run inputParser partA partB

runA :: ByteString -> Either String OutputA
runA input = runPart input inputParser partA

runB :: ByteString -> Either String OutputB
runB input = runPart input inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy actionParser endOfLine
  where
    actionParser = liftA2 (,) anyChar decimal

------------ TYPES ------------
type Input = [(Char, Int)]

data Direction = E | S | W | N
  deriving (Show, Eq, Enum, Bounded)

data ShipState = ShipState {_facing :: Direction, _pos :: Point}
  deriving (Show, Eq)

data Point = Pt {_x :: Int, _y :: Int}
  deriving (Eq)

instance Show Point where
  show Pt {..} = GHC.Show.show (_x, _y)

scale :: Int -> Point -> Point
scale k Pt {..} = Pt (_x * k) (_y * k)

-- names stolen from https://hackage.haskell.org/package/linear-1.21.3/docs/Linear-Vector.html
infixl 6 ^+^, ^-^
(^-^), (^+^) :: Point -> Point -> Point
p1 ^+^ p2 = Pt (p1 ^. x + p2 ^. x) (p1 ^. y + p2 ^. y)
p1 ^-^ p2 = Pt (p1 ^. x - p2 ^. x) (p1 ^. y - p2 ^. y)

x :: Lens' Point Int
x = lens _x (\st newX -> st {_x = newX})
y :: Lens' Point Int
y = lens _y (\st newY -> st {_y = newY})
facing :: Lens' ShipState Direction
facing = lens _facing (\st f -> st {_facing = f})
pos :: Lens' ShipState Point
pos = lens _pos (\st p -> st {_pos = p})

initialState :: ShipState
initialState = ShipState E (Pt 0 0)

type OutputA = Int

type OutputB = Int

data StateB = StateB {_ship :: Point, _waypoint :: Point}
  deriving (Show)
ship, waypoint :: Lens' StateB Point
ship = lens _ship (\s a -> s {_ship = a})
waypoint = lens _waypoint (\s a -> s {_waypoint = a})

------------ PART A ------------
partA :: Input -> OutputA
partA = manhattan . view pos . flipfoldl' step initialState

manhattan :: Point -> Int
manhattan p = abs (p ^. x) + abs (p ^. y)

step :: (Char, Int) -> ShipState -> ShipState
step (action, magnitude) st = case action of
  'N' -> over (pos . y) (+ magnitude) st
  'S' -> over (pos . y) (subtract magnitude) st
  'E' -> over (pos . x) (+ magnitude) st
  'W' -> over (pos . x) (subtract magnitude) st
  'F' -> (\dir -> step (dir, magnitude) st)
    case st ^. facing of N -> 'N'; S -> 'S'; E -> 'E'; W -> 'W'
  'L' -> st & over facing \dir -> (fromEnum dir - (magnitude `div` 90)) `mod` 4 & toEnum
  'R' -> st & over facing \dir -> (fromEnum dir + (magnitude `div` 90)) `mod` 4 & toEnum
  _ -> error $ "invalid action " <> one action

------------ PART B ------------
partB :: Input -> OutputB
partB = manhattan . view ship . flipfoldl' stepB initialStateB

initialStateB :: StateB
initialStateB = StateB (Pt 0 0) (Pt 10 1)

stepB :: (Char, Int) -> StateB -> StateB
stepB (action, magnitude) st = case action of
  'N' -> over (waypoint . y) (+ magnitude) st
  'S' -> over (waypoint . y) (subtract magnitude) st
  'E' -> over (waypoint . x) (+ magnitude) st
  'W' -> over (waypoint . x) (subtract magnitude) st
  'F' ->
    let diff = st ^. waypoint ^-^ st ^. ship
        ship' = st ^. ship ^+^ scale magnitude diff
        waypoint' = ship' ^+^ diff
     in StateB ship' waypoint'
  'R' -> stepB ('L', case magnitude of { 90 -> 270; 270 -> 90; m -> m }) st
  'L' ->
    st & waypoint
      .~ let waypoint' = st ^. waypoint ^-^ st ^. ship -- center around origin
             waypoint'' = case magnitude of -- rotate
              90 -> Pt (negate $ waypoint' ^. y) (waypoint' ^. x)
              180 -> Pt 0 0 ^-^ waypoint'
              270 -> Pt (waypoint' ^. y) (negate $ waypoint' ^. x)
              _ -> error $ "invalid magnitude " <> CS.show magnitude
             waypoint''' = waypoint'' ^+^ st ^. ship -- translate back
          in waypoint'''
  _ -> error $ "invalid action " <> one action
