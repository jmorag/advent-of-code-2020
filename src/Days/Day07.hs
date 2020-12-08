module Days.Day07 (runDay, Input, OutputA, OutputB, runA, runB) where

import Data.Graph.DGraph
import Data.Graph.Traversal
import Data.Graph.Types

runDay :: Bool -> String -> IO ()
runDay = run inputParser partA partB

runA :: ByteString -> Either String OutputA
runA input = runPart input inputParser partA

runB :: ByteString -> Either String OutputB
runB input = runPart input inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = concat <$> sepBy edgeParser endOfLine

-- edge goes from container to containee
edgeParser :: Parser [Arc ByteString Int]
edgeParser = do
  container <- bag
  void "bags contain "
  containees <- ([] <$ "no other bags") <|> sepBy containeeParser ", "
  void (char '.')
  pure $ map (\(n, containee) -> Arc container containee n) containees

containeeParser :: Parser (Int, ByteString)
containeeParser = do
  n <- decimal <* space
  color <- bag
  void "bag"
  peekChar' >>= \case 's' -> void "s"; _ -> pass
  pure (n, color)

word :: Parser ByteString
word = takeWhile1 isAlpha_ascii

bag :: Parser ByteString
bag = do
  adj <- word
  void space
  color <- word
  void space
  pure (adj <> " " <> color)

------------ TYPES ------------
type Input = [Arc ByteString Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA edges =
  let edges' = map (\(Arc v v' e) -> Arc v' v e) edges
      graph = fromArcsList edges'
   in length (dfsVertices graph "shiny gold") - 1 -- don't count starting vertex

------------ PART B ------------
partB :: Input -> OutputB
partB edges = go 1 "shiny gold" - 1
  where
    graph =
      let g = fromArcsList edges
       in if isSimple g then g else error "AHHHHHHHHH LOOPS"
    go n v =
      let next = reachableAdjacentVertices' graph v
       in n + n * sum (map (\(_, v', n') -> go n' v') next)
