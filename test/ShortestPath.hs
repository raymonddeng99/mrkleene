import Data.Semiring
import Data.List (foldl')

type Graph = [[Double]]

-- Kleene algebras to solve shortest path problem
shortestPath :: Graph -> Graph
shortestPath graph = fixpoint
  where
    instanceSemiring :: Semiring Double
    instanceSemiring = minRing (*)

    multiplyMatrices :: Graph -> Graph -> Graph
    multiplyMatrices a b =
      let rows = length a
          cols = length (head b)
      in [[foldl' (+) 0 [a !! i !! k * b !! k !! j | k <- [0 .. cols - 1]] | j <- [0 .. cols - 1]] | i <- [0 .. rows - 1]]

    kleeneStar :: Graph -> Graph
    kleeneStar a = fixpoint
      where
        fixpoint = a `add` (a `multiplyMatrices` fixpoint)

    shortestPathMatrix = kleeneStar graph

main :: IO ()
main = do
  let graph =
        [ [0, 3, infinity, 7]
        , [8, 0, 2, infinity]
        , [5, infinity, 0, 1]
        , [2, infinity, infinity, 0]
        ]
      where
        infinity = 1e9

  putStrLn "Shortest Path Matrix:"
  mapM_ (print . map formatDouble) (shortestPath graph)
  where
    formatDouble x
      | x == 1e9 = "∞"
      | otherwise = show x
