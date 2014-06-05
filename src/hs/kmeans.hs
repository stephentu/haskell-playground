-- 1D k-means

import Data.List
import Data.Ord
import System.Random

groupByCluster :: (Ord b) => [(a, b)] -> [[a]]
groupByCluster xs =
  map (\x -> map fst x) $
  groupBy (\x y -> snd x == snd y) $
  sortBy (comparing snd) xs

argmin :: Ord a => [a] -> Int
argmin xs = snd (foldl update ((head xs, 0)) (zip (tail xs) [1..]))
  where update s@(vbest, ibest) t@(vcur, icur) = if vcur < vbest then t else s

dist :: Float -> Float -> Float
dist a b = diff * diff where diff = a - b

vecMinus :: [Float] -> [Float] -> [Float]
vecMinus as bs = [a-b | (a, b) <- zip as bs]

vecSqNorm :: [Float] -> Float
vecSqNorm as = sum $ map (^2) as

-- Takes a point x and k clusters [x_1, ..., x_k],
-- returns \argmin_{i} || x - x_i ||^2
closestCluster :: Float -> [Float] -> Int
closestCluster x xs = argmin $ map (dist x) xs

avgPoint :: [Float] -> Float
avgPoint xs = (sum xs) / (fromIntegral $ length xs)

--- (points, current_means) -> (new_means)
newClusters :: [Float] -> [Float] -> [Float]
newClusters points clusters =
  map avgPoint $ groupByCluster assignments
    where assignments = [(p, closestCluster p clusters) | p <- points]

--- (points, current_guess, tolerance) -> finished_guess
kmeans0 :: [Float] -> [Float] -> Float -> [Float]
kmeans0 points curGuess tol
  | curDiff <= tol = nextGuess
  | otherwise      = kmeans0 points nextGuess tol
  where
    nextGuess = newClusters points curGuess
    curDiff   = vecSqNorm $ vecMinus curGuess nextGuess

kmeans :: [Float] -> Int -> Float -> StdGen -> [Float]
kmeans points k tol g = kmeans0 points startGuess tol
  where
    startGuess = take k $ randomRs (-1::Float, 1::Float) g

main = do
  let points = [-1, -2, -3, 3, 2, 1]
  g <- newStdGen
  putStrLn $ "clusters: " ++ (show $ kmeans points 2 0.001 g)
