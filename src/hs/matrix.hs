import Control.Exception (assert)
import Data.List (mapAccumL)

data Matrix a = Matrix {
  rowMajor :: [[a]],
  colMajor :: [[a]],
  rows :: Int,
  cols :: Int
} deriving (Show)

data Vector a = Vector {
  vals :: [a],
  size :: Int
} deriving (Show)

floatAlmostEq :: (RealFloat a) => a -> a -> Bool
floatAlmostEq a b = abs (a-b) <= 1e-5

emptyMatrix :: (RealFloat a) => Matrix a
emptyMatrix = Matrix [] [] 0 0

buildMatrix :: (RealFloat a) => [[a]] -> Matrix a
buildMatrix []   = emptyMatrix
buildMatrix [[]] = emptyMatrix

buildMatrix rows =
    Matrix rows cols nRows nCols
  where
    cols  = reprTranspose rows
    nRows = length rows
    nCols = length $ head rows
    _     = foldl (\x _ -> assert (x == nCols) nCols) nCols (tail rows)

emptyVector :: (RealFloat a) => Vector a
emptyVector = Vector [] 0

buildVector :: (RealFloat a) => [a] -> Vector a
buildVector a = Vector a (length a)

reprTranspose0 :: (RealFloat a) => [[a]] -> [[a]] -> [[a]]
reprTranspose0 accum values
  | null $ head values = accum
  | otherwise          = reprTranspose0 (col:accum) valuesNext
  where
    col        = map head values
    valuesNext = map tail values

reprTranspose :: (RealFloat a) => [[a]] -> [[a]]
reprTranspose values = reverse $ reprTranspose0 [] values

reprEq :: (RealFloat a) => [[a]] -> [[a]] -> Bool
reprEq a b = reprIsZero $ reprMinus a b

reprOneIsZero :: (RealFloat a) => [a] -> Bool
reprOneIsZero r = all (\x -> floatAlmostEq x 0) r

reprIsZero :: (RealFloat a) => [[a]] -> Bool
reprIsZero a = all id $ map reprOneIsZero a

reprPlus :: (RealFloat a) => [[a]] -> [[a]] -> [[a]]
reprPlus a b = map (\x -> reprOnePlus (fst x) (snd x)) $ zip a b

reprMinus :: (RealFloat a) => [[a]] -> [[a]] -> [[a]]
reprMinus a b = map (\x -> reprOneMinus (fst x) (snd x)) $ zip a b

reprOnePlus :: (RealFloat a) => [a] -> [a] -> [a]
reprOnePlus a b = [x + y | (x,y) <- zip a b]

reprOneMinus :: (RealFloat a) => [a] -> [a] -> [a]
reprOneMinus a b = [x - y | (x,y) <- zip a b]

reprOneDot :: (RealFloat a) => [a] -> [a] -> a
reprOneDot a b = foldl (\acc (a, b) -> acc + (a*b)) 0 $ zip a b

reprOneScale :: (RealFloat a) => [a] -> a -> [a]
reprOneScale a b = map (*b) a

reprOneNormalize :: (RealFloat a) => [a] -> [a]
reprOneNormalize a = reprOneScale a $ sqrt (reprOneDot a a)

-- naive grade school algorithm
matrixMult :: (RealFloat a) => Matrix a -> Matrix a -> Matrix a
matrixMult a b = buildMatrix $ map mkRow $ rowMajor a
  where
    mkRow row = map (reprOneDot row) (colMajor b)
    _ = assert (cols a == rows b) 0

linear :: (RealFloat a) => Matrix a -> Vector a -> Vector a
linear a b = buildVector $ map mkRow $ rowMajor a
  where
    mkRow row = reprOneDot row $ vals b
    _ = assert (cols a == size b) 0

dot :: (RealFloat a) => Vector a -> Vector a -> a
dot a b = reprOneDot (vals a) (vals b)

scale :: (RealFloat a) => Vector a -> a -> Vector a
scale a b = buildVector $ reprOneScale (vals a) b

secondMomentProduct :: (RealFloat a) => Vector a -> Matrix a
secondMomentProduct a = Matrix repr repr (size a) (size a)
  where
    repr = map (\v -> reprOneScale (vals a) v) (vals a)

-- proj(u, v) = (u^T v)/(u^T u) u
reprOneProjOperator :: (RealFloat a) => [a] -> [a] -> [a]
reprOneProjOperator u v
  | reprOneIsZero u = take (length u) $ repeat 0
  | otherwise       = reprOneScale u ((reprOneDot u v) / (reprOneDot u u))

-- assumes matrix has linearly independent *columns*
-- <=> if size(M) = [n, k] then k <= n and rank(M) = k
gramSchmidt :: (RealFloat a) => Matrix a -> Matrix a
gramSchmidt m = buildMatrix $ snd $ mapAccumL fn [] (colMajor m)
  where
    _ = assert ((cols m) <= (rows m))
    removeComponents acc col = map (\x -> reprOneProjOperator x col) acc
    reprOneSum reprOnes = foldl reprOnePlus (head reprOnes) (tail reprOnes)
    fn acc col = (newCol:acc, reprOneNormalize newCol)
       where newCol = col `reprOneMinus` (reprOneSum $ removeComponents acc col)
