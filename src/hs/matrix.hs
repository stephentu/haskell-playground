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
    cols       = reprTranspose rows
    nRows      = length rows
    nCols      = foldl (\x r -> assert (length r == x) x) firstNCols (tail rows)
    firstNCols = length $ head rows

emptyVector :: (RealFloat a) => Vector a
emptyVector = Vector [] 0

buildVector :: (RealFloat a) => [a] -> Vector a
buildVector a = Vector a (length a)

reprIdentity :: (RealFloat a) => Int -> [[a]]
reprIdentity n = map mkRow (take n [0..])
  where
    zeroSource = repeat 0
    mkRow i    = (take i zeroSource) ++ [1] ++ (take (n-i-1) zeroSource)

reprTranspose0 :: (RealFloat a) => [[a]] -> [[a]] -> [[a]]
reprTranspose0 accum values
  | null $ head values = accum
  | otherwise          = reprTranspose0 (col:accum) valuesNext
  where
    col        = map head values
    valuesNext = map tail values

reprTranspose :: (RealFloat a) => [[a]] -> [[a]]
reprTranspose values = reverse $ reprTranspose0 [] values

reprOneEq :: (RealFloat a) => [a] -> [a] -> [Bool]
reprOneEq a b = map (\x -> floatAlmostEq x 0) $ reprOneMinus a b

binToTupleFn :: (a -> b -> c) -> ((a,b) -> c)
binToTupleFn fn = \tup -> fn (fst tup) (snd tup)

reprEq :: (RealFloat a) => [[a]] -> [[a]] -> [[Bool]]
reprEq a b = map (binToTupleFn reprOneEq) $ zip a b

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

reprOneNorm :: (RealFloat a) => [a] -> a
reprOneNorm a = sqrt $ reprOneDot a a

reprScale :: (RealFloat a) => [[a]] -> a -> [[a]]
reprScale a b = map (\row -> reprOneScale row b) a

reprOneScale :: (RealFloat a) => [a] -> a -> [a]
reprOneScale a b = map (*b) a

reprOneNormalize :: (RealFloat a) => [a] -> [a]
reprOneNormalize a = reprOneScale a $ (1 / (reprOneNorm a))

-- naive grade school algorithm
matrixMult :: (RealFloat a) => Matrix a -> Matrix a -> Matrix a
matrixMult a b = buildMatrix $ map mkRow $ rowMajor a
  where
    mkRow row = map (reprOneDot row) (colMajor (assert (cols a == rows b) b))

matrixTranspose :: (RealFloat a) => Matrix a -> Matrix a
matrixTranspose a = a{colMajor = (rowMajor a), rowMajor = (colMajor a), rows = (cols a), cols = (rows a)}

matrixEq :: (RealFloat a) => Matrix a -> Matrix a -> [[Bool]]
matrixEq a b = reprEq (colMajor a) (colMajor b)

matrixIdentity :: (RealFloat a) => Int -> Matrix a
matrixIdentity n = buildMatrix $ reprIdentity n

linear :: (RealFloat a) => Matrix a -> Vector a -> Vector a
linear a b = buildVector $ map mkRow $ rowMajor a
  where
    mkRow row = reprOneDot row $ vals (assert (cols a == size b) b)

dot :: (RealFloat a) => Vector a -> Vector a -> a
dot a b = reprOneDot (vals a) (vals b)

scale :: (RealFloat a) => Vector a -> a -> Vector a
scale a b = buildVector $ reprOneScale (vals a) b

reprOneSecondMomentProduct :: (RealFloat a) => [a] -> [[a]]
reprOneSecondMomentProduct a = map (\v -> reprOneScale a v) a

secondMomentProduct :: (RealFloat a) => Vector a -> Matrix a
secondMomentProduct a = Matrix repr repr (size a) (size a)
  where
    repr = reprOneSecondMomentProduct $ vals a

-- proj(u, v) = (u^T v)/(u^T u) u
reprOneProjOperator :: (RealFloat a) => [a] -> [a] -> [a]
reprOneProjOperator u v
  | reprOneIsZero u = take (length u) $ repeat 0
  | otherwise       = reprOneScale u ((reprOneDot u v) / (reprOneDot u u))

gramSchmidt :: (RealFloat a) => Matrix a -> Matrix a
gramSchmidt m = buildMatrix $ reprTranspose (snd $ mapAccumL fn [] (colMajor m))
  where
    removeComponents acc col = map (\x -> reprOneProjOperator x col) acc
    reprOneSum reprOnes      = foldl reprOnePlus (head reprOnes) (tail reprOnes)
    fn acc col               = (newAcc, newCol)
      where
        (newAcc, newCol) = case acc of
          []        -> if reprOneIsZero col then ([], []) else ([col], reprOneNormalize col)
          otherwise -> if reprOneIsZero testNewCol then (acc, []) else (testNewCol:acc, reprOneNormalize testNewCol)
            where
              testNewCol = col `reprOneMinus` (reprOneSum $ removeComponents acc col)

-- compute householder matrix H of b and k (k is indexed by 0)
householder :: (RealFloat a) => Vector a -> Int -> Matrix a
householder b k = buildMatrix hrepr
  where
    n     = size b
    dlen  = n - k -- k is indexed by 0
    w     = (take (n-dlen) $ repeat 0) ++ (assert (length v == length d) v)
    d     = drop (k) $ vals b
    alpha = -1 * (sgn (head d)) * (reprOneNorm d)
    sgn s = if s < 0 then -1 else 1
    v0    = sqrt (0.5 * (1 - ((head d)/alpha)))
    p     = -1 * alpha * v0
    v     = v0 : (map (/(2*p)) $ tail d)
    hrepr = reprIdentity n `reprMinus` (reprScale (reprOneSecondMomentProduct w) 2)
