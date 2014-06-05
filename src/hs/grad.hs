-- 1D gradient descent

-- (grad, iters_left, w_t, eta) -> w_tp1
gradDesc :: (Float -> Float) -> Int -> Float -> Float -> Float
gradDesc _ 0 w _    = w
gradDesc df n w eta = gradDesc df (n-1) (w - eta * (df w)) eta

-- poly representation is [c0, c1, c2, c3, ...]

polyEval0 :: Int -> [Float] -> Float -> Float
polyEval0 _ [] _     = 0.0
polyEval0 0 (c:cs) x = c + polyEval0 1 cs x
polyEval0 i (c:cs) x = c * (x^i) + polyEval0 (i+1) cs x

polyEval :: [Float] -> Float -> Float
polyEval cs x = polyEval0 0 cs x

polyGrad :: [Float] -> (Float -> Float)
polyGrad (_:cs) = polyEval [c*i | (c, i) <- zip cs [1..]]
