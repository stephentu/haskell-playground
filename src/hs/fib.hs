-- magical
fibs :: [Int]
fibs = 0 : 1 : [a+b | (a,b) <- zip fibs (tail fibs)]
