isBeforeMiddle :: Int -> Int -> Bool
isBeforeMiddle is l = is < (quot l 2)

isExactMiddle :: Int -> Int -> Bool
isExactMiddle is l
  | l `mod` 2 == 1 = is == (quot l 2)
  | otherwise      = False

checkPalin0 :: (Eq a) => [a] -> [a] -> Int -> Int -> Bool
checkPalin0 xs ss is l =
  case (xs, isBeforeMiddle is l, isExactMiddle is l) of
    ([], _, _)            -> True
    (e:xss, True, _)      -> checkPalin0 xss (e:ss) (is+1) l
    (_:xss, _, True)      -> checkPalin0 xss ss (is+1) l
    (e:xss, False, False) -> if e == head ss then checkPalin0 xss (tail ss) (is+1) l else False

checkPalin :: (Eq a) => [a] -> Bool
checkPalin xs = checkPalin0 xs [] 0 (length xs)
