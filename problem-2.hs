module Problem2 where



f limit =
  sum $ filter even $ takeWhile (< limit) fibs
    where fibs = 1:1:zipWith (+) fibs (tail fibs)

  (+ 1) <$> [1, 2, 3, 4]
         -- [2, 3, 4, 5]

  (+1) <$> Just 5
  (+1) <$> Nothing

  Just (+1) <*> Just 5
  Nothing <*> Nothing

  [(+1), (+2), (+3)] <*> Just 5
