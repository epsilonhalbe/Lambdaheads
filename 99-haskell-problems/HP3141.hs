module HP3141 where

minus :: (Ord a) => [a] -> [a] -> [a]
minus (x:xs) (y:ys) = case compare x y of
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs

-- isPrime :: Int -> Bool
-- isPrime i = foldr (\x y -> i `rem` x /= 0 && y) True (primeList i)
isPrime i = last (primeList i) == i

primeList :: Int -> [Int]
primeList m = 2 : sieve [3,5..m]
            where sieve (p:xs) | p*p > m   = p : xs
                               | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])

