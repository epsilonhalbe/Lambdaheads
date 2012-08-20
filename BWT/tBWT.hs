import BWT
import Test.QuickCheck
import Data.List (sort)

main :: IO ()
main = do
    putStrLn     "idPROP"
    quickCheck   (idPROP       :: String -> Bool)
    putStrLn     "tfIdPROP"
    quickCheck   (tfIdPROP     :: String -> Property)
    putStrLn     "lrrotPROP"
    quickCheck   (lrrotPROP    :: String -> Property)
    putStrLn     "rlrotPROP"
    quickCheck   (rlrotPROP    :: String -> Property)
    putStrLn     "recreateSPEC"
    verboseCheck (recreateSPEC :: Int -> String -> Property)
    putStrLn     "headsortPROP1"
    verboseCheck (headsortPROP1 :: Int -> String -> Property)
    putStrLn     "headsortPROP2"
    verboseCheck (headsortPROP2 :: Int -> String -> Property)
    putStrLn     "headsortPROP3"
    verboseCheck (headsortPROP3 :: Int -> String -> Property)
    putStrLn     "headsortPROP4"
    verboseCheck (headsortPROP4 :: Int -> String -> Property)

idPROP :: Eq a => a -> Bool
idPROP x = x == x

tfIdPROP :: String -> Property
tfIdPROP xs = not (null xs) ==> (untransform . transform) xs == xs

{-recreateSPEC :: (Ord a, Eq a )=> [a] -> Bool-}
{-recreateSPEC xs = (recreate . map last . sort . rots) xs == (sort . rots) xs-}

lrrotPROP :: Eq a => [a] -> Property
lrrotPROP xs = not (null xs) ==> (lrot . rrot) xs == xs

rlrotPROP :: Eq a => [a] -> Property
rlrotPROP xs = not (null xs) ==> (rrot . lrot) xs == xs

recreateSPEC :: (Ord a, Eq a) => Int -> [a] -> Property
recreateSPEC j xs = j >= 0 && j <= length xs ==> (recreate j . map last . sort . rots) xs == (takeCols j . sort . rots) xs

headsortPROP1 :: (Ord a, Eq a) => Int -> [[a]] -> Bool
headsortPROP1 j xs = (takeCols j . headsort) xs == (headsort . takeCols j) xs

headsortPROP2 :: (Ord a, Eq a) => [[a]] -> Bool
headsortPROP2 xs = (headsort . map rrot . sort . rots) xs == (sort . rots) xs

headsortPROP3 :: (Ord a, Eq a) => [[a]] -> Bool
headsortPROP3 xs = sort xs == iterateN (headsort . map rrot) n xs
                  where n = length xs

headsortPROP4 :: (Ord a, Eq a) => [a] -> Bool
headsortPROP4 xs = (headsort . map wrap) xs == (map wrap . sort) xs
                  where wrap x = [x]

sortPROP :: (Ord a, Eq a) => [a] -> Bool
sortPROP ys = sort ys == apply ps ys
             where ps = p ys

tpPROP1 :: [a] -> Bool
tpPROP1 xs = (tp . take 0) xs == map (const []) xs

tpPROP2 :: Int -> [a] -> Property
tpPROP2 j xs = j >= 0 && j <= length xs ==> (tp . take (j +1)) xs ==  (consCol . fork (head , tp . take j . tail )) xs

tpPROP3 :: [a] -> Bool
tpPROP3 xs = (apply pp . tp) xs == (tp . map (apply pp)) xs
            where pp = p xs

{-applyPROP :: (Ord a, Eq a) => Int j[a] -> Bool-}
{-applyPROP j xs = (apply p . consCol) xs == (consCol . pair (apply p)) xs-}
               {-where p = permutations [1..j] !! n-}
                     {-r = getStdRandom (randomR (0,product [1..j]))-}
                     {-r <- n-}

{---------------------------------------------------------------------------
 -                           auxiliary functions                           -
 ---------------------------------------------------------------------------}

iterateN :: (a -> a) -> Int -> a -> a
iterateN f n x = foldr ($) x ff
               where ff = replicate n f

pair :: (a -> b) -> (a, a) -> (b, b)
pair f (x,y) = (f x,f y)
