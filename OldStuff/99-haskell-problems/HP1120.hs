module HP1120 where
import Data.List (group)
import Control.Arrow (first,
                      (&&&))

data Count a = Single a
             | Multiple Int a
             deriving (Eq, Show)

encode :: String -> [(Int, Char)]
encode = map (length &&& head) . group

encodeModified :: String -> [Count Char]
encodeModified  = map transform . encode

transform :: (Int,Char) -> Count Char
transform (1,x) = Single x
transform (i,x) = Multiple i x

decodeModified :: [Count Char] -> String
decodeModified = concatMap stringify

stringify :: Count Char -> String
stringify (Single x) = [x]
stringify (Multiple i x) = replicate i x

(.+.) :: Count Char -> [Count Char] -> [Count Char]
x .+. [] = [x]
y@(Single x2)       .+. xx@(Single x1:xs)      | x1 == x2   = Multiple 2 x1:xs
                                               | otherwise = y:xx
y@(Single x2 )      .+. xx@(Multiple i1 x1:xs) | x1 == x2   = Multiple (i1+ 1) x1:xs
                                               | otherwise = y:xx
y@(Multiple i2 x2)  .+. xx@(Single x1:xs)      | x1 == x2   = Multiple (1 +i2) x1:xs
                                               | otherwise = y:xx
y@(Multiple i2 x2 ) .+. xx@(Multiple i1 x1:xs) | x1 == x2   = Multiple (i1+i2) x1:xs
                                               | otherwise = y:xx

encodeDirect :: String -> [Count Char]
-- encodeDirect = foldr (.+.) [] . map transform . zip (repeat 1)
encodeDirect = foldr ((.+.) . transform) [] . zip (repeat 1)

dupli :: [a] -> [a]
dupli = foldr (\x y -> x:x:y) []

repli :: [a] -> Int -> [a]
repli xx i =  foldr (\x y -> replicate i x++y) [] xx

dropEvery :: [a] -> Int -> [a]
dropEvery xx 0 = xx
dropEvery xx i = _dropEvery xx i
               where _dropEvery [] _ = []
                     _dropEvery (x:xs) 1 = _dropEvery xs i
                     _dropEvery (x:xs) n = x:_dropEvery xs (n-1)

split :: [a] -> Int -> ([a],[a])
split x 0 = ([],x)
split x i | i < 0 = split x (length x + i)
          | otherwise =  _split ([],x) i
          where _split xx 0 = xx
                _split (y,x:xs) i = _split (y++[x],xs) (i-1)

slice :: [a] -> Int -> Int -> [a]
slice xx i1 i2 = [x| (x,i)<- zip xx [1..], i1<=i,i<=i2]

rotate :: [a] -> Int -> [a]
rotate x i = bb++aa
           where (aa,bb) = split x i

removeAt :: Int -> [a] -> (a,[a])
removeAt i x | i<0 = (b,aa++bs)
             where (aa,b:bs) = split x i
removeAt i x = (head a,as++bb)
             where (aa,bb) = split x (i+1)
                   (as,a ) = split aa (-1)


