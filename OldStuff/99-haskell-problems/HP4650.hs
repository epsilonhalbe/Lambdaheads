module HP4150 where
import Data.List (transpose)

and2 :: Bool -> Bool -> Bool
True `and2` b = b
_ `and2` _ = False

or2 :: Bool -> Bool -> Bool
False `or2` b = b
_ `or2` _ = True

table :: (Bool -> Bool -> Bool) -> [[Bool]]
table f = [[a,b,f a b] | a <- [True,False],
                         b <- [True,False]]

inputValues :: Int -> [[Bool]]
inputValues i = transpose $  map genTable [2^j | j <- [i-1,i-2..0]]
         where genTable = take (2^i) . cycle . tblSequence
               tblSequence i = replicate i True++replicate i False

and',or',nor',nand',xor',impl',equ' :: Bool -> Bool -> Bool
and'      = (&&)
or'       = (||)
nand' a b = not (and' a b)
nor'  a b = not (or' a b)
xor'  a b = not (equ' a b)
impl' a   = or' (not a)
equ'      = (==)

table' :: Int -> ([Bool] -> Bool) -> [[Bool]]
table' i f = [x++[f x]|x <- xs]
           where xs = inputValues i

prettyfy :: (Show a) => [[a]] -> String
prettyfy = (unlines . map unwords .  map . map . id) show

--gray :: Int -> [String]
{-gray i = (map . map) f $ inputValues i
         where f True = '1'
               f False = '0'-}
gray 1 = ["0","1"]
gray n = map ('0':) gg ++ map ('1':) (reverse gg)
       where gg = gray (n-1)

