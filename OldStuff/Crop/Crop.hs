module Crop.Crop where

class Transformable a where
    rrot :: a -> a
    lrot :: a -> a
    lrflip :: a -> a
    udflip :: a -> a
    transpose :: a -> a

-- import Data.List (splitAt)
data Crop = Crop [[Int]]
          deriving (Show)
instance Transformable Crop where
    rrot (Crop a) = Crop (rrotMatrix a)
    lrot (Crop a) = Crop (lrotMatrix a)
    lrflip (Crop a) = Crop (lrFlip a)
    udflip (Crop a) = Crop (udFlip a)
    transpose (Crop a) = Crop (transposeMatrix a)


data Direction = U
              |L | R|
                 D deriving (Show)
instance Transformable Direction where
    rrot U = R
    rrot R = D
    rrot D = L
    rrot L = U
    lrot U = L
    lrot L = D
    lrot D = R
    lrot R = U
    lrflip U = U
    lrflip L = R
    lrflip R = L
    lrflip D = D
    udflip U = D
    udflip L = L
    udflip R = R
    udflip D = U
    transpose L = U
    transpose R = D
    transpose D = R
    transpose U = L

data Corner = LU | RU
            | LD | RD deriving (Show)
instance Transformable Corner where
    rrot LU = RU
    rrot RU = RD
    rrot RD = LD
    rrot LD = LU
    lrot LU = LD
    lrot LD = RD
    lrot RD = RU
    lrot RU = LU
    lrflip LU = RU
    lrflip RU = LU
    lrflip LD = RD
    lrflip RD = LD
    udflip LU = LD
    udflip RU = RD
    udflip LD = LU
    udflip RD = RU
    transpose LU = LU
    transpose RD = RD
    transpose LD = RU
    transpose RU = LD

rcMatrix :: Int -> Int -> [[Int]]
rcMatrix r c = matrix r [1..n]
            where n = r*c
crMatrix :: Int -> Int -> [[Int]]
crMatrix c r = rcMatrix r c

stringify :: Show a => [a] -> String
stringify xx = unwords $ map show xx

nextCorner :: Corner -> Direction -> Corner
nextCorner LU R = RU
nextCorner LU D = LD
nextCorner RU L = LU
nextCorner RU D = RD
nextCorner LD R = RD
nextCorner LD U = LU
nextCorner RD L = LD
nextCorner RD U = RU
nextCorner _ _ = undefined

genCorners :: Corner -> [Direction] -> [Corner]
genCorners c [] = [c]
genCorners c (d:ds) = c: genCorners next ds
                   where next = nextCorner c d

genHarvestPath :: Corner -> [Direction] -> [(Corner,Direction)]
genHarvestPath c ds = zip (genCorners c ds) ds

matrix :: Int -> [a] -> [[a]]
matrix _ [] = []
matrix rsize xx = h:matrix rsize t
                where (h,t) = splitAt rsize xx

harvest :: Crop -> [(Corner,Direction)] -> [Int]
harvest _ [] = []
harvest (Crop []) _ = []
harvest as (cd:csds) = b ++ harvest (Crop bs) csds'
                     where f :: Transformable a => a -> a
                           f = turnLUR cd
                           Crop (b:bs) = f as
                           fcfd (c,d) = (f c,f d)
                           csds' = map fcfd csds

harvest2 :: Crop -> [(Corner,Direction)] -> [Int]
harvest2 _ [] = []
harvest2 (Crop []) _ = []
harvest2 (Crop [x]) csds = harvest (Crop [x]) csds
harvest2 as (cd:csds) = rows2 b1 b2 ++ harvest2 (Crop $ swap1strows bs) csds'
                     where fC = turnLUR cd
                           Crop (b1:b2:bs) = fC as
                           fc = turnLUR cd
                           fd = turnLUR cd
                           fcfd (c,d) = (fc c,fd d)
                           csds' = map fcfd csds

swap1strows :: [[a]] -> [[a]]
swap1strows (a:b:cs) = b:a:cs
swap1strows (a:[]) = [a]
swap1strows [] = []


rows2 :: [a] -> [a] -> [a]
rows2 [] _ = []
rows2 _ [] = []
rows2 (a:aa) (b:bb) = a:b:rows2 aa bb


turnLUR :: Transformable a => (Corner, Direction) -> a -> a
turnLUR (LU,R) = id
turnLUR (LU,D) = transpose
turnLUR (RU,L) = lrflip
turnLUR (RU,D) = lrot
turnLUR (LD,R) = udflip
turnLUR (LD,U) = rrot
turnLUR (RD,L) = rrot . rrot
turnLUR (RD,U) = lrflip . lrot
turnLUR (_,_)  = undefined

rrotMatrix :: [[a]] -> [[a]]
rrotMatrix = lrotMatrix . udFlip . lrFlip

lrotMatrix :: [[a]] -> [[a]]
lrotMatrix xs = _lrotMatrix ([],xs)

_lrotMatrix :: ([[a]],[[a]]) -> [[a]]
_lrotMatrix (acc, []) = acc
_lrotMatrix (acc, ys) = _lrotMatrix (h:acc,t)
                     where (h,t)= htMatrix ys

htMatrix :: [[a]] -> ([a],[[a]])
htMatrix xx = foldr ht ([],[]) xx
            where ht :: [a] -> ([a],[[a]]) -> ([a],[[a]])
                  ht [] _ = error "there is no head of empty"
                  ht (x:[]) (hs,_) = (x:hs, [])
                  ht (x:xs) (hs, ts) = (x:hs, xs:ts)

transposeMatrix :: [[a]] -> [[a]]
transposeMatrix [] = []
transposeMatrix ys = h : transposeMatrix t
                  where (h, t) = htMatrix ys

{-     rrotMatrix   rrotMatrix  rrotMatrix  rrotMatrix
  [[1,2,3]    [[7,4,1]    [[9,8,7]     [[3,6,9]    [[1,2,3]
  ,[4,5,6]    ,[8,5,2]    ,[6,5,4]     ,[2,5,8]    ,[4,5,6]
  ,[7,8,9]]   ,[9,6,3]]   ,[3,2,1]]    ,[1,4,7]]   ,[7,8,9]]
-}

{-      transpose
  [[1,2,3]    [[1,4,7]
  ,[4,5,6]    ,[2,5,8]
  ,[7,8,9]]   ,[3,6,9]]
-}

{-    lrotMatrix  - lrotMatrix - lrotMatrix - lrotMatrix
  [[ 1, 2, 3]  [[3,6,9,12]   [[12,11,10]  [[10,7,4,1]  [[ 1, 2, 3]
  ,[ 4, 5, 6]  ,[2,5,8,11]   ,[ 9, 8, 7]  ,[11,8,5,2]  ,[ 4, 5, 6]
  ,[ 7, 8, 9]  ,[1,4,7,10]]  ,[ 6, 5, 4]  ,[12,9,6,3]] ,[ 7, 8, 9]
  ,[10,11,12]]               ,[ 3, 2, 1]]              ,[10,11,12]]
-}

lrFlip :: [[a]] -> [[a]]
lrFlip = map reverse

udFlip :: [[a]] -> [[a]]
udFlip = reverse

zigzag :: [(Corner,Direction)]
zigzag = genHarvestPath LU (cycle [R,L])
