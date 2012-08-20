module BWT where

import Data.List (sort
                 ,sortBy
                 ,tails
                 ,transpose)
import Data.Array (listArray
                  ,(!))

-- | the slow version of transform
{-transform :: Ord a ⇒ [a] → ([a],Int)-}
{-transform xs = (map last xss, position xs xss)-}
             {-where xss = sort $ rots xs-}
transform :: String → (String,Int)
transform xs = ([xa ! (pa ! i ) | i ← [0..(n-1)]], k )
             where n     = length xs
                   k     = length (takeWhile (≡ 0) ps)
                   xa    = listArray (0, n-1) (rrot xs)
                   pa    = listArray (0, n-1) ps
                   ps    = map snd (sort (zip (tails (tag xs))[0 .. n-1]))
                   tag x = x ++ "\EOT"

untransform :: Ord a ⇒ ([a],Int) → [a]
{-untransform (ys,k) = recreate n ys !! k-}
untransform (ys,k) = take n (tail (map (ya!)(iterate (pa!)k)))
                   where n  = length ys
                         ya = listArray (0,n-1) ys
                         pa = listArray (0,n-1) (map snd (sort (zip ys [0..])))

-- | recreate has several versions
recreate :: Ord a ⇒ Int → [a] → [[a]]
recreate 0 xs = map (const []) xs
{-recreate j xs = (headsort ∙ consCol ∙ fork (id , recreate (j-1))) xs-}
{-recreate j xs = (consCol ∙ fork (apply pp, apply pp ∙ recreate j)) xs-}
recreate j xs = (tp ∙ take j ∙ tail ∙ iterate (apply pp)) xs
where pp = p xs

apply :: [Int] → [a] → [a]
apply pp ys = [ys !! (pp !! i)|i ← [0..n']]
where n' = length ys - 1

{-recreate' :: Ord a ⇒ [a] → [[a]]-}
{-recreate' = undefined-}

{---------------------------------------------------------------------------
    -                           auxiliary functions                           -
        ---------------------------------------------------------------------------}

headsort :: Ord a ⇒ [[a]] → [[a]]
headsort = sortBy cmp
where cmp (x:_) (y:_) = compare x y
      cmp [] _ = error "no first component"
      cmp _ [] = error "no second component"

-- | calculates the position of a given element of a list - better use Maybe
position :: Eq a ⇒ a → [a] → Int
position x xs = length $ takeWhile (≠ x) xs

-- | takes the first j columns of a matrix ([[a]])
takeCols :: Int → [[a]] → [[a]]
takeCols j = map $ take j

-- | prepends a column to a matrix
consCol :: ([a],[[a]]) → [[a]]
consCol (xs,xss) = zipWith (:) xs xss

-- | generates all rotations of a given list
rots :: [a] → [[a]]
rots xs = take (length xs) (iterate lrot xs)

-- | puts the first list element to the last position
lrot :: [a] → [a]
lrot []     = []
lrot (x:xs) = xs ++ [x]

-- | puts the last list element to the first position
rrot :: [a] → [a]
rrot xs = last xs : init xs

fork :: (a → b, a → c) → a → (b, c)
fork (f,g) x = (f x, g x)

p :: (Ord a) ⇒ [a] → [Int]
p ys = map snd (sort $ zip ys [0..])

-- | transpose
tp :: [[a]] → [[a]]
tp = transpose
