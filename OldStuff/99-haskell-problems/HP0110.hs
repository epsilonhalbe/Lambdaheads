myLast :: [a] -> a
myLast [] = error "there is no last in the empty list"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "there is no last but one element in the empty list"
myButLast (_:[]) = error "there is no last but one element in the singleton list"
myButLast (x:_:[]) = x
myButLast (_:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt [] _ = undefined
elementAt (x:xs) n | n == 0    = x
                   | n < 0     = undefined
                   | otherwise = elementAt xs (n-1)

myLengh :: [a] -> Int
myLengh = sum . (map (const 1))

myReverse :: [a] -> [a]
myReverse xs = fst $ _myReverse ([],xs)
             where _myReverse :: ([a],[a]) -> ([a],[a])
                   _myReverse (rev,[]) = (rev,[])
                   _myReverse (rev,t:ts) = _myReverse (t:rev,ts)

isPalindrome ::(Eq a) => [a] -> Bool
isPalindrome xs = xs == (myReverse xs)

data List a = Elem a | List [List a]


myFlatten :: List a -> [a]
myFlatten (Elem x)  = [x]
myFlatten (List []) = []
myFlatten (List (x:xs)) = (myFlatten x) ++ (myFlatten (List xs))

myCompress :: (Eq a) => [a] -> [a]
myCompress [] = []
myCompress [x] = [x]
myCompress xs = _myCompress ([],[],xs)
      where _myCompress ::(Eq a) => ([a],[a],[a]) -> [a]
            _myCompress (cmprss,c,[]) = cmprss
            _myCompress (cmprss,c,t:ts) | [t]==c =_myCompress (cmprss,c,ts)
                                        | otherwise = _myCompress (cmprss++[t],[t],ts)

myPack :: (Eq a) => [a] -> [[a]]
myPack [] = [[]]
myPack [x] = [[x]]
myPack xs = _myPack ([],[],xs)
      where _myPack ::(Eq a) => ([[a]],[a],[a]) -> [[a]]
            _myPack (pck,c,[]) = myReverse pck
            _myPack (p:pck,c,t:ts) | [t]==c =_myPack ((t:p):pck,c,ts)
                                   | otherwise = _myPack ([t]:p:pck,[t],ts)

myEncode :: (Eq a) => [a] -> [(Int,a)]
myEncode [] = []
myEncode xs = map (\x -> (length x, head x)) (myPack xs)
