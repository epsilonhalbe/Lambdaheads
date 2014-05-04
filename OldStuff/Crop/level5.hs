import Crop

path1 = zip (cycle [LU,RU]) (cycle [R,L])
ex1 = stringify $ harvest2 (Crop (crMatrix 5 4)) path1

path2 = zip (cycle [LD,RU]) (cycle [R,L])
ex2 = stringify $ harvest2 (Crop (crMatrix 5 4)) path2

path3 = zip (cycle [RD,LD]) (cycle [L,R])
ex3 = stringify $ harvest (Crop (crMatrix 10 10)) path3

path4 = zip (cycle [RD,LU]) (cycle [R,L])
ex4 = stringify $ harvest2 (Crop (crMatrix 10 10)) path4

path5 = zip (cycle [LD,RU]) (cycle [U,D])
ex5 = stringify $ harvest2 (Crop (crMatrix 17 9)) path5

main = do
    putStrLn ex1
    putStrLn ex2
    putStrLn ex3
    putStrLn ex4
    putStrLn ex5
