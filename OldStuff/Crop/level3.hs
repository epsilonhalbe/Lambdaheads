import Crop.Crop

path1 = genHarvestPath LU (cycle [D,U])
ex1 = stringify $ harvest (Crop (crMatrix 3 4)) path1

path2 = genHarvestPath RU (cycle [D,U])
ex2 = stringify $ harvest (Crop (crMatrix 2 5)) path2

path3 = genHarvestPath RD (cycle [U,D])
ex3 = stringify $ harvest (Crop (crMatrix 5 2)) path3

path4 = genHarvestPath LD (cycle [U,D])
ex4 = stringify $ harvest (Crop (crMatrix 23 12)) path4

main = do
    putStrLn ex1
    putStrLn ex2
    putStrLn ex3
    putStrLn ex4
