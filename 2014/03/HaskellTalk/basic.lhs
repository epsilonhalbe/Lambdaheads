> module Main where

Haskell code wird in Modulen verwaltet - ein Modul per Datei
das Main modul muss eine main Funktion haben

> import Data.Char (toUpper)
> import Data.List (transpose)
> import Data.List.Split (chunksOf)
> import Data.Maybe (catMaybes)
> import qualified Data.Map as M

Standardmäßig ist immer `Prelude` die Standardbibliothek geladen.
Andere Module oder Bibliotheken werden importiert - um Namenskonflikte zu
vermeiden muss man manche Bibliotheken "qualifiziert" einbinden, und kann sie
auch mit `as` eine Abkürzung einführen.

> main :: IO ()

Die main Funktion muss immer diese komische typsignatur haben `IO ()` entspricht
dem `void` in anderen Sprachen.

Exkurs - was soll unser Programm machen?

1. Eine Datei der folgenden Form einlesen

  _input.txt_____________________________________________________________________________________________________
  | 4                                                                                                           |
  | 5                                                                                                           |
  | Haskell                                                                                                     |
  |  #  ##   ## ##  ### ###  ## # # ###  ## # # #   # # ###  #  ##   #  ##   ## ### # # # # # # # # # # ### ### |
  | # # # # #   # # #   #   #   # #  #    # # # #   ### # # # # # # # # # # #    #  # # # # # # # # # #   #   # |
  | ### ##  #   # # ##  ##  # # ###  #    # ##  #   ### # # # # ##  # # ##   #   #  # # # # ###  #   #   #   ## |
  | # # # # #   # # #   #   # # # #  #  # # # # #   # # # # # # #    ## # #   #  #  # # # # ### # #  #  #       |
  | # # ##   ## ##  ### #    ## # # ###  #  # # ### # # # #  #  #     # # # ##   #  ###  #  # # # #  #  ###  #  |
  |_____________________________________________________________________________________________________________|

2. Das folgende Ausgeben
  ______________________________
  |                             |
  | # #  #   ## # # ### #   #   |
  | # # # # #   # # #   #   #   |
  | ### ###  #  ##  ##  #   #   |
  | # # # #   # # # #   #   #   |
  | # # # # ##  # # ### ### ### |
  |_____________________________|

> main = do w' <- getLine
>           let w = (read w') :: Int
>           _ <- getLine -- _ ist die ist-mir-egal-Variable
>           wrd <- getLine :: IO String
>           alphaString <- getContents :: IO String
               #  ##   ##| # # # # #  | ### ##  #  | # # # # #  | # # ##   ## \n
>           let alphaLines = lines alphaString
            [ #  ##   ## ,
             # # # # #   ,
             ### ##  #   ,
             # # # # #   ,
             # # ##   ## ]
>           let alphaChunks = map (chunksOf w) alphaLines
           [[ #  ,##  , ## ],
            [# # ,# # ,#   ],
            [### ,##  ,#   ],
            [# # ,# # ,#   ],
            [# # ,##  , ## ]]
>           let alphabet = transpose alphaChunks
            [[ #  ,
              # # ,
              ### ,
              # # ,
              # # ],
             [##  ,
              # # ,
              ##  ,
              # # ,
              ##  ],
             [ ## ,
              #   ,
              #   ,
              #   ,
               ## ]]
>           let translate = M.fromList $ zip "ABCDEFGHIJKLMNOPQRSTUVWXYZ?" alphabet
translate :: M.Map Char [String]
Bastelt ein Dictionary/Map
|        #  |       ##  |        ## |
|       # # |       # # |       #   |
| A ->  ### | B ->  ##  | C ->  #   | ...
|       # # |       # # |       #   |
|       # # |       ##  |        ## |

>               output = transpose . catMaybes $ map ((`M.lookup` translate) . subst) wrd

Hier passiert noch ein wenig haskell-magic(tm)

>           putStr $ unlines $ map concat output

Bastelt die Strings noch zusammen und `putStr 

> subst :: Char -> Char
> subst x | x ∈ ['A'..'Z'] = x
>         | x ∈ ['a'..'z'] = toUpper x
>         | otherwise      = '?'

(∈) x ['a'..'z']
x `elem` ['A'..'Z']
> -- helper stuff
> (∈) :: (Eq a) => a -> [a] -> Bool
> (∈) = elem
