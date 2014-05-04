HaskellTalk part2
=================

Haskell kann man in `*.hs` oder `*.lhs` Files abegen, wobei `lhs` für Literate
Haskell steht und es ermöglicht Code und Kommentare zu mischen, code fängt dabei
mit `>` an und der Rest ist Kommentar.

```haskell
> module Main where
```

Haskell code wird in Modulen verwaltet - ein Modul per Datei
das Main modul muss eine main Funktion haben

```haskell
> import Data.Char (toUpper)
> import Data.List (transpose)
> import Data.List.Split (chunksOf)
> import Data.Maybe (catMaybes)
> import qualified Data.Map as M
```

Standardmäßig ist immer `Prelude` die Standardbibliothek geladen.
Andere Module oder Bibliotheken werden importiert - um Namenskonflikte zu
vermeiden muss man manche Bibliotheken "qualifiziert" einbinden, und kann sie
auch mit `as` eine Abkürzung einführen.

```haskell
> main :: IO ()
```

Die main Funktion muss immer diese komische typsignatur haben `IO ()` entspricht
dem `void` in anderen Sprachen.

Exkurs - was soll unser Programm machen?

1. Eine Datei der folgenden Form einlesen

   `input.txt`
```
   4
   5
   Haskell
    #  ##   ## ##  ### ###  ## # # ###  ## # # #   # # ###  #  ##   #  ##   ## ### # # # # # # # # # # ### ###
   # # # # #   # # #   #   #   # #  #    # # # #   ### # # # # # # # # # # #    #  # # # # # # # # # #   #   #
   ### ##  #   # # ##  ##  # # ###  #    # ##  #   ### # # # # ##  # # ##   #   #  # # # # ###  #   #   #   ##
   # # # # #   # # #   #   # # # #  #  # # # # #   # # # # # # #    ## # #   #  #  # # # # ### # #  #  #
   # # ##   ## ##  ### #    ## # # ###  #  # # ### # # # #  #  #     # # # ##   #  ###  #  # # # #  #  ###  #
```

2. Das folgende Ausgeben
```
    # #  #   ## # # ### #   #
    # # # # #   # # #   #   #
    ### ###  #  ##  ##  #   #
    # # # #   # # # #   #   #
    # # # # ##  # # ### ### ###
```

```haskell
> main = do w' <- getLine
>           let w = (read w') :: Int
>           _ <- getLine -- _ ist die ist-mir-egal-Variable
>           wrd <- getLine :: IO String
>           alphaString <- getContents :: IO String
```
Dabei sieht `alphaString` so aus (wobei die `|` zur besseren Lesbarkeit
eingefügt wurden).

`alphaString = "  #  ##   ##| # # # # #  | ### ##  #  | # # # # #  | # # ##   ## \n"`
```haskell
>           let alphaLines = lines alphaString
```
    [" #  ##   ## "
    ,"# # # # #   "
    ,"### ##  #   "
    ,"# # # # #   "
    ,"# # ##   ## "]
```haskell
>           let alphaChunks = map (chunksOf w) alphaLines
```
    [[" #  ","##  "," ## "],
     ["# # ","# # ","#   "],
     ["### ","##  ","#   "],
     ["# # ","# # ","#   "],
     ["# # ","##  "," ## "]]
```haskell
>           let alphabet = transpose alphaChunks
```
    [[" #  ",
      "# # ",
      "### ",
      "# # ",
      "# # "],
     ["##  ",
      "# # ",
      "##  ",
      "# # ",
      "##  "],
     [" ## ",
      "#   ",
      "#   ",
      "#   ",
      " ## "]]
```haskell
>           let translate = M.fromList $ zip "ABCDEFGHIJKLMNOPQRSTUVWXYZ?" alphabet
```
`translate :: M.Map Char [String]` Bastelt ein Dictionary/Map

       " #  "      "##  "      " ## "
       "# # "      "# # "      "#   "
    A →"### ", B → "##  ", C → "#   " ...
       "# # "      "# # "      "#   "
       "# # "      "##  "      " ## "

```haskell
>               output = transpose . catMaybes $ map ((`M.lookup` translate) . subst) wrd
```

Hier passiert noch ein wenig haskell-magic(tm)

```haskell
>           putStr $ (unlines . map concat) output
```

`unlines` & `map concat` basteln die Strings noch zusammen und `putStr` schreibt das ganze wieder auf den
Bildschirm.

```haskell
> subst :: Char -> Char
> subst x | x ∈ ['A'..'Z'] = x
>         | x ∈ ['a'..'z'] = toUpper x
>         | otherwise      = '?'

(∈) x ['a'..'z']
x `elem` ['A'..'Z']
> -- helper stuff
> (∈) :: (Eq a) => a -> [a] -> Bool
> (∈) = elem
```
