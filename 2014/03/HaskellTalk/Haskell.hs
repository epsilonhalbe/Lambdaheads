module Haskell where
import Data.Char (toUpper)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)
import qualified Data.Map as M

alpha :: String
alpha =  "  #  ##   ## ##  ### ###  ## # # ###  ## # # #   # # ###  #  ##   #  ##   ## ### # # # # # # # # # # ### ### \n"
      ++ " # # # # #   # # #   #   #   # #  #    # # # #   ### # # # # # # # # # # #    #  # # # # # # # # # #   #   # \n"
      ++ " ### ##  #   # # ##  ##  # # ###  #    # ##  #   ### # # # # ##  # # ##   #   #  # # # # ###  #   #   #   ## \n"
      ++ " # # # # #   # # #   #   # # # #  #  # # # # #   # # # # # # #    ## # #   #  #  # # # # ### # #  #  #       \n"
      ++ " # # ##   ## ##  ### #    ## # # ###  #  # # ### # # # #  #  #     # # # ##   #  ###  #  # # # #  #  ###  #  \n"

subst :: Char -> Char
subst x | x ∈ ['A'..'Z'] = x
        | x ∈ ['a'..'z'] = toUpper x
        | otherwise      = '?'

(∈) :: (Eq a) => a -> [a] -> Bool
(∈) = elem
