-- from http://stackoverflow.com/questions/11144393
import Prelude hiding (lookup)
import Data.Char ( isAlpha
                 , toUpper)
import Data.List ( group
                 , sort
                 , sortBy)
import Data.Function (on)
import Data.Map ( fromList
                , lookup
                , Map)
import Data.Maybe (mapMaybe)

codedMsg :: String
{-
codedMsg    = "V'Z GELVAT GB GRNPU GUR PNIRZRA GB CYNL FPENOOYR." ++
              "VG'F HCUVYY JBEX. GUR BAYL JBEQ GURL XABJ VF 'HAU'," ++
              "NAQ GURL QBA'G XABJ UBJ GB FCRYY VG."
-}
codedMsg = "LIVITCSWPIYVEWHEVSRIQMXLEYVEOIEWHRXEXIPFEMVEWHKVSTYLXZIXLIKIIXPIJVS"
        ++ "ZEYPERRGERIMWQLMGLMXQERIWGPSRIHMXQEREKIETXMJTPRGEVEKEITREWHEXXLEXXM"
        ++ "ZITWAWSQWXSWEXTVEPMRXRSJGSTVRIEYVIEXCVMUIMWERGMIWXMJMGCSMWXSJOMIQXL"
        ++ "IVIQIVIXQSVSTWHKPEGARCSXRWIEVSWIIBXVIZMXFSJXLIKEGAEWHEPSWYSWIWIEVXL"
        ++ "ISXLIVXLIRGEPIRQIVIIBGIIHMWYPFLEVHEWHYPSRRFQMXLEPPXLIECCIEVEWGISJKT"
        ++ "VWMRLIHYSPHXLIQIMYLXSJXLIMWRIGXQEROIVFVIZEVAEKPIEWHXEAMWYEPPXLMWYRM"
        ++ "WXSGSWRMHIVEXMSWMGSTPHLEVHPFKPEZINTCMXIVJSVLMRSCMWMSWVIRCIGXMWYMX"

freqTable_EN :: [Char]
freqTable_EN = ['E', 'T', 'A', 'O', 'I', 'N', 'S', 'H', 'R', 'D', 'L', 'C', 'U'
               ,'M', 'W', 'F', 'G', 'Y', 'P', 'B', 'V', 'K', 'X', 'J', 'Q', 'Z']

-- weed out non alphabetical characters from the list
filterAlpha :: String -> String
filterAlpha = filter isAlpha

-- sort the list by length
sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy (compare `on` length)

-- sort the list into most frequent character first
sortByFreq :: [[a]] -> [[a]]
sortByFreq = reverse . sortByLength

-- change the list into one instance of each character
reduceGroups :: [[a]] -> [a]
reduceGroups = map head

-- Pairing letters
pairs :: [(Char, Char)]
pairs = nonAlphaPairs ++ zip freqSortedMsg freqTable_EN
  where cleanedMsg    = (filterAlpha . map toUpper) codedMsg
        freqSortedMsg = (reduceGroups . sortByFreq . group . sort) cleanedMsg
        nonAlphaPairs = map (\x ->(x,x)) $ filter (not . isAlpha) codedMsg

-- and creating a map of tuples containing frequency related characters
cipher :: Map Char Char
cipher = fromList pairs

-- replace coded list with analyzed list
decipher :: String -> String
decipher = mapMaybe (uplook cipher)
         where uplook = flip lookup

result :: String
result = decipher codedMsg

main :: IO ()
main = print result
