%Thoroughly Testing in Haskell
%Martin Heuschober;
 [CC-BY-NC-SA 4.0][1]
%9. April 2014

<link rel="stylesheet" href="highlight.js/styles/solarized_light.css">
<script src="highlight.js/highlight.pack.js"></script>
<script>hljs.initHighlightingOnLoad();</script>

Test-Driven Development
=======================

Intro
-----

Test driven development ist das Prinzip Software so zu entwickeln, dass man erst
die Tests zu den Funktionen schreibt und erst dann dieselben implementiert.

Dies Sichert einerseits die Qualität und Funktionalität des Programms
andererseits, macht es Software wartbarer und spart erhebliche Zeit beim
Debuggen.

--------------------------------------------------------------------------------

Weiters passiert bei dieser Art zu entwickeln ein zweiter und meiner Meinung
nach wichtigerer Schritt, man beginnt in Tests zu denken bzw. schreibt auch
seine Funktionen so, dass sie leichter testbar sind.

Dadurch werden Funktionsblöcke oft kürzer und eine Funktion die vorher ein
zeilenlanges ungetüm war wird in mehrere Teilroutinen aufgedröselt.

Testkultur in Haskell
---------------------

Wenn man sich das Haskellland ansieht merkt man, dass die Priorität des Testens
schon von erster Stunde an mitgeplant war. So liefert die Compilerinfrastruktur
Tools wie `hpc` mit das Test-Coverage testet. Und gerade der Compiler selbst,
hat höchstwahrscheinlich hunderte Tests, die sicherstellen dass neue Änderungen
keine altbekannten Fehler zum Leben erwecken.

--------------------------------------------------------------------------------

Weiters hat sich durch das akademische Umfeld in dem sich Haskell entwickelt hat
auch eine Kultur des Beweisens verbreitet, so ist es nicht unüblich Teile des
Programms, von Hand oder computergestützt zu Beweisen. Ein Beispiel dafür wäre
der Window Manager `xmonad`, dessen Basis in der Programmiersprache `coq`
bewiesen ist.

--------------------------------------------------------------------------------

Eine weitere Besonderheit im Haskellland ist das Property based Testing.

Erfunden wurde das ganze während eines ICFP Contests - die Autoren brauchten
während ihrere Teilnahme ein ausgefeilteres Testing-Tool und haben zwar glaube
ich den Wettkampf nicht gewonnen, aber relativ zügig danach ein Paper
veröffentlicht in dem sie den Urvater `QuickCheck` präsentieren.

--------------------------------------------------------------------------------

Die erste Idee ist es, dass in vielen Problemstellungen das Schreiben von Tests
recht mühsam ist und gerade in einer stark&statisch typisierten Sprache, das
Input für diese Tests doch von einem Computer generierbar sein sollte.

Die zweite Idee ist, wenn in einer langen Liste ein Test-Error auftaucht, gibt
es meistens eine kürzere Liste in der der selbe Fehler auftaucht.

Ausgehend von diesem Paper finden sich mittlerweile zahlreiche Implementierungen,
heutzutage auch in dynamischen Programmiersprachen wie Erlang oder Clojure.

Tasty
=====

Hello my name is *Tasty*
------------------------

 - **Autor**: Roman Cheplyaka
 - **Features**:
    + Ist neben `HSpec` eines der populären Testing-Frameworks
    + Integriert HUnit, Small- und QuickCheck, sowie Golden HUnit Tests die FileIO benutzen
    + Roman Cheplyaka ist auch Autor des Pakets `SmallCheck`

Installation
------------

```bash
$> cabal install tasty
$> cabal install tasty-quickcheck
$> cabal install tasty-smallcheck
$> cabal install tasty-hunit
```

und los kanns gehen.

FruitShop.hs
------------

Als kleines Beispiel habe ich mir überlegt wäre ein stark vereinfachter Shop in
dem man Obst einkaufen kann, inspiration gab mir die Erlang meetup Guppe.

--------------------------------------------------------------------------------

Ich persönlich beginne gerne mit den Datentypen.

```haskell
module Data where

import Data.Word
import Test.SmallCheck.Series

data Fruit = Apple    | Blackberry | Coconut    | Date
           | Eggplant | F          | Grapefruit | H
           | I        | J          | Kiwi       | Lychee
           | Mango    | N          | Orange     | Pear
           | Quitte   | Raspberry  | Strawberry | Tomato
           | U        | V          | W          | X
           | Y        | Z
           deriving (Ord, Eq, Enum, Show)

type ShoppingList = [(Fruit,Word8)]
type Price = Double
type PriceList = [(Fruit,Price)]
```

--------------------------------------------------------------------------------

Und einem ungefähren Plan, wie meine Funkitonen heißen sollen und welche
Signatur sie haben.

```haskell
module FruitShop where

import Data
import Data.Word

priceList :: PriceList
priceList = zip [Apple .. Z] (cycle [1,2])

addFruit :: Fruit -> ShoppingList -> ShoppingList
addFruit = undefined

remFruit :: Fruit -> ShoppingList -> ShoppingList
remFruit = undefined
```

--------------------------------------------------------------------------------

```haskell
emptyShoppingList :: ShoppingList
emptyShoppingList = []

size :: ShoppingList -> Word8
size = undefined

totalPrice :: ShoppingList -> PriceList -> Price
totalPrice = undefined

(+>) :: (Fruit,Word8) -> [(Fruit,Word8)] -> [(Fruit,Word8)]
(+>) = undefined

(<+>):: [(Fruit,Word8)] -> [(Fruit,Word8)] -> [(Fruit,Word8)]
(<+>) = undefined
```

Tests.hs
--------

So und nun zu den Tests.

```haskell
module Test where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit
import HUnit.Utils

import FruitShop
import Data
import Data.List (permutations)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [scProps, unitTests, errorTests]
```

Unit Tests
----------

```haskell
unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "apple +> empty = [(Apple,1)]" $
      (Apple,1) +> emptyShoppingList @?= [(Apple,1)]
  --hunit
  ]
```

dabei ist `(@?=)` als `assertEquals` zu lesen.

Error Tests
-----------

Für die Error Tests muss man ein bischen basteln, aber mit Stackoverflow und
ein wenig Copy&Paste Code aus einem Bestehenden Hackage-Paket baut man sich
`(@!=)` mein Analogon zu `(@?=)` das assertRaises bedeutet.

*Und ja ich weiß in vielen Programmiersprachen ist `!=` als* ungleich *zu
lesen, das ist in Haskell aber `/=`*

--------------------------------------------------------------------------------

```haskell
errorTests :: TestTree
errorTests = testGroup "Unit tests for Exceptions"
  [ testCase "test error" $
      "exception" @!= (1 :: Int)
  --error
  ]
```

SmallCheck Tests
----------------

 - Setzt im Gegensatz zu QuickCheck auf weniger, dafür gezielte Tests
 - vereinfacht es Zufallsgeneratoren für eigene Datentypen zu schreiben,
   soll heißen mit entsprechenden Language-Extension wird das sogar
   automatisiert erledigt.

Wie sehen nun diese „vereinfachten” Generatoren aus?
----------------------------------------------------

```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

instance Serial IO Fruit where
  series = generate $ \d -> map fromDepthtoFruit [0..d]
         where fromDepthtoFruit = toEnum . id

instance Serial IO Word8 where
  series = generate $ \d -> map fromIntegral [0..d]

```

--------------------------------------------------------------------------------

```haskell
scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "length (remFruit shoplst) <= length shoplst" $
      \shoplst frt -> length (remFruit frt shoplst) <= length shoplst

  , SC.testProperty "size (remFruit shoplst) <= length shoplst" $
      \shoplst frt -> size (remFruit frt shoplst) <= size shoplst

  , SC.testProperty "add then rem == id" $ \lst frt ->
      (remFruit frt . addFruit frt) lst == id lst

  , SC.testProperty "size (lst1 <+> lst2) == size lst1 + size lst2" $
      \lst1 lst2 -> size (lst1 <+> lst2) == size lst1 + size lst2

  , SC.testProperty "commutivity of summation (Int)" $ \lst ->
      all (== sum (lst::[Int])) (map sum (permutations lst))

  , SC.testProperty "commutivity of summation (Double)" $ \lst ->
      all (== sum (lst::[Double])) (map sum (permutations lst))

  --smallcheck
  ]
```

QuickCheck Tests
----------------

Lasse ich aus weil sie ganz analog zu SmallCheck funktionieren


Output
------

<pre>
Tests
  Properties
    (checked by SmallCheck)
      length (remFruit shoplst) <= length shoplst:   <span style="color:red;">FAIL</span>
        <span style="color:red;">Exception: Prelude.undefined</span>
      size (remFruit shoplst) <= length shoplst:     <span style="color:red;">FAIL</span>
        <span style="color:red;">Exception: Prelude.undefined</span>
      add then rem == id:                            <span style="color:red;">FAIL</span>
        <span style="color:red;">Exception: Prelude.undefined</span>
      size (lst1 <+> lst2) == size lst1 + size lst2: <span style="color:red;">FAIL</span>
        <span style="color:red;">Exception: Prelude.undefined</span>
      commutivity of summation (Int):               <span style="color:green">OK</span>
        1333 tests completed
      commutivity of summation (Double):            <span style="color:green">OK</span>
        4815 tests completed
  Unit tests
    apple +> empty = [(Apple,1)]:                    <span style="color:red;">FAIL</span>
        <span style="color:red;">Exception: Prelude.undefined</span>
  Unit tests for Exceptions
    test error:                                      <span style="color:red;">FAIL</span>
      <span style="color:red;">Received no exception, but was expecting exception: exception</span>

6 out of 8 tests failed
</pre>


Criterion
=========

Intro
-----

Criterion ist ein Benchmarking tool, Autor: Bryan O'Sullivan
und ist meiner Meinung nach unerreicht!

Installation & Usage
--------------------
Installation funktioniert wie immer mit dem Paketmanager **cabal**

```bash
$> cabal update
$> cabal install criterion
```

ReverseInteger.hs
-----------------

```haskell
{-# LANGUAGE BangPatterns #-}

module ReverseInteger where

epsilon_fast :: Int-> Int
epsilon_fast n = aux n 0
               where aux :: Int -> Int -> Int
                     aux 0 !y = y
                     aux x !y = let (x',y') = x `quotRem` 10
                                 in aux x' (10*y+y')

epsilon_rInt :: Int-> Int
epsilon_rInt n = aux (n,0)
               where aux (0,y) = y
                     aux (x,y) = let (x',y') = x `quotRem` 10
                                 in aux (x',10*y+y')
```

--------------------------------------------------------------------------------

```haskell
epsilon_rInt' :: Int-> Int
epsilon_rInt' n = aux (n,0)
                where aux (0,y) = y
                      aux (x,y) = let (x',y') = x `quotRem` 10
                                      !z = 10*y+y'
                                  in aux (x',z)

fRaabe_Int :: Int -> Int
fRaabe_Int x | x < 0     = 0 - (read . reverse . tail . show $ x)
             | otherwise = read . reverse . show $ x

zeta_Int :: Int -> Int
zeta_Int x = (*) (signum x) . read . reverse . show . abs  $ x
```

Benchmarks.hs
-------------

```haskell
import Criterion.Main
import ReverseInteger

main :: IO ()
main =  defaultMain
  [bgroup "epsilon_fast" [ bench "fast1" $ whnf epsilon_fast 123456789 ,
                           bench "fast2" $ whnf epsilon_fast 987654321 ]
  ,bgroup "epsilon_rInt" [ bench "rInt1" $ whnf epsilon_rInt 123456789 ,
                           bench "rInt2" $ whnf epsilon_rInt 987654321 ]
  ,bgroup "epsilon_rInt'"[ bench "rInt'1" $ whnf epsilon_rInt' 123456789 ,
                           bench "rInt'2" $ whnf epsilon_rInt' 987654321 ]
  ,bgroup "fRaabe_Int"   [ bench "fRaabe1" $ whnf fRaabe_Int 123456789 ,
                           bench "fRaabe2" $ whnf fRaabe_Int 987654321 ]
  ,bgroup "zeta_Int"     [ bench "zeta1" $ whnf zeta_Int 123456789 ,
                           bench "zeta2" $ whnf zeta_Int 987654321 ]]
```

Usage
-----

Benutzen kann man das dann mit:

```bash
$> ghc -O2 Benchmark.hs
$> ./Benchmark -o Benchmark.html
```

Mit [Output](ReverseInteger.html)

Profiling
=========

Intro
-----

Profiling ist ebenfalls ein im Compiler eingebautes Feature, da das
Nachvollziehen des Codes durch 'lazy evaluation' leider erheblich erschwert
wird. Außerdem schafft man es traditionell sehr schnell Space Leaks oder Time
Leaks zu schaffen.

--------------------------------------------------------------------------------

Um nun ein Programm mit profiling informationen zu compilen fügt man in erster
Näherung einfach die Option `-fprof-auto` an.

```bash
$> ghc -O2 --make -prof -fprof-auto -rtsopts -fforce-recomp MyProgram.hs
$> ./MyProgram +RTS -hc -K100M
$> hp2ps MyProgram.hp
```

--------------------------------------------------------------------------------

und erhält dann ein Heap-Profile

![Profile][p1]

Mehr dazu
---------

[RWH Ch.25 Profiling and tuning for performance](http://book.realworldhaskell.org/read/profiling-and-optimization.html)


Equational reasoning
====================

Intro
-----

Equational reasoning ist eigentlich nichts anderes als Beweisen. Zwar manchmal
etwas mühsam aber kann durchaus zu schnelleren, aber bestimmt sichereren
Programmen führen.

Beispiel: MAP
-------------

**Behauptung:** Die Funktion `map: a -> b -> [a] -> [b]` die folgende Eigenschaft.

> map f . map g == map (f . g)

Beweis
------

Induktion über die länge der Liste:

**Basisfall:** `[]`

> (map f . map g) [] == [] == map (f . g)

--------------------------------------------------------------------------------

Angenommen wir wissen die Eigenschaft gilt für eine Liste von Länge (n-1),
dann zeigen wir das gilt für die ganze Liste.
<pre>
(map f . map g) (x:xs) == map f ( map g (x:xs))
                       == map f ((g x:map g xs))
                       == (f (g x):map f (map g xs))
                       == (f.g) x:map f (map g xs)
                       == (f.g) x:map (f . g) xs)
                       == map (f . g) (x:xs)
</pre>

Beispiel
--------

```haskell
twice :: [Int] -> [Int]
twice = map (*2)

thrice :: [Int] -> [Int]
thrice = map (*3)

(twice . thrice) == map (*2) . map (*3)
                 == map ((*2).(*3)) == map (*6)
```

Referenzen
==========

Tasty
-----

 - [hackage](http://hackage.haskell.org/package/tasty)
 - [Introduction](http://documentup.com/feuerbach/tasty)
 - [SmallCheck vs. QuickCheck](http://)

Criterion
---------

 - [Bryan O'Sullivan's blog](http://www.serpentine.com/blog/2009/09/29/criterion-a-new-benchmarking-library-for-haskell/)
 - [hackage](http://hackage.haskell.org/package/criterion)

Profiling
---------

 - [Stackoverflow Antwort von Don Stewart](http://stackoverflow.com/questions/3276240/tools-for-analyzing-performance-of-a-haskell-program/3276557#3276557)

Equational Reasoning
--------------------

 - Pearls of Functional Algorithm Design - R. Bird (support your local bookstore)
 - [Stream Fusion for Pipes - G.Gonzalez](http://www.haskellforall.com/2014/01/stream-fusion-for-pipes.html)(blog)
 - [Equational Reasoning - G.Gonzalez](http://www.haskellforall.com/2013/12/equational-reasoning.html)(blog)




[1]: http://creativecommons.org/licenses/by-nc-sa/4.0/
[p1]: Profiling1-1.jpg "test"

