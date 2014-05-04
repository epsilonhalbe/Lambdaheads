Haskell
=======

History
-------

 - 1936 - Allan Turing erfindet die "Turing Machine"

 - 1936 - Alonzo Church veröffentlicht eine äquivalente Theorie das sogenannte
   *lambda calculus*

 - 1958 - John McCarthy erschafft LISP, eine Programmiersprache basierend auf
   ebendiesem *lambda calculus*

 - 1970er - Robin Milner erfindet ML, eine noch striktere funktionale
   Programmiersprache (automatisiertes Beweisen)

 - 70/80 - viele andere  - erfinden SASL, KRC, NPL, Hope, Lazy ML, Clean und
   Miranda

 - 1990 - Das Haskell Commitee veröffentlicht Haskell 1.0 (nach 3 Jahren
   arbeit); C++, Java, Python, Perl und Ruby werden erfunden oder erfreuen sich
   immer mehr an Bedeutung

 - heute - viele "alte" Sprachen sind zu Recht oder auch nicht vergessen LISP
   feiert sein Comeback auf diversen Systemen (z.B. Clojure) Python, C++, Perl
   und Ruby gibt es noch immer, Java ist nicht tot zu kriegen ;-), Haskell hat
   auch so seine Nische und ist meiner Meinung zu unrecht nicht richtig populär.

Wer verwendet heutzutage Haskell?
---------------------------------

Haskell wird eher im Backend eingesetzt. Die Bank "Standard Chartered" ist
dafür bekannt Haskell einzusetzen, Namen die dort auftauchen sind Lennart
Augustsson oder Don Stewart,der zuvor bei "Galois Inc." gearbeitet hat. Simon
Marlowe hat kürzlich zu Facebook gewechselt, die schon Bryan O'Sullivan
beschäftigen. Natürlich Simon Marlowes vorhergehender Arbeitgeber Microsoft
forscht rege an Haskell und hat Simon Peyton Jones als Researcher. Desweiteren
kann ich noch FPComplete und Well-Typed nennen.

In Österreich gibt es dieses Ticketing System der Öbb, das in Haskell
geschrieben ist wird mit Sommer 2014 durch irgendwas ersetzt, keine Ahnung durch
was.

Was brauche ich zum herumspielen?
---------------------------------

- Einen funktionierenden Compiler/Interpreter am besten Haskell-Platform
[herunterladen](http://www.haskell.org) und die Intstallationsinstruktionen befolgen.
  + Compiler gibt es meherer der mit Abstand populärste ist GHC
  + Interpreter gibt es auch einige aber außer GHCi sind alle veraltet

- Einen Editor
  + Vim + Plugins (Syntastic, ghc-mod, haskellmode, hdevtools, lushtags)
  + Emacs + Plugins (ghc-mod)
  + Eclipse + EclipseFP
  + FPComplete hat einen online editor

- Ein oder Zwei Lernressourcen
  + [Learn you a haskell for great good](http://learnyouahaskell.com) (short lyah)
  + [Real World Haskell](http://book.realworldhaskell.org/read/)
  + [Parallel and Concurrent Programming in Haskell](http://chimera.labs.oreilly.com/books/1230000000929/index.html)
  + [Haskell and Yesod](http://www.yesodweb.com/)

- Blogs, Podcasts & Co
  + [Planet Haskell](http://planet.haskell.org/) eine Sammlung von aktuellen
  Blogartikeln
  + [Reddit](http://www.reddit.com/r/haskell)
  + [Haskellcast](http://www.haskellcast.com/)
  + haskell-cafe - mailinglist
  + [Lambdaheads](https://metalab.at/wiki/Lambdaheads)

- Sonstige Tools
  + HLint - ein ausgezeichnetes Tool das Codestyle verbessert
  + Cabal - der Paketmanager von Haskell
  + Hoogle - eine Suchmaschine die es ermöglicht nach Funktionen zu suchen
  + Hayoo - noch eine Suchmaschine, die noch mehr Pakete durchsucht
  + [www.stackoverflow.com](http://stackoverflow.com/questions/tagged/haskell)

Wer gerne Bücher kauft: Bitte fördert lokale Händler!

Haskell hat so einige Buzzwords die sich um die Sprache ranken und ein paar
davon möchte ich kurz erklären.

Haskell ist Funktional
----------------------

Funktionen sind ganz normale Bürger wie alles andere auch, man kann sie in
Variablen speichern, in Listen packen, auswerten und auch als Parameter in
anderen Funktionen verwenden.

**Java**

    Object a = Math.pow /* geht nicht */

**Python**

    a = min
    a(1,3) => 1
    b = lambda x: x*2
    b(1) => 2
    c = lambda x y: x*y
    c(2,3) => 6

**Haskell**

    let a = (+)
    a 1 2 => 3
    let b = \x -> x*2
    b 1 => 2
    let c = \x y -> x*y
    c 2 3  => 6

der Backslash wurde aus visueller analogie gewählt - `\` schaut fast aus wie `λ`

**Perl**

    $a = \&min
    $b = sub{ $_*2}
    $a -> (1,2)

und dieses lambda ist genau das, das Alonzo Church in seinem *lambda calculus*
geprägt hat.

Stark & Statisch Typisiert + Immutable
--------------------------------------

**C**

    char a = 'a'; a+=3;printf("%s\n",&a);

**Python**

    a = 3; a = "foo" # stark aber dynamisch

**Haskell**

    let a = 3; let a = irgendwas -- error

Jetzt muss ich zugeben, dass dies **NICHT** im Interpreter funktioniert, da kann
man Variablen nach belieben neu belegen!

Ähm warum denn das - dynamisch und schwach ist doch viel produktiver und toller!
Dazu ein Beispiel in C.

**C**

    #include "stdio.h"
    int main(int argc, char *argv[]) {
        char x = 'a';
        x +=19;
        printf("Hello Segfaul%s\n",x);
        return 0; }

In **Haskell** gibt es bis auf Compilerfehler keine Segfaults

Typsicherheit ist langfristig ein nicht zu vernachlässigendes Asset andere
Programmiersprachen behelfen sich mit Tests und kommen auch weit, für mich ist
das Typkorsett eindeutig mehr unterstützend und weniger hinderlich.

Haskell ist Lazy
----------------

Zeug wird erst dann ausgewertet wenn es gebraucht wird, kennt man bereits aus
anderen Sprachen (in form von "short-circuit-operators"), aber weit nicht so
konsequent durchgezogen

**Java**

    bool a = true || (1/0 > 0);

**Haskell**: wir starten den Interpreter :`> ghci`

    λ> let a = 3
    λ> :sprint a
    a = 3
    λ> let b = [1..10]
    λ> :sprint b
    b = _
    λ> let c = map (*2) b
    λ> :sprint c
    c = _
    λ> length c
    λ> :sprint b
    b = [1,2,3,4,5,6,7,8,9,10]
    λ> :sprint c
    c = [_,_,_,_,_,_,_,_,_,_]

    λ> let fib = 1:1:zipWith (+) fib (tail fib)
    λ> take 10 fib
    [1,1,2,3,5,8,13,21,34,55]
    λ> let ziplist = zipWith (,) [1..9] ['a'..'z']
    λ> ziplist
    [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e'),(6,'f'),(7,'g'),(8,'h'),(9,'i')]

so und wie schaut Haskell code aus ??


    module HaskellTalk where

    myMax :: (Ord a) => a -> a -> a
    myMax a b = if a > b then a else b

    myFac :: Int -> Int
    myFac n | n < 0     = error "Keine negativen Argumente erlaubt"
            | n == 0    = 1
            | otherwise = n * myFac (n-1)

    myIndex :: Int -> [a] -> a
    myIndex _ []     = error "Error: Liste ist kuerzer als der gesuchte Index"
    myIndex 0 (x:_)  = x
    myIndex n (_:xs) = myIndex (n-1) xs

ist okay aber macht Probleme - denn in Haskell gibt es (tusch) unendliche Listen
z.B. `myIndex (-1) [1..]` führt zu warmer Luft und keinem Ergebnis.


    myIndex2 :: Int -> [a] -> a
    myIndex2 _ []    = error "Error: Liste ist kuerzer als der gesuchte Index"
    myIndex2 0 (x:_) = x
    myIndex2 n (_:xs) | n < 0 = error "Keine negativen Indizes erlaubt"
                      | True = myIndex (n-1) xs


