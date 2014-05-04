# Here will be all of ε/2's code for lambdaheads meeting

## 2012-02-19: Crop:

from the [ccc2010](http://catcoder.catalyst.cc): the task to solve is to
get numbers of an array according to a path.
 * [instructions and level1](http://www.unet.univie.ac.at/~a0307893/downloads/description-level1.pdf)
 * [level2](http://www.unet.univie.ac.at/~a0307893/downloads/description-level2.pdf)
 * [level3](http://www.unet.univie.ac.at/~a0307893/downloads/description-level3.pdf)
 * [level4](http://www.unet.univie.ac.at/~a0307893/downloads/description-level4.pdf)
 * [level5](http://www.unet.univie.ac.at/~a0307893/downloads/description-level5.pdf)
I tried to solve this in Haskell - by generating a path and turning the array
and always collecting the first row.

## 99-haskell-problems
are just some finger exercises for not loosing grasp of haskell

## Coding Challenges
inspired by hop I want to introduce coding challenges to Lambdaheads

### Cryptoanalysis 101:
Inspired by a question I answered on stackoverflow.com:
One knows the easiest of cryptographic ciphers to break is the "Ceasar-Cipher"
it works just by transposition of letters. The way to break it is to analyze the
number of occurences of a letter in a text and compare it to a frequency table
of the guessed language the text is written in.
**write a function to decipher the following text by use of frequency analysis**

 * the following text is written in english:
<pre>
LIVITCSWPIYVEWHEVSRIQMXLEYVEOIEWHRXEXIPFEMVEWHKVSTYLXZIXLIKI
IXPIJVSZEYPERRGERIMWQLMGLMXQERIWGPSRIHMXQEREKIETXMJTPRGEVEKE
ITREWHEXXLEXXMZITWAWSQWXSWEXTVEPMRXRSJGSTVRIEYVIEXCVMUIMWERG
MIWXMJMGCSMWXSJOMIQXLIVIQIVIXQSVSTWHKPEGARCSXRWIEVSWIIBXVIZM
XFSJXLIKEGAEWHEPSWYSWIWIEVXLISXLIVXLIRGEPIRQIVIIBGIIHMWYPFLE
VHEWHYPSRRFQMXLEPPXLIECCIEVEWGISJKTVWMRLIHYSPHXLIQIMYLXSJXLI
MWRIGXQEROIVFVIZEVAEKPIEWHXEAMWYEPPXLMWYRMWXSGSWRMHIVEXMSWMG
STPHLEVHPFKPEZINTCMXIVJSVLMRSCMWMSWVIRCIGXMWYMX
</pre>
 * the frequency table for the english language is given by:
<pre>
['E', 'T', 'A', 'O', 'I', 'N', 'S', 'H', 'R', 'D', 'L', 'C', 'U'
,'M', 'W', 'F', 'G', 'Y', 'P', 'B', 'V', 'K', 'X', 'J', 'Q', 'Z']
</pre>
 * the result should be - as frequency analysis is not a real good tool:
<pre>
RENEMYSODEUNTOLTNSHEWIARTUNTKETOLHATAEDBTINTOLFNSMURAPEAREFE
EADEGNSPTUDTHHCTHEIO WRICRIAWTHEOCDSHELIAWTHTFETMAIGMDHCTNTF
TEMHTOLTAARTAAIPEMOVOSWOASOTAMNTDIHAHSGCSMNHETUNETAYNIJEIOTH
CIEOAIGICYSIOASGKIEWARENEWENEAWSNSMOLFDTCVHYSAHOETNSOEEXANEP
IABSGAREFTCVTOLTDSOUSOEOETNARESARENAREHCTDEHWENEEXCEELIOUDBR
TNLTOLUDSHHBWIARTDDARETYYETNTOCESGFMNOIHRELUSDLAREWEIURASGAR
EIOHECAWTHKENBNEPTNVTFDETOLATVIOUTDDARIOUHIOASCSOHILENTAISOI
CSMDLRTNLDBFDTPEQMYIAENGSNRIHSYIOISONEHYECAIOUIA
</pre>
* the real result is to be deciphered by rot13!
