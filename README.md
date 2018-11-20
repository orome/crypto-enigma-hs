## crypto-enigma

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)](https://www.haskell.org)
[![Hackage](https://img.shields.io/hackage/v/crypto-enigma.svg)](https://hackage.haskell.org/package/crypto-enigma)
[![Stackage](https://www.stackage.org/package/crypto-enigma/badge/lts?label=lts)](https://www.stackage.org/lts/package/crypto-enigma)
![Hackage Dependencies](https://img.shields.io/hackage-deps/v/crypto-enigma.svg)
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://github.com/orome/crypto-enigma-hs/blob/hackage/LICENSE)
[![Build Status](https://travis-ci.org/orome/crypto-enigma-hs.svg?branch=hackage)](https://travis-ci.org/orome/crypto-enigma-hs/branches)
[![Gitter](https://img.shields.io/gitter/room/badges/shields.svg)](https://gitter.im/orome/crypto-enigma-hs)

An Enigma machine simulator with state and encoding display.

Currently support is only provided for those [machine models] in most widespread general use during the war years: the
[I], [M3], and [M4].

This is adapted, as an exercise in learning Haskell, from an earlier learning project written in Mathematica. It is my
first Haskell program. A [Python version] with substantially the same API, plus a command line interface, is also
available.

### Functionality: package API

Perform [message encoding]:

    >>> enigmaEncoding (configEnigma "b-γ-V-VIII-II" "LFAP" "UX.MO.KZ.AY.EF.PL" "03.17.04.11") "KRIEG"
    "GOWNW"

    >>> let cfg = configEnigma "c-β-V-VI-VIII" "CDTJ" "AE.BF.CM.DQ.HU.JN.LX.PR.SZ.VW" "05.16.05.12"
    >>> putStr $ showEnigmaEncoding cfg "FOLGENDES IST SOFORT BEKANNTZUGEBEN"
    RBBF PMHP HGCZ XTDY GAHG UFXG EWKB LKGJ

Show [configuration details]:

    >>> let cfg = configEnigma "b-γ-V-VIII-II" "LFAP" "UX.MO.KZ.AY.EF.PL" "03.17.04.11"
    >>> putStr $ showEnigmaConfigInternal cfg 'K'
    K > ABCDEFGHIJK̲̅LMNOPQRSTUVWXYZ
      P YBCDFEGHIJZ̲̅PONMLQRSTXVWUAK         UX.MO.KZ.AY.EF.PL
      1 LORVFBQNGWKATHJSZPIYUDXEMC̲̅  Q  07  II
      2 BJY̲̅INTKWOARFEMVSGCUDPHZQLX  A  24  VIII
      3 ILHXUBZQPNVGKMCRTEJFADOYS̲̅W  F  16  V
      4 YDSKZPTNCHGQOMXAUWJ̲̅FBRELVI  L  10  γ
      R ENKQAUYWJI̲̅COPBLMDXZVFTHRGS         b
      4 PUIBWTKJZ̲̅SDXNHMFLVCGQYROAE         γ
      3 UFOVRTLCASMBNJWIHPYQEKZDXG̲̅         V
      2 JARTMLQ̲̅VDBGYNEIUXKPFSOHZCW         VIII
      1 LFZVXEINSOKAYHBRG̲̅CPMUDJWTQ         II
      P YBCDFEG̲̅HIJZPONMLQRSTXVWUAK         UX.MO.KZ.AY.EF.PL
    G < CMAWFEKLNVG̲̅HBIUYTXZQOJDRPS

Simulate [machine operation]:

    >>> let cfg = configEnigma "b-γ-V-VIII-II" "LFAP" "UX.MO.KZ.AY.EF.PL" "03.17.04.11"
    >>> putStr $ showEnigmaOperation cfg "KRIEG"
        OHNKJYSBTEDMLCARWPGIXZQUFV  LFAP  10 16 24 06
    K > CMAWFEKLNVG̲̅HBIUYTXZQOJDRPS  LFAQ  10 16 24 07
    R > HXETCUMASQNZGKRYJO̲̅IDFWVBPL  LFAR  10 16 24 08
    I > FGRJUABYW̲̅DZSXVQTOCLPENIMHK  LFAS  10 16 24 09
    E > SJWYN̲̅UZPQBVXRETHIMAOFKCLDG  LFAT  10 16 24 10
    G > EOKPAQW̲̅JLHCISTBDFVMNXRGUZY  LFAU  10 16 24 11

## Functionality: command line

A command line executable, `enigma` (accessible if installed on the path or through `stack exec -- enigma`) for local
Haskell installations, provides almost all the functionality of the API.

Encode messages:

    $ enigma encode "B-I-III-I EMO UX.MO.AY 13.04.11" "TESTINGXTESTINGUD"
    OZQKPFLPYZRPYTFVU

    $ enigma encode "B-I-III-I EMO UX.MO.AY 13.04.11" "OZQKPFLPYZRPYTFVU"
    TESTINGXTESTINGUD

Show configuration details (explained in more detail in the command line help):

    $ enigma show "B-I-III-I EMO UX.MO.AY 13.04.11" -l 'X' -H'()' -f internal
    X > ABCDEFGHIJKLMNOPQRSTUVW(X)YZ
      P YBCDEFGHIJKLONMPQRSTXVW(U)AZ         UX.MO.AY
      1 HCZMRVJPKSUDTQOLWEXN(Y)FAGIB  O  05  I
      2 KOMQEPVZNXRBDLJHFSUWYACT(G)I  M  10  III
      3 AXIQJZ(K)RMSUNTOLYDHVBWEGPFC  E  19  I
      R YRUHQSLDPX(N)GOKMIEBFZCWVJAT         B
      3 ATZQVYWRCEGOI(L)NXDHJMKSUBPF         I
      2 VLWMEQYPZOA(N)CIBFDKRXSGTJUH         III
      1 WZBLRVXAYGIPD(T)OHNEJMKFQSUC         I
      P YBCDEFGHIJKLONMPQRS(T)XVWUAZ         UX.MO.AY
    T < CNAUJVQSLEMIKBZRGPHXDFY(T)WO

Simulate machine operation (explained in more detail command line help):

    $ enigma run "B-I-III-I EMO UX.MO.AY 13.04.11" -m "TESTING" -t -H'()'
    0000       CNAUJVQSLEMIKBZRGPHXDFYTWO   EMO  19 10 05
    0001  T > UNXKGVERLYDIQBTWMHZ(O)AFPCJS  EMP  19 10 06
    0002  E > QTYJ(Z)XUPKDIMLSWHAVNBGROFCE  EMQ  19 10 07
    0003  S > DMXAPTRWKYINBLUESG(Q)FOZHCJV  ENR  19 11 08
    0004  T > IUSMHRPEAQTVDYWGJFC(K)BLOZNX  ENS  19 11 09
    0005  I > WMVXQRLS(P)YOGBTKIEFHNZCADJU  ENT  19 11 10
    0006  N > WKIQXNRSCVBOY(F)LUDGHZPJAEMT  ENU  19 11 11
    0007  G > RVPTWS(L)KYXHGNMQCOAFDZBEJIU  ENV  19 11 12

Watch the machine as it runs for 500 steps:

    $ enigma run "c-β-VIII-VII-VI QMLI UX.MO.AY 01.13.04.11" -s 500 -t -f internal -o

### Limitations

Note that the correct display of some characters used to represent components (thin Naval rotors) assumes support for
Unicode, while some aspects of the display of machine state depend on support for combining Unicode. This is a
[known limitation](https://github.com/orome/crypto-enigma-hs/issues/10) that will be addressed in a future release.

### Compatability

[Versions](https://www.stackage.org/package/crypto-enigma/snapshots) of this package have been part of [Stackage] LTS
Haskell since LTS 7.24, and the current version will work with LTS since 3.2.2. For information on which GHC versions
are supported by each release, see the
[package's Hackage Matrix](https://matrix.hackage.haskell.org/package/crypto-enigma).

### Documentation

Full [documentation] — for the latest [release version] — is available on Hackage.
[Documentation](https://www.stackage.org/haddock/lts/crypto-enigma/Crypto-Enigma.html) for the [current Stackage
LTS version](https://hackage.haskell.org/package/crypto-enigma) — generally identical to the latest release version —
is avalable on Stackage.

### Alternatives

For other Haskell Enigma machines see:

* [enigma-hs](https://github.com/kc1212/enigma-hs)
* [crypto-classical](https://github.com/fosskers/crypto-classical)
* [enigma.lhs](https://gist.github.com/erantapaa/f071bc3f58d017f9280a)
* [henigma](https://github.com/erantapaa/henigma)

This package served as the basis for a [Python version], with essentially the same API.

### Development status

[![Build Status](https://travis-ci.org/orome/crypto-enigma-hs.svg?branch=develop)](https://travis-ci.org/orome/crypto-enigma-hs/branches)

I'm currently learning and experimenting with some Haskell language features and can't promise the [development version]
will work. More detail about planned releases and activities can be found the list of scheduled [milestones] and in the
list of [open issues]. I may also be working on new major features on branches seperate from the development branch
(for example I've completed the [addition of a command line interface](https://github.com/orome/crypto-enigma-hs/issues/13)
on its [own branch](https://github.com/orome/crypto-enigma-hs/compare/develop...new/cli).)

[Python version]: https://pypi.python.org/pypi/crypto-enigma
[documentation]: https://hackage.haskell.org/package/crypto-enigma
[release version]: https://github.com/orome/crypto-enigma-hs/tree/hackage
[development version]: https://github.com/orome/crypto-enigma-hs/tree/develop
[milestones]: https://github.com/orome/crypto-enigma-hs/milestones
[open issues]: https://github.com/orome/crypto-enigma-hs/issues

[message encoding]: https://hackage.haskell.org/package/crypto-enigma/docs/Crypto-Enigma.html#v:enigmaEncoding
[configuration details]: https://hackage.haskell.org/package/crypto-enigma/docs/Crypto-Enigma-Display.html#v:showEnigmaConfigInternal
[machine operation]: https://hackage.haskell.org/package/crypto-enigma/docs/Crypto-Enigma-Display.html#v:showEnigmaOperation

[machine models]: http://www.cryptomuseum.com/crypto/enigma/tree.htm
[I]: http://www.cryptomuseum.com/crypto/enigma/i/index.htm
[M3]: http://www.cryptomuseum.com/crypto/enigma/m3/index.htm
[M4]: http://www.cryptomuseum.com/crypto/enigma/m4/index.htm

[Stackage]: https://www.stackage.org