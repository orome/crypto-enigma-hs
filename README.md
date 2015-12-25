## crypto-enigma

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)](https://www.haskell.org)
[![Hackage](https://img.shields.io/hackage/v/crypto-enigma.svg)](https://hackage.haskell.org/package/crypto-enigma)
![Hackage Dependencies](https://img.shields.io/hackage-deps/v/crypto-enigma.svg)
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://github.com/orome/crypto-enigma-hs/blob/hackage/LICENSE)
[![Build Status](https://travis-ci.org/orome/crypto-enigma-hs.svg?branch=hackage)](https://travis-ci.org/orome/crypto-enigma-hs/branches)

An Enigma machine simulator with state and encoding display.

Currently support is only provided for those [machine models] in most widespread general use during the war years:
the [I], [M3], and [M4].

This is adapted, as an exercise in learning Haskell, from an earlier learning project written in Mathematica.
It is my first Haskell program. A [Python version] with substantially the same API, plus a command line interface, is
also available.

### Functionality

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

### Limitations

Note that the correct display of some characters used to represent components
(thin Naval rotors) assumes support for Unicode, while some aspects of the display of machine state
depend on support for combining Unicode. This is a [known limitation](https://github.com/orome/crypto-enigma-hs/issues/10)
that will be addressed in a future release.

### Documentation

Full [documentation] — for the latest [release version] — is available on Hackage.

### Alternatives

For other Haskell Enigma machines see:

* [enigma-hs](https://github.com/kc1212/enigma-hs)
* [crypto-classical](https://github.com/fosskers/crypto-classical)
* [enigma.lhs](https://gist.github.com/erantapaa/f071bc3f58d017f9280a)
* [henigma](https://github.com/erantapaa/henigma)

This package served as the basis for a [Python version], with essentialy the same API.

### Development status

[![Build Status](https://travis-ci.org/orome/crypto-enigma-hs.svg?branch=develop)](https://travis-ci.org/orome/crypto-enigma-hs/branches)

I'm currently learning and experimenting with some Haskell language features and can't promise the
[development version] will work. More detail about planned releases and activities can be found the list of
scheduled [milestones] and in the list of [open issues].

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