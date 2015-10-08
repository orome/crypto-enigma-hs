## crypto-enigma

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)](https://www.haskell.org)
[![Hackage](https://img.shields.io/hackage/v/crypto-enigma.svg)](https://hackage.haskell.org/package/crypto-enigma)
![Hackage Dependencies](https://img.shields.io/hackage-deps/v/crypto-enigma.svg)
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://github.com/orome/crypto-enigma/blob/hackage/LICENSE)
[![Build Status](https://travis-ci.org/orome/crypto-enigma.svg?branch=hackage)](https://travis-ci.org/orome/crypto-enigma/branches)

An Enigma machine simulator with state and encoding display.

Currently support is only provided for those [machine models] in most widespread general use during the war years:
the [I], [M3], and [M4].

This is adapted, as an exerecise in learning Haskell, from an earlier learning project written in Mathematica.
It is my first Haskell program.

Note that the correct display of some characters used to represent components
(thin Naval rotors) assumes support for Unicode, while some aspects of the display of machine state
depend on support for combining Unicode. This is a [known limitation](https://github.com/orome/crypto-enigma/issues/10)
that will be addressed in a future release.

Full [documentation] — for the latest [release version] — is available on Hackage.

For other Haskell Enigma machines see:

* [enigma-hs](https://github.com/kc1212/enigma-hs)
* [crypto-classical](https://github.com/fosskers/crypto-classical)
* [enigma.lhs](https://gist.github.com/erantapaa/f071bc3f58d017f9280a)
* [henigma](https://github.com/erantapaa/henigma)

---

### Development status

[![Build Status](https://travis-ci.org/orome/crypto-enigma.svg?branch=develop)](https://travis-ci.org/orome/crypto-enigma/branches)

I'm currently learning and experimenting with some Haskell language features and can't promise the
[development version] will work. More detail about planned releases and activities can be found the list of
scheduled [milestones] and in the list of [open issues].

[documentation]: https://hackage.haskell.org/package/crypto-enigma
[release version]: https://github.com/orome/crypto-enigma/tree/hackage
[development version]: https://github.com/orome/crypto-enigma/tree/develop
[milestones]: https://github.com/orome/crypto-enigma/milestones
[open issues]: https://github.com/orome/crypto-enigma/issues

[machine models]: http://www.cryptomuseum.com/crypto/enigma/tree.htm
[I]: http://www.cryptomuseum.com/crypto/enigma/i/index.htm
[M3]: http://www.cryptomuseum.com/crypto/enigma/m3/index.htm
[M4]: http://www.cryptomuseum.com/crypto/enigma/m4/index.htm