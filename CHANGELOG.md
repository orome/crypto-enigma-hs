A list of key changes in [Hackage releases], along with planned features of some expected future releases (in
parentheses).

See also:

* the list of [code releases];
* [open milestones];
* [closed milestones]; and
* [issues].

### (0.1.1.3)

Current [development version]

### [0.1.1.2]

Latest [release version], including fixes to documentation and CLI display bugs.

### [0.1.1.1]

[Breaking API changes](https://github.com/orome/crypto-enigma-hs/compare/ef97f8ac..62d0ff59):

* Change to [handling errors with `Either`](http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Except.html#g:3)
  ([rather than](https://stackoverflow.com/a/53456278/656912) `Except`, eliminating the dependency on `mtl`).
* Removal of deprecated display functions from `Crypto.Enigma.Display`.

### [0.0.3.1]

Non-breaking API additions, adding [new CLI] with significant
[refactoring and additional features](https://github.com/orome/crypto-enigma-hs/compare/0.0.2.14...0.0.3.1):

* [Add](https://github.com/orome/crypto-enigma-hs/issues/13)
  [command line interface](https://github.com/orome/crypto-enigma-hs#functionality-command-line),
  using [optparse-applicative](http://hackage.haskell.org/package/optparse-applicative).
* Refactor display functions, deprecating `show...` functions and adding new display options.
* Encapsulate `EnigmaConfig` display options in
  [`DisplayOpts`](https://hackage.haskell.org/package/crypto-enigma/docs/Crypto-Enigma-Display.html#DisplayOptsA)
  with a total constructor (coercing any bad values to defaults).
* More consistent and robust error handling in `EnigmaConfig` constructors
  (including restoring `readSpec` compliance in `read`).
* Add (export existing) [total constructor](https://hackage.haskell.org/package/crypto-enigma/docs/Crypto-Enigma.html#v:configEnigmaExcept)
  for `EnigmaConfig`.

### [0.0.2.14]

* Update for GHC 8.6.1.
* Travis CI infrastructure and resolver updates.

### [0.0.2.13]

* Update for GHC 8.4.3.
* Travis CI infrastructure and resolver updates.

### [0.0.2.12]

* Update for base 4.11.1.0.

### [0.0.2.11]

* Update for base 4.11.0.0.

### [0.0.2.10]

* [Include Stackage LTS 10.1](https://www.stackage.org/lts-10.1)
* Minor workflow and tidying

### [0.0.2.9]

* [Submit to Stackage](https://github.com/orome/crypto-enigma-hs/issues/19).
* [Update for base 4.10.0.0.](https://github.com/orome/crypto-enigma-hs/issues/22)
* [Update for base 4.10.1.0.](https://github.com/orome/crypto-enigma-hs/issues/24)

### [0.0.2.8]

* Workflow changes.

### [0.0.2.7]

* Fix [testing (build) error](https://travis-ci.org/orome/crypto-enigma-hs/jobs/187207215).

### [0.0.2.6]

* Add [QuickCheck](https://hackage.haskell.org/package/QuickCheck) tests.
* Remove (disabled) assertions from
  [`configEnigma`](https://hackage.haskell.org/package/crypto-enigma/docs/Crypto-Enigma.html#v:configEnigma) and fail
  with an error when bad arguments are given.
* Convert all strings provided as `Message` arguments to valid machine input (see
  [`message`](https://hackage.haskell.org/package/crypto-enigma/docs/Crypto-Enigma.html#v:message)).

### [0.0.2.5]

* Test and abandon [Travis CI Hackage deployment](http://docs.travis-ci.com/user/deployment/hackage/).

### [0.0.2.4]

* Correct minor errors in project repo and CI links.

### [0.0.2.3]

* Minor tidying and organization.

### [0.0.2.2]

* Minor corrections to documentation.

### [0.0.2.1]

* Add [test figure] to documentation.
* Minor corrections to documentation.

### [0.0.2.0]

* Start testing module.
* Expose valid rotor and reflector names in API.

### [0.0.1.7]

* Fix Hackage uploading and versioning errors.

### [0.0.1.6]

* Fix Hackage uploading and versioning errors.

### [0.0.1.5]

* Fix readme formatting errors.

### [0.0.1.4]

* Some minor spelling corrections and notes.
* Added Documentation.
* Added README.
* Added CHANGELOG.
* Support for [build checks].

### [0.0.1.3]

Initial Hackage version. First upload of package to Hackage, without
([successful](https://hackage.haskell.org/package/crypto-enigma-0.0.1.3/reports/1)) Hackage-built documentation.
Stable enough for use, but not reviewed.

[Hackage releases]: https://hackage.haskell.org/package/crypto-enigma
[test figure]: https://hackage.haskell.org/package/crypto-enigma/docs/Crypto-Enigma-Display.html#showEnigmaConfigInternalFIG
[build checks]: https://travis-ci.org/orome/crypto-enigma-hs/branches
[code releases]: https://github.com/orome/crypto-enigma-hs/releases
[closed milestones]: https://github.com/orome/crypto-enigma-hs/milestones?state=closed
[open milestones]: https://github.com/orome/crypto-enigma-hs/milestones?state=open
[issues]: https://github.com/orome/crypto-enigma-hs/issues?utf8=✓&q=
[First stable release]: https://github.com/orome/crypto-enigma-hs/milestones/First%20Stable%20Release

[release version]: https://github.com/orome/crypto-enigma-hs/tree/hackage
[development version]: https://github.com/orome/crypto-enigma-hs/tree/develop
[new CLI]: https://github.com/orome/crypto-enigma-hs/tree/new/cli

[0.1.1.2]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.1.1.2
[0.1.1.1]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.1.1.1
[0.0.3.1]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.0.3.1
[0.0.2.14]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.0.2.14
[0.0.2.13]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.0.2.13
[0.0.2.12]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.0.2.12
[0.0.2.11]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.0.2.11
[0.0.2.10]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.0.2.10
[0.0.2.9]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.0.2.9
[0.0.2.8]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.0.2.8
[0.0.2.7]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.0.2.7
[0.0.2.6]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.0.2.6
[0.0.2.5]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.0.2.5
[0.0.2.4]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.0.2.4
[0.0.2.3]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.0.2.3
[0.0.2.2]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.0.2.2
[0.0.2.1]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.0.2.1
[0.0.2.0]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.0.2.0
[0.0.1.7]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.0.1.7
[0.0.1.6]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.0.1.6
[0.0.1.5]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.0.1.5
[0.0.1.4]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.0.1.4
[0.0.1.3]: https://github.com/orome/crypto-enigma-hs/releases/tag/0.0.1.3