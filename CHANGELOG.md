A list of key changes in [Hackage releases],
along with planned features of some expected future releases (in parentheses).

See also:

* the list of [code releases];
* [open milestones];
* [closed milestones]; and
* [issues].

### (0.0.2.?)

* ([First stable release].)

### (0.0.2.9)

* (Unicode checking for messages and letters.)
* (Force characters into map range or catch indexing errors in encoding.)
* ([Submit to Stackage](https://github.com/orome/crypto-enigma-hs/issues/19).)
* (Further workflow changes: stack integration, Stackage, etc.)

### [0.0.2.8]

* Workflow changes.

### [0.0.2.7]

* Fix [testing (build) error](https://travis-ci.org/orome/crypto-enigma-hs/jobs/187207215).

### [0.0.2.6]

* Add [QuickCheck](https://hackage.haskell.org/package/QuickCheck) tests.
* Remove (disabled) assertions from [`configEnigma`](https://hackage.haskell.org/package/crypto-enigma/docs/Crypto-Enigma.html#v:configEnigma)
  and fail with an error when bad arguments are given.
* Convert all strings provided as `Message` arguments to valid machine input
  (see [`message`](https://hackage.haskell.org/package/crypto-enigma/docs/Crypto-Enigma.html#v:message)).

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

Initial Hackage version. First upload of package to Hackage,
without ([successful](https://hackage.haskell.org/package/crypto-enigma-0.0.1.3/reports/1)) Hacakge-built documentation.
Stable enough for use, but not reviewed.

[Hackage releases]: https://hackage.haskell.org/package/crypto-enigma
[test figure]: https://hackage.haskell.org/package/crypto-enigma/docs/Crypto-Enigma-Display.html#showEnigmaConfigInternalFIG
[build checks]: https://travis-ci.org/orome/crypto-enigma-hs/branches
[code releases]: https://github.com/orome/crypto-enigma-hs/releases
[closed milestones]: https://github.com/orome/crypto-enigma-hs/milestones?state=closed
[open milestones]: https://github.com/orome/crypto-enigma-hs/milestones?state=open
[issues]: https://github.com/orome/crypto-enigma-hs/issues?utf8=✓&q=
[First stable release]: https://github.com/orome/crypto-enigma-hs/milestones/First%20Stable%20Release
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