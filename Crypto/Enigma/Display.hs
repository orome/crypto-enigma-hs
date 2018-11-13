{-# OPTIONS_HADDOCK show-extensions #-}
{-|
Module      : Crypto.Enigma.Display
Description : Display of Enigma machine state and encoding
Copyright   : (c) 2014-2018 Roy Levien
License     : BSD3
Maintainer  : royl@aldaron.com
Stability   : experimental
Portability : POSIX

A module for rich display of the state of and encoding performed by Enigma machines defined in "Crypto.Enigma".
-}

{-# LANGUAGE Safe #-}
module Crypto.Enigma.Display (
        -- * Display options
        DisplayOpts,
        displayOpts,
        format,
        showencoding,
        markerspec,
        showsteps,
        steps,
        Format,
        MarkerSpec,
        DisplaySteps,
        -- * Configuration display
        displayEnigmaConfig,
        showEnigmaConfig,
        showEnigmaConfigInternal,
        -- * Operation display
        displayEnigmaOperation,
        listEnigmaOperation,
        showEnigmaOperation,
        showEnigmaOperationInternal,
        -- * Encoding display
        displayEnigmaEncoding,
        showEnigmaEncoding
) where

--import Control.Applicative
import Data.Monoid              ((<>))          -- For GHC < 8.4.3 - https://stackoverflow.com/a/53024485/656912
--import Data.Char
import Data.List
import Data.List.Split          (chunksOf)
import Text.Printf              (printf)
import Data.Char                (toLower)
import Crypto.Enigma.Utils
import Crypto.Enigma

-- REV - Final newline in show... functions is a bit inconsistent



-- Helpers ===================================================================


-- Message display -----------------------------------------------------------

-- TBD - Don't remove spaces (at least in showEnigmaOperation and instead put a blank line?)
-- Standard formatting of encoded messages
postproc :: String -> String
postproc = unlines . chunksOf 60 . unwords . chunksOf 4


-- Mapping markup -----------------------------------------------------------

-- TBD Move up (closer to encoding?)
-- TBD - Can't use below unless encode handles ch == ' '
-- locate the index of the encoding with m of ch, in s
locCar :: Char -> String -> Mapping -> Maybe Int
locCar ch s m = elemIndex (encode m ch) s

markedMapping :: Maybe Int -> Mapping -> MarkerFunc -> String
markedMapping (Just loc) e (MarkerFunc mf) = take loc <> mf.(!!loc) <> drop (loc + 1) $ e
markedMapping Nothing e _    = e


-- Character restriction ----------------------------------------------------

-- If the character isn't in 'letters', treat it as blank (a special case for 'encode' and other functions)
enigmaChar :: Char -> Char
enigmaChar ch = if ch `elem` letters then ch else ' '



-- Machine display ===========================================================


-- Display options -----------------------------------------------------------

{-|
A (<https://wiki.haskell.org/Type_synonym synonym> for) 'String', indicating that this option will be coerced to a valid
valid 'format' when used by display functions.
-}
type Format = String

-- The various possible formats
fmtsSingle_ = ["single", "summary"]
fmtsInternal_ = ["internal", "detailed", "schematic"]
fmtsWindows_ = ["windows", "winds"]
fmtsConfig_ = ["config", "configuration", "spec", "specification"]
fmtsEncoding_ = ["encoding"]
fmts_ = fmtsInternal_ ++ fmtsSingle_ ++ fmtsWindows_ ++ fmtsConfig_ ++ fmtsEncoding_
-- TBD: Debug


{-|
A (<https://wiki.haskell.org/Type_synonym synonym> for) 'String', indicating that this option will be coerced to a valid
valid 'markerspec' when used by display functions.
-}
type MarkerSpec = String

-- REV: Expose with markerFunc?
-- A function specifying how to highlight an encoded character in an 'EnigmaConfig', created using 'markerFunc'.
data MarkerFunc = MarkerFunc (Char -> String)

-- REV: Expose to support arbitrary user-defined functions?
-- REV: Version checks for character compatability w/ substitutions that work; force checks with a type? eg:
--      decorate ch = ['[',ch,']'] -- version that works when Unicode fails to display properly (e.g. IHaskell as of 0.7.1.0)
--      https://stackoverflow.com/a/33206814
-- Create a 'MarkerFunc' from a string specification.
-- Ignores unrecognized/invalid values
markerFunc :: String -> MarkerFunc
markerFunc spec = MarkerFunc (case spec of
                    "*" ->  \_ -> "*"
                    "lower" ->  \c -> [toLower c]
                    "omit" ->  \_ -> " "
                    "bars" -> \ch -> ch:"\818\773"
                    "legacy" -> \c -> [ '[', c, ']' ]
                    "red" ->  \c -> "\ESC[31;1m" ++ [c] ++ "\ESC[0m"
                    "blue" ->  \c -> "\ESC[34;1m" ++ [c] ++ "\ESC[0m"
                    "green" ->  \c -> "\ESC[32;1m" ++ [c] ++ "\ESC[0m"
                    "bold" ->  \c -> "\ESC[1m" ++ [c] ++ "\ESC[0m"
                    "underline" ->  \c -> "\ESC[4m" ++ [c] ++ "\ESC[0m"
                    "highlight" ->  \c -> "\ESC[7m" ++ [c] ++ "\ESC[0m"
                    "cyan" ->  \c -> "\ESC[36;1m" ++ [c] ++ "\ESC[0m"
                    "yellow" ->  \c -> "\ESC[33;1m" ++ [c] ++ "\ESC[0m"
--                     "51" ->  \c -> "\ESC[51m" ++ [c] ++ "\ESC[0m"
--                     "52" ->  \c -> "\ESC[52m" ++ [c] ++ "\ESC[0m"
                    -- TBD - Colors and other escapes
                    [l, r] -> \c -> [l, c, r]
                    _ -> \c -> [c])

{-|
A (<https://wiki.haskell.org/Type_synonym synonym> for) 'Int', indicating that this option will be coerced to a valid
valid 'steps' value when used by display functions.
-}
type DisplaySteps = Int

-- TBD: Add catalog of examples for choices here (not in display functions) showing choices; refer to display functions for details of presentation (as now) <<<
{-|
Options for 'displayEnigmaConfig', 'displayEnigmaOperation', and 'listEnigmaOperation', created using 'displayOpts'.
All fields are coerced to valid values by display functions.
-}
data DisplayOpts = DisplayOpts {
        {-|
        The 'Format' to use to display the 'EnigmaConfig': a 'String' specifying the format used to display machine
        configuration(s) that is one of @"single"@, @"internal"@, @"windows"@, @"config"@, and @"encoding"@.
        Display functions are forgiving about the supplied format and will accept a range of reasonable substitutes
        (e.g., @"detailed"@ or @"schematic"@ for @"internal"@), and will treat unrecognized formats as the default, @"single"@.
        -}
        format :: !Format,
        {-|
        A 'Bool' indicating whether to show encoding if not normally shown for the specified format.
        -}
        showencoding :: !Bool,
        {-|
        A 'String' that is either a pair or characters (e.g. @"[]"@) to use to highlight encoded characters encoding,
        or a specification of a color (e.g, @"red"@) or style (@"bars"@).
        Invalid or unrecognized higlight specifications are treated as the @"bars"@.
        -}
        markerspec :: !String,
        markerfunction_ :: !MarkerFunc,  -- REV: Internal only; expose for custom marker functions?
        {-|
        A 'Bool' indicating whether to show step numbers, defaults to @False@.
        Only relevant for <#v:displayEnigmaOperation operation display> functions.
        -}
        showsteps :: !Bool,
        {-|
        An 'Int' indicating the number of steps to display, which if omitted when a message is provided will default
        to the length of the message, and to @1@ otherwise. Values less than @1@ are treated as the default.
        Only relevant for <#v:displayEnigmaOperation operation display> functions.
        -}
        steps :: !DisplaySteps
}

allSteps_ = (-1)

{-|
Default 'DisplayOpts' equivalent to @DisplayOpts{format = "single", showencoding  = False, markerspec = "bars",...}@.
See individual options below for defaults values. This is the sole method for providing and setting options for
display functions. E.g. to supply specify a 'format' of @"windows"@ which shows encoding (see 'showencoding') provide

@
displayOpts{format="windows",showencoding=True}
@

as the @DisplayOpts@ argument to a display finction.
-}
displayOpts = DisplayOpts {
        format = "single",
        showencoding  = False,
        markerspec = "bars",
        markerfunction_ = markerFunc "bars",
        showsteps = False,
        steps = allSteps_
}

-- TBD: Confirm defaults here and above are same as command line / match docs <<<
-- Internal function used by all display functions to coerce all display options to valid values.
validOpts_ :: DisplayOpts -> DisplayOpts
validOpts_ opts = DisplayOpts {
                        format = case fmt of
                                  f | elem f fmts_ -> f
                                    | otherwise -> fmtsSingle_!!0,
                        showencoding = se,
                        markerspec = ms,
                        markerfunction_ = markerFunc ms,
                        showsteps = ss,
                        steps = if ns > 0 then ns else allSteps_
                        }
                        where
                                fmt = (format opts)
                                se = (showencoding opts)
                                ms = (markerspec opts)
                                ss = (showsteps opts)
                                ns = (steps opts)


-- Configuration display -----------------------------------------------------

displayEnigmaConfig :: EnigmaConfig -> Char -> DisplayOpts -> String
displayEnigmaConfig ec ch optsin =
    case (format optsin) of
        x | elem x fmtsSingle_ -> showEnigmaConfig_
        x | elem x fmtsInternal_ -> showEnigmaConfigInternal_
        x | elem x fmtsWindows_ -> (windows ec) ++ encs
        x | elem x fmtsConfig_ -> (show ec) ++ encs
        x | elem x fmtsEncoding_ -> drop 2 encs
        -- TBD - How to implement debug format?
        -- This should not happen: all display option arguments are coerced to valid values by displayOpts
        _ -> error ("Unrecognized format " ++ (format opts)) -- TBD -- Error handling EnigmaDisplayError('Bad argument - Unrecognized format, {0}'.format(format)) <<<
    where
        -- Ensure valid arguments
        enc = enigmaMapping ec
        ech = enigmaChar ch
        opts = validOpts_ optsin

        encs = if (elem ech letters) && (( showencoding opts) || elem (format opts) fmtsEncoding_)
                then "  " ++ [ech] ++ " > " ++ [(encode (enigmaMapping ec) ech)]
                else ""

        showEnigmaConfig_ = fmt ech (markedMapping (locCar ech enc enc) enc (markerfunction_ opts))
                                     (windows ec)
                                     (reverse $ tail.init $ positions ec)
                where
                    fmt ch e ws ps = printf "%s %s  %s  %s" lbl e ws ps'
                        where
                            lbl = if ch == ' ' then "   " else  ch:" >"
                            ps' = unwords $ (printf "%02d") <$> ps

        showEnigmaConfigInternal_ =
                unlines $ [fmt (if ech == ' ' then "" else ech:" >") (markedMapping (head charLocs) letters (markerfunction_ opts)) ' ' 0 ""] ++
                          (zipWith5 fmt (init <> reverse $ ["P"] ++ (show <$> (tail.init $ stages ec)) ++ ["R"])
                                        (zipWith3 markedMapping (tail.init $ charLocs) (stageMappingList ec) (replicate 500  (markerfunction_ opts))) -- TBD -- Fix replication! <<<
                                        (" " ++ (reverse $ windows ec) ++ replicate (length $ positions ec) ' ')
                                        ([0] ++ ((tail.init $ positions ec)) ++ replicate (length $ positions ec) 0 )
                                        (components ec ++ (tail $ reverse $ components ec))
                          ) ++
                          [fmt (if ech == ' ' then "" else (encode (enigmaMapping ec) ech):" <")
                               (markedMapping (last charLocs) (enigmaMapping ec) (markerfunction_ opts)) ' ' 0 ""]
                where
                    charLocs = zipWith (locCar ech)
                                   ([letters] ++ stageMappingList ec ++ [enigmaMapping ec])
                                   ([letters] ++ enigmaMappingList ec ++ [enigmaMapping ec])
                    fmt l e w p n = printf "%3.3s %s  %s  %s  %s" l e (w:[]) p' n
                       where
                            p' = if p == 0 then "  " else printf "%02d" (p::Int)

{-# DEPRECATED showEnigmaConfig "This has been replaced by 'displayEnigmaConfig'" #-} -- TBD - Replace doc with deprecation note and supply args <<<
-- | Display a summary of the Enigma machine configuration as its encoding (see 'Mapping'),
--   the letters at the windows (see 'windows'), and the 'Position's of the rotors (see 'positions').
--
--   If an uppercase letter is provided, indicate that as input and mark the encoded letter.
--   Other characters will be ignored.
--
--   For example, #showEnigmaConfigEG#
--
--   >>> putStr $ showEnigmaConfig (configEnigma "b-γ-V-VIII-II" "LFAQ" "UX.MO.KZ.AY.EF.PL" "03.17.04.11") 'K'
--   K > CMAWFEKLNVG̲̅HBIUYTXZQOJDRPS  LFAQ  10 16 24 07
--
--   shows the process of encoding of the letter __@\'K\'@__ to __@\'G\'@__.
showEnigmaConfig :: EnigmaConfig -> Char -> String
showEnigmaConfig ec ch = displayEnigmaConfig ec ch displayOpts

-- TBD - Improve resolution of figure showing mapping <<<
{-# DEPRECATED showEnigmaConfigInternal "This has been replaced by 'displayEnigmaConfig'" #-} -- TBD - Replace doc with deprecation note and supply args <<<
-- | Display a summary of the Enigma machine configuration as a schematic showing the encoding (see 'Mapping')
--   performed by each stage (see 'stageMappingList'), along with an indication of the stage
--   (rotor number, @\"P\"@ for plugboard, or @\"R\"@ for reflector), window letter (see 'windows'),
--   'Position' (see 'positions') and 'Name',
--   followed by the encoding for the machine, and preceded by  a (trivial, no-op) keyboard \"encoding\"
--   for reference.
--
--   If an uppercase letter is provided, indicate that as input and mark the letter it is encoded to at
--   each stage; mark its encoding as output. Other characters will be ignored.
--
--   For example, #showEnigmaConfigInternalEG#
--
--   >>> putStr $ showEnigmaConfigInternal (configEnigma "b-γ-V-VIII-II" "LFAQ" "UX.MO.KZ.AY.EF.PL" "03.17.04.11") 'K'
--   K > ABCDEFGHIJK̲̅LMNOPQRSTUVWXYZ
--     P YBCDFEGHIJZ̲̅PONMLQRSTXVWUAK         UX.MO.KZ.AY.EF.PL
--     1 LORVFBQNGWKATHJSZPIYUDXEMC̲̅  Q  07  II
--     2 BJY̲̅INTKWOARFEMVSGCUDPHZQLX  A  24  VIII
--     3 ILHXUBZQPNVGKMCRTEJFADOYS̲̅W  F  16  V
--     4 YDSKZPTNCHGQOMXAUWJ̲̅FBRELVI  L  10  γ
--     R ENKQAUYWJI̲̅COPBLMDXZVFTHRGS         b
--     4 PUIBWTKJZ̲̅SDXNHMFLVCGQYROAE         γ
--     3 UFOVRTLCASMBNJWIHPYQEKZDXG̲̅         V
--     2 JARTMLQ̲̅VDBGYNEIUXKPFSOHZCW         VIII
--     1 LFZVXEINSOKAYHBRG̲̅CPMUDJWTQ         II
--     P YBCDFEG̲̅HIJZPONMLQRSTXVWUAK         UX.MO.KZ.AY.EF.PL
--   G < CMAWFEKLNVG̲̅HBIUYTXZQOJDRPS
--
--   shows the process of encoding of the letter __@\'K\'@__ to __@\'G\'@__:
--
--   * __@\'K\'@__ is entered at the keyboard, which is then
--   * encoded by the plugboard (@\'P\'@), which includes  @\"KZ\"@ in its specification (see 'Name'),
--     to __@\'Z\'@__, which is then
--   * encoded by the first rotor (@\'1\'@), a @\"II\"@ rotor in the @07@ position (and @\'Q\'@ at the window),
--     to __@\'C\'@__, which is then
--   * encoded by the second rotor (@\'2\'@), a @\"VIII\"@ rotor in the @24@ position (and @\'A\'@ at the window),
--     to __@\'Y\'@__, which is then
--   * encoded by the third rotor (@\'3\'@), a @\"V\"@ rotor in the @16@ position (and @\'F\'@ at the window),
--     to __@\'S\'@__, which is then
--   * encoded by the fourth rotor (@\'4\'@), a @\"γ\"@ rotor in the @10@ position (and @\'L\'@ at the window),
--     to __@\'J\'@__, which is then
--   * encoded by the reflector rotor (@\'U\'@), a @\"b\"@ reflector,
--     to __@\'I\'@__, which reverses the signal sending it back through the rotors, where it is then
--   * encoded in reverse by the fourth rotor (@\'4\'@),
--     to __@\'Z\'@__, which is then
--   * encoded in reverse by the third rotor (@\'3\'@),
--     to __@\'G\'@__, which is then
--   * encoded in reverse by the second rotor (@\'2\'@),
--     to __@\'Q\'@__, which is then
--   * encoded in reverse by the first rotor (@\'1\'@),
--     to __@\'G\'@__, which is then
--   * left unchanged by the plugboard (@\'P\'@), and finally
--   * displayed as __@\'G\'@__
--
--   Note that (as follows from 'Mapping') the position of the marked letter at each stage is the alphabetic position
--   of the marked letter at the previous stage.
--
--   This can be represented schematically (with input arriving and output exiting on the left) as #showEnigmaConfigInternalFIG#
--
--   <<figs/configinternal.jpg>>
showEnigmaConfigInternal :: EnigmaConfig -> Char -> String
showEnigmaConfigInternal ec ch = displayEnigmaConfig ec ch displayOpts{format="internal"}


-- Operation display ---------------------------------------------------------

-- REV - Put `(if elem fmt fmtsInternal then init else id) $` in front of `unlines` to remove final new line of internal?

-- Preprocess a string into a 'Message' (using 'message') and produce a configuration display for the
-- starting configuration and for each character of the message, using the provided configuration display function.
-- Note that while 'showEnigmaOperation' and 'showEnigmaOperationInternal' indicate a 'Message' argument, it is
-- this function, which both call, that applies 'message'.
-- TBD: Add single and others: before: show use of the two additional operation flags using some simeper simpler formats <<<
--      Internal showing steps nubers, no step numbers, and limited numbers of steps
--      Show encoding for a format that doesn't use it
{-|
Produce a 'String' representation of an Enigma machine's internal configuration (see 'displayEnigmaConfig' for details)
and for each subsequent configuration as it processes each letter of a 'Message'.

In addition to the options that apply to the representation of each individual configuration, 'DisplayOpts' for this
function include options for specifying whether to run for a specific number of steps and whether to include a step
number in the representations.

>>> let cfg = configEnigma "b-γ-V-VIII-II" "LFAP" "UX.MO.KZ.AY.EF.PL" "03.17.04.11"
>>> putStr $ displayEnigmaOperation cfg "KRIEG" displayOpts
    OHNKJYSBTEDMLCARWPGIXZQUFV  LFAP  10 16 24 06
K > CMAWFEKLNVG̲̅HBIUYTXZQOJDRPS  LFAQ  10 16 24 07
R > HXETCUMASQNZGKRYJO̲̅IDFWVBPL  LFAR  10 16 24 08
I > FGRJUABYW̲̅DZSXVQTOCLPENIMHK  LFAS  10 16 24 09
E > SJWYN̲̅UZPQBVXRETHIMAOFKCLDG  LFAT  10 16 24 10
G > EOKPAQW̲̅JLHCISTBDFVMNXRGUZY  LFAU  10 16 24 11
>>> putStr $ displayEnigmaOperation cfg "KRIEG" displayOpts{showsteps=True}
000      OHNKJYSBTEDMLCARWPGIXZQUFV  LFAP  10 16 24 06
001  K > CMAWFEKLNVG̲̅HBIUYTXZQOJDRPS  LFAQ  10 16 24 07
002  R > HXETCUMASQNZGKRYJO̲̅IDFWVBPL  LFAR  10 16 24 08
003  I > FGRJUABYW̲̅DZSXVQTOCLPENIMHK  LFAS  10 16 24 09
004  E > SJWYN̲̅UZPQBVXRETHIMAOFKCLDG  LFAT  10 16 24 10
005  G > EOKPAQW̲̅JLHCISTBDFVMNXRGUZY  LFAU  10 16 24 11
>>> putStr $ displayEnigmaOperation cfg "KRIEG" displayOpts{showsteps=False,steps=2}
    OHNKJYSBTEDMLCARWPGIXZQUFV  LFAP  10 16 24 06
K > CMAWFEKLNVG̲̅HBIUYTXZQOJDRPS  LFAQ  10 16 24 07
R > HXETCUMASQNZGKRYJO̲̅IDFWVBPL  LFAR  10 16 24 08
>>> putStr $ displayEnigmaOperation cfg "KRIEG" displayOpts{showsteps=False,steps=10}
    OHNKJYSBTEDMLCARWPGIXZQUFV  LFAP  10 16 24 06
K > CMAWFEKLNVG̲̅HBIUYTXZQOJDRPS  LFAQ  10 16 24 07
R > HXETCUMASQNZGKRYJO̲̅IDFWVBPL  LFAR  10 16 24 08
I > FGRJUABYW̲̅DZSXVQTOCLPENIMHK  LFAS  10 16 24 09
E > SJWYN̲̅UZPQBVXRETHIMAOFKCLDG  LFAT  10 16 24 10
G > EOKPAQW̲̅JLHCISTBDFVMNXRGUZY  LFAU  10 16 24 11
    IKOEDUXWAMBPJYCLSZQVFTHGNR  LFAV  10 16 24 12
    RSWOUZNXTQLKPGDMJABIEYCHVF  LFAW  10 16 24 13
    CUAMOTILGNWHDJERSPQFBZKYXV  LFAX  10 16 24 14
    HXPVYZKAOTGSRWICUMLJQDNBEF  LFAY  10 16 24 15
    QSXTPJNRUFMVKGZEAHBDILYCWO  LFAZ  10 16 24 16
>>> putStr $ displayEnigmaOperation cfg "KRIEG" displayOpts{showsteps=False,steps=2,format="internal"}
    ABCDEFGHIJKLMNOPQRSTUVWXYZ
  P YBCDFEGHIJZPONMLQRSTXVWUAK         UX.MO.KZ.AY.EF.PL
  1 DMPSWGCROHXLBUIKTAQJZVEYFN  P  06  II
  2 BJYINTKWOARFEMVSGCUDPHZQLX  A  24  VIII
  3 ILHXUBZQPNVGKMCRTEJFADOYSW  F  16  V
  4 YDSKZPTNCHGQOMXAUWJFBRELVI  L  10  γ
  R ENKQAUYWJICOPBLMDXZVFTHRGS         b
  4 PUIBWTKJZSDXNHMFLVCGQYROAE         γ
  3 UFOVRTLCASMBNJWIHPYQEKZDXG         V
  2 JARTMLQVDBGYNEIUXKPFSOHZCW         VIII
  1 RMGAWYFJOTPLBZICSHDQNVEKXU         II
  P YBCDFEGHIJZPONMLQRSTXVWUAK         UX.MO.KZ.AY.EF.PL
    OHNKJYSBTEDMLCARWPGIXZQUFV
<BLANKLINE>
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
<BLANKLINE>
R > ABCDEFGHIJKLMNOPQR̲̅STUVWXYZ
  P YBCDFEGHIJZPONMLQR̲̅STXVWUAK         UX.MO.KZ.AY.EF.PL
  1 NQUEAPMFVJZSGIRYOH̲̅XTCWDLBK  R  08  II
  2 BJYINTKW̲̅OARFEMVSGCUDPHZQLX  A  24  VIII
  3 ILHXUBZQPNVGKMCRTEJFADO̲̅YSW  F  16  V
  4 YDSKZPTNCHGQOMX̲̅AUWJFBRELVI  L  10  γ
  R ENKQAUYWJICOPBLMDXZVFTHR̲̅GS         b
  4 PUIBWTKJZSDXNHMFLV̲̅CGQYROAE         γ
  3 UFOVRTLCASMBNJWIHPYQEK̲̅ZDXG         V
  2 JARTMLQVDBG̲̅YNEIUXKPFSOHZCW         VIII
  1 EYUWDHM̲̅RNJZXGAQFBOLTCIVSPK         II
  P YBCDFEGHIJZPO̲̅NMLQRSTXVWUAK         UX.MO.KZ.AY.EF.PL
O < HXETCUMASQNZGKRYJO̲̅IDFWVBPL

Note that the first block of the display represents the initial configuration of the machine, but does not
perform any encoding (as explained in 'step'). Note also that the second block of this display is the same
as one displayed in the example for 'displayEnigmaConfig', where it is explained in more detail.
-}
displayEnigmaOperation :: EnigmaConfig -> Message -> DisplayOpts -> String
displayEnigmaOperation ec str opts = unlines $ listEnigmaOperation ec str opts

-- TBD : Document <<<
-- Generate a list where each element is a step of displayEnigmaOperation
listEnigmaOperation :: EnigmaConfig -> Message -> DisplayOpts -> [String]
listEnigmaOperation ec str optsin = zipWith3 (\n sec scr -> (fmtN  (showsteps opts) n) ++ (displayEnigmaConfig sec scr opts))
                                                      [0..(if (steps opts) < 0 then max (length msg) 1 else (steps opts))]
                                                      (iterate step ec)
                                                      (' ':msg ++ [' ',' '..])
                                                where
                                                    -- Ensure valid arguments
                                                    msg = message str
                                                    opts = validOpts_ optsin

                                                    fmtN :: Bool -> Int -> String
                                                    fmtN True n = (printf "%03d  " n) ++ (if elem (format opts) fmtsInternal_ then "\n" else "")
                                                    fmtN False _ = ""

{-# DEPRECATED showEnigmaOperation "This has been replaced by 'displayEnigmaOperation'" #-} -- TBD - Replace doc with deprecation note and supply args <<<
-- | Show a summary of an Enigma machine configuration (see 'showEnigmaConfig')
--   and for each subsequent configuration as it processes each letter of a 'Message'. #showEnigmaOperationEG#
--
--   >>> putStr $ showEnigmaOperation (configEnigma "b-γ-V-VIII-II" "LFAP" "UX.MO.KZ.AY.EF.PL" "03.17.04.11") "KRIEG"
--       OHNKJYSBTEDMLCARWPGIXZQUFV  LFAP  10 16 24 06
--   K > CMAWFEKLNVG̲̅HBIUYTXZQOJDRPS  LFAQ  10 16 24 07
--   R > HXETCUMASQNZGKRYJO̲̅IDFWVBPL  LFAR  10 16 24 08
--   I > FGRJUABYW̲̅DZSXVQTOCLPENIMHK  LFAS  10 16 24 09
--   E > SJWYN̲̅UZPQBVXRETHIMAOFKCLDG  LFAT  10 16 24 10
--   G > EOKPAQW̲̅JLHCISTBDFVMNXRGUZY  LFAU  10 16 24 11
--
--   Note that the first line of the display represents the initial configuration of the machine, but does not
--   perform any encoding (as explained in 'step').
--   Note also that the second line of this display is the same as one displayed in the example for 'showEnigmaConfig'.
showEnigmaOperation :: EnigmaConfig -> Message -> String
showEnigmaOperation ec str = displayEnigmaOperation ec str displayOpts{format="single"}
-- displayEnigmaOperation

-- REV: Trial alternate doc commenting using block comments <<<
{-# DEPRECATED showEnigmaOperationInternal "This has been replaced by 'displayEnigmaOperation'" #-} -- TBD - Replace doc with deprication note and supply args <<<
{-|
Show a schematic of an Enigma machine's internal configuration (see 'showEnigmaConfigInternal' for details)
and for each subsequent configuration as it processes each letter of a 'Message'.
-}
showEnigmaOperationInternal :: EnigmaConfig -> Message -> String
showEnigmaOperationInternal ec str = displayEnigmaOperation ec str displayOpts{format="internal"}


-- Encoding display ==========================================================

-- REV - Move postproc here
-- TBD - Add new arguments for formatting (and use in cli)
displayEnigmaEncoding :: EnigmaConfig -> Message -> String
displayEnigmaEncoding ec str = postproc $ enigmaEncoding ec (message str)

{-# DEPRECATED showEnigmaEncoding "This has been replaced by 'displayEnigmaEncoding'" #-} -- TBD - Replace doc with deprication note and supply args <<<
-- | Show the conventionally formatted encoding of a 'Message' by an (initial) Enigma machine configuration.
--
--   >>> let cfg = configEnigma "c-β-V-VI-VIII" "CDTJ" "AE.BF.CM.DQ.HU.JN.LX.PR.SZ.VW" "05.16.05.12"
--   >>> putStr $ showEnigmaEncoding cfg "FOLGENDES IST SOFORT BEKANNTZUGEBEN"
--   RBBF PMHP HGCZ XTDY GAHG UFXG EWKB LKGJ
showEnigmaEncoding :: EnigmaConfig -> Message -> String
showEnigmaEncoding ec str = displayEnigmaEncoding ec str


