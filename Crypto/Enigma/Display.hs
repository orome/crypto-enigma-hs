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
import Data.Char                (toLower, isAscii)
import Crypto.Enigma.Utils
import Crypto.Enigma

-- REV: Final newline in show... functions is a bit inconsistent



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
data MarkerFunc_ = MarkerFunc_ (Char -> String)
        
-- REV: Export to support arbitrary user-defined functions?
-- Create a 'MarkerFunc_' from a string specification. Ignores unrecognized/invalid values
markerFunc_ :: MarkerSpec -> MarkerFunc_
markerFunc_ spec = MarkerFunc_ (case spec of
                    "*" ->  \_ -> "*"
                    "lower" ->  \c -> [toLower c]
                    "omit" ->  \_ -> " "
                    "bars" -> \ch -> ch:"\818\773"
                    "legacy" -> \c -> [ '[', c, ']' ]
                    "red" ->  \c -> escape_ "31;1m" c
                    "blue" ->  \c -> escape_ "34;1m" c
                    "green" ->  \c -> escape_ "32;1m" c
                    "bold" ->  \c -> escape_ "1m" c
                    "underline" ->  \c -> escape_ "4m" c
                    "highlight" ->  \c -> escape_ "7m" c
                    "cyan" ->  \c -> escape_ "36;1m" c
                    "yellow" ->  \c -> escape_ "33;1m" c
--                     "51" ->  \c -> esc ++ "51m" ++ [c] ++ esc ++ "0m"
--                     "52" ->  \c -> esc ++ "52m" ++ [c] ++ esc ++ "0m"
                    -- TBD: More colors and other escapes
                    [l, r] -> \c -> [l, c, r]
                    _ -> \c -> [c])

mark_ :: MarkerFunc_ -> Char -> String
mark_ (MarkerFunc_ f) c = f c

-- !!! - Assumes that MarkerFuncs that add visible length to marked character always add exactly two characters
markerWidth_ :: MarkerFunc_ -> Int
markerWidth_ mf | (length $ filter isAscii (mark_ mf 'X')) == 3 = 1     -- Property enforced by markerFunc_
                | otherwise = 0

{-|
A (<https://wiki.haskell.org/Type_synonym synonym> for) 'Int', indicating that this option will be coerced to a valid
valid 'steps' value when used by display functions.
-}
type DisplaySteps = Int

{-|
Options for 'displayEnigmaConfig', 'displayEnigmaOperation', and 'listEnigmaOperation', created using 'displayOpts'.
All fields are coerced to valid values by display functions.
-}
data DisplayOpts = DisplayOpts {
        {-|
        A 'Format' specifying the format used to display 'EnigmaConfig' machine configuration(s) that should be one of
        @"single"@, @"internal"@, @"windows"@, @"config"@, and @"encoding"@:

            [@"single"@] A summary of the Enigma machine configuration as its encoding (see 'Mapping'),
            the letters at the windows (see 'windows'), and the 'Position's of the rotors (see 'positions').

                Any valid letter being encoded (see 'showencoding') by the configuration is indicated as input,
                and the encoding of the letter is marked (see 'markerspec').

                For example, #displayEnigmaSingleEG#

                > K > CMAWFEKLNVG̲̅HBIUYTXZQOJDRPS  LFAQ  10 16 24 07

                shows the process of encoding of the letter __@\'K\'@__ to __@\'G\'@__.

            [@"internal"@] An expaned schematic of the configuration showing the encoding (see 'Mapping')
            performed by each stage (see 'stageMappingList'), along with an indication of the stage (rotor number,
            @\"P\"@ for plugboard, or @\"R\"@ for reflector), window letter (see 'windows'), 'Position'
            (see 'positions') and 'Name', followed by the encoding for the machine as a whole, and preceded by
            a (trivial, no-op) keyboard \"encoding\" for reference.

                Any valid letter being encoded (see 'showencoding') by the configuration is indicated as input,
                and its encoding at each stage is marked (see 'markerspec').

                For example, #displayEnigmaConfigInternalEG#

                @
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
                @

                shows the "internals" of the same proccess as avove using the same machine configuration:
                the encoding of the letter __@\'K\'@__ to __@\'G\'@__:

                * __@\'K\'@__ is entered at the keyboard, which is then
                * encoded by the plugboard (@\'P\'@), which includes  @\"KZ\"@ in its specification (see 'Name'),
                to __@\'Z\'@__, which is then
                * encoded by the first rotor (@\'1\'@), a @\"II\"@ rotor in the @07@ position (and @\'Q\'@ at the window),
                to __@\'C\'@__, which is then
                * encoded by the second rotor (@\'2\'@), a @\"VIII\"@ rotor in the @24@ position (and @\'A\'@ at the window),
                to __@\'Y\'@__, which is then
                * encoded by the third rotor (@\'3\'@), a @\"V\"@ rotor in the @16@ position (and @\'F\'@ at the window),
                to __@\'S\'@__, which is then
                * encoded by the fourth rotor (@\'4\'@), a @\"γ\"@ rotor in the @10@ position (and @\'L\'@ at the window),
                to __@\'J\'@__, which is then
                * encoded by the reflector rotor (@\'U\'@), a @\"b\"@ reflector,
                to __@\'I\'@__, which reverses the signal sending it back through the rotors, where it is then
                * encoded in reverse by the fourth rotor (@\'4\'@),
                to __@\'Z\'@__, which is then
                * encoded in reverse by the third rotor (@\'3\'@), to __@\'G\'@__, which is then
                * encoded in reverse by the second rotor (@\'2\'@), to __@\'Q\'@__, which is then
                * encoded in reverse by the first rotor (@\'1\'@), to __@\'G\'@__, which is then
                * left unchanged by the plugboard (@\'P\'@), and finally
                * displayed as __@\'G\'@__

                Note that (as follows from 'Mapping') the position of the marked letter at each stage is the
                alphabetic position of the marked letter at the previous stage.

                This can be represented schematically (with input arriving and output exiting on the left) as #showEnigmaConfigInternalFIG#

                <<figs/configinternal.jpg>>

            [@"windows"@] The letters at the window (see 'windows'):

                > LFAQ

            [@"config"@] The specification of the configuration (see 'configEnigmaExcept') in the same format used by
            @configEnigma@ and @configEnigmaExcept@, as a single string:

                > b-γ-V-VIII-II LFAQ UX.MO.KZ.AY.EF.PL 03.17.04.11

            [@"encoding"@] The encoding of any valid letter being encoded (see 'showencoding'):

                > K > G


        Note the "windows" and config" effectively display the same format as 'windows' and @show@, and are most
        useful in conjunction with other options (e.g. 'showencoding' and 'showsteps', respectively) and for showing
        operation with 'displayEnigmaOperation'.

        See 'displayEnigmaConfig' for further examples.

        Display functions are forgiving about the supplied format and will accept a range of reasonable substitutes
        (e.g., @"detailed"@ or @"schematic"@ for @"internal"@), and will treat unrecognized formats as the default,
        @"single"@.
        -}
        format :: !Format,
        {-|
        A @Bool@ indicating whether to show encoding if not normally shown for the specified format.

        If a valid letter is being encoded (one is either provided as an argument to 'displayEnigmaConfig', or there
        is a message provided as an argument to 'displayEnigmaOperation' with a letter at the displayed step) the letter
        and its encoding are indicated.

        This setting applies only to @"windows"@ and @"config"@. For example, where "windows" normally shows just

        > LFAQ

        setting @showencoding@ to @True@ produces

        > LFAQ  K > G
        -}
        showencoding :: !Bool,
        {-|
        A 'MarkerSpec' that should be either a pair or characters (e.g. @"[]"@) or a specification of a color
        (e.g, @"red"@) or style (@"bars"@) to use to highlight the encoded character in formats that
        show encoding. For example the default @markerspec@ of @"bars"@ highlights the encoding (in, e.g.,
        the @"single"@ format with a bar above and below the encoding

        > K > CMAWFEKLNVG̲̅HBIUYTXZQOJDRPS  LFAQ  10 16 24 07

        and supplying a @markerspec@ of @"[]"@ instead highlights the encoding of the letter by placing a @"["@ to
        the left and a @"]"@ to the right of the encoding

        > K > CMAWFEKLNV[G]HBIUYTXZQOJDRPS  LFAQ  10 16 24 07

        Invalid or unrecognized highlight specifications are treated as the @"bars"@.
        -}
        markerspec :: !MarkerSpec,
        markerfunction_ :: !MarkerFunc_,  -- REV: Internal only; expose for custom marker functions?
        {-|
        A @Bool@ indicating whether to show step numbers, defaults to @False@.

        Only relevant for operation display functions. See 'displayEnigmaOperation' for examples.
        -}
        showsteps :: !Bool,
        {-|
        An @Int@ indicating the number of steps to display, which defaults to the length of a message if one is
        being displayed, and to @1@ otherwise. Values less than @1@ are treated as the default.

        Only relevant for operation display functions. See 'displayEnigmaOperation' for examples.
        -}
        steps :: !DisplaySteps
}

allSteps_ = (-1)

{-|
Default 'DisplayOpts' equivalent to @DisplayOpts{format="single",showencoding=False,markerspec="bars",...}@.
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
        markerfunction_ = markerFunc_ "bars",
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
                        markerfunction_ = markerFunc_ ms,
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

{-|
A @String@ representation of an 'EnigmaConfig' using the specified 'DisplayOpts'.

If an uppercase letter is provided, indicate that as input show its encoding(s), as determined by the the 'format'
and 'showencoding' and, where for the @format@, 'markerspec'. Other characters will be ignored.

For example, the illustrations given in the discussion of the 'format' option above can be created using (in order)

>>> let cfg = step $ configEnigma "b-γ-V-VIII-II" "LFAP" "UX.MO.KZ.AY.EF.PL" "03.17.04.11"
>>> putStrLn $ displayEnigmaConfig cfg 'K' displayOpts{format="single"}
K > CMAWFEKLNVG̲̅HBIUYTXZQOJDRPS  LFAQ  10 16 24 07
>>> putStrLn $ displayEnigmaConfig cfg 'K' displayOpts{format="internal"}
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

>>> putStrLn $ displayEnigmaConfig cfg 'K' displayOpts{format="windows"}
LFAQ

>>> putStrLn $ displayEnigmaConfig cfg 'K' displayOpts{format="config"}
b-γ-V-VIII-II LFAQ UX.MO.KZ.AY.EF.PL 03.17.04.11

>>> putStrLn $ displayEnigmaConfig cfg 'K' displayOpts{format="encoding"}
K > G

and other options can be freely combined (or omitted), for example

>>> putStrLn $ displayEnigmaConfig cfg 'K' displayOpts{format="single",markerspec="()"}
K > CMAWFEKLNV(G)HBIUYTXZQOJDRPS  LFAQ  10 16 24 07

>>> putStrLn $ displayEnigmaConfig cfg 'K' displayOpts{format="config",showencoding=True}
b-γ-V-VIII-II LFAQ UX.MO.KZ.AY.EF.PL 03.17.04.11  K > G

>>> putStrLn $ displayEnigmaConfig cfg 'K' displayOpts
K > CMAWFEKLNVG̲̅HBIUYTXZQOJDRPS  LFAQ  10 16 24 07
-}
displayEnigmaConfig :: EnigmaConfig -> Char -> DisplayOpts -> String
displayEnigmaConfig ec ch optsin =
    case (format optsin) of
        x | elem x fmtsSingle_ -> showEnigmaConfig_
        x | elem x fmtsInternal_ -> showEnigmaConfigInternal_
        x | elem x fmtsWindows_ -> (windows ec) ++ encs
        x | elem x fmtsConfig_ -> (show ec) ++ encs
        x | elem x fmtsEncoding_ -> drop 2 encs
        _ -> showEnigmaConfig_          -- Should not happen: all display option arguments are coerced by validOpts_
    where
        -- Ensure valid arguments
        enc = enigmaMapping ec
        ech = enigmaChar ch
        opts = validOpts_ optsin

        encs = if (elem ech letters) && (( showencoding opts) || elem (format opts) fmtsEncoding_)
                then "  " ++ [ech] ++ " > " ++ [(encode (enigmaMapping ec) ech)]
                else ""

        -- TBD: Can't use below unless encode handles ch == ' '
        -- Locate the index of the encoding with m of ch, in s
        --locCar :: Char -> String -> Mapping -> Maybe Int
        locCar ch s m = elemIndex (encode m ch) s

        --markedMapping :: Maybe Int -> Mapping -> MarkerFunc_ -> String
        markedMapping (Just loc) e mf = take loc <> (mark_ mf).(!!loc) <> drop (loc + 1) $ e
        markedMapping Nothing e mf = buff ++ e ++ buff where buff = replicate (markerWidth_ mf) ' '
        -- Pad to align unmarked encoding if visible length is changed by marking

        -- If the character isn't in 'letters', treat it as blank (a special case for 'encode' and other functions)
        --enigmaChar :: Char -> Char
        enigmaChar ch = if ch `elem` letters then ch else ' '

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
                                        (zipWith3 markedMapping (tail.init $ charLocs) (stageMappingList ec) (cycle [markerfunction_ opts]))
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

{-# DEPRECATED showEnigmaConfig "This has been replaced by 'displayEnigmaConfig'" #-}
{-|
Equivalent to 'displayEnigmaConfig' with @displayOpts{format="single"}@.
-}
showEnigmaConfig :: EnigmaConfig -> Char -> String
showEnigmaConfig ec ch = displayEnigmaConfig ec ch displayOpts

{-# DEPRECATED showEnigmaConfigInternal "This has been replaced by 'displayEnigmaConfig'" #-}
{-|
Equivalent to 'displayEnigmaConfig' with @displayOpts{format="internal"}@.
-}
showEnigmaConfigInternal :: EnigmaConfig -> Char -> String
showEnigmaConfigInternal ec ch = displayEnigmaConfig ec ch displayOpts{format="internal"}


-- Operation display ---------------------------------------------------------

{-|
A 'String' representation of an Enigma machine's internal configuration (see 'displayEnigmaConfig' for details)
and for each subsequent configuration as it processes each letter of a 'Message', subject to the provided 'DisplayOpts'.

In addition to the options that apply to the representation of each individual configuration, @DisplayOpts@ for this
function include options for specifying whether to run for a specific number of steps and whether to include a step
number in the representations.

For example, these options applied to the default @"single"@ 'format' #displayEnigmaOperationEG#

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

to the @"internal"@ 'format'

>>> putStr $ displayEnigmaOperation cfg "KRIEG" displayOpts{format="internal",showsteps=False,steps=2}
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

and to some of the other 'format's

>>> putStr $ displayEnigmaOperation cfg "KRIEG" displayOpts{showencoding=True,format="windows",showsteps=True,steps=8}
000  LFAP
001  LFAQ  K > G
002  LFAR  R > O
003  LFAS  I > W
004  LFAT  E > N
005  LFAU  G > W
006  LFAV
007  LFAW
008  LFAX

>>> putStr $ displayEnigmaOperation cfg "KRIEG" displayOpts{format="config",showsteps=True,steps=8,showencoding=True}
000  b-γ-V-VIII-II LFAP UX.MO.KZ.AY.EF.PL 03.17.04.11
001  b-γ-V-VIII-II LFAQ UX.MO.KZ.AY.EF.PL 03.17.04.11  K > G
002  b-γ-V-VIII-II LFAR UX.MO.KZ.AY.EF.PL 03.17.04.11  R > O
003  b-γ-V-VIII-II LFAS UX.MO.KZ.AY.EF.PL 03.17.04.11  I > W
004  b-γ-V-VIII-II LFAT UX.MO.KZ.AY.EF.PL 03.17.04.11  E > N
005  b-γ-V-VIII-II LFAU UX.MO.KZ.AY.EF.PL 03.17.04.11  G > W
006  b-γ-V-VIII-II LFAV UX.MO.KZ.AY.EF.PL 03.17.04.11
007  b-γ-V-VIII-II LFAW UX.MO.KZ.AY.EF.PL 03.17.04.11
008  b-γ-V-VIII-II LFAX UX.MO.KZ.AY.EF.PL 03.17.04.11

>>> putStr $ displayEnigmaOperation cfg "KRIEG" displayOpts{format="encoding",showsteps=True,showencoding=False}
000
001  K > G
002  R > O
003  I > W
004  E > N
005  G > W

Note that first step of each of these displays represents the initial configuration of the machine,
but does not perform any encoding (as explained in 'step').

Also also that the second block of the @"internal"@ display example is the same as one illustrated for
the @"internal"@ 'format', where it is explained in detail.
-}
displayEnigmaOperation :: EnigmaConfig -> Message -> DisplayOpts -> String
displayEnigmaOperation ec str opts = unlines $ listEnigmaOperation ec str opts

{-|
A list representation of the operation of an enigma machine, equivalent to

> lines $ displayEnigmaOperation
-}
-- Preprocess a string into a 'Message' (using 'message') and produce a configuration display for the
-- starting configuration and for each character of the message, using the provided configuration display function.
-- Note that while 'displayEnigmaOperation' indicate a 'Message' argument, it is this function that applies 'message'.
listEnigmaOperation :: EnigmaConfig -> Message -> DisplayOpts -> [String]
listEnigmaOperation ec str optsin = zipWith3 (\n sec scr -> (fmtN  (showsteps opts) n) ++ (displayEnigmaConfig sec scr opts))
                                                      [0..(if (steps opts) < 0 then max (length msg) 1 else (steps opts))]
                                                      (iterate step ec)
                                                      (' ':msg ++ [' ',' '..])
                                                where
                                                    -- Ensure valid arguments
                                                    msg = message str
                                                    opts = validOpts_ optsin

                                                    --fmtN :: Bool -> Int -> String
                                                    fmtN True n = (printf "%04d  " n) ++ (if elem (format opts) fmtsInternal_ then "\n" else "")
                                                    fmtN False _ = ""

{-# DEPRECATED showEnigmaOperation "This has been replaced by 'displayEnigmaOperation'" #-}
{-|
Equivalent to 'displayEnigmaOperation' with @displayOpts{format="single"}@.
-}
showEnigmaOperation :: EnigmaConfig -> Message -> String
showEnigmaOperation ec str = displayEnigmaOperation ec str displayOpts{format="single"}


{-# DEPRECATED showEnigmaOperationInternal "This has been replaced by 'displayEnigmaOperation'" #-}
{-|
Equivalent to 'displayEnigmaOperation' with @displayOpts{format="internal"}@.
-}
showEnigmaOperationInternal :: EnigmaConfig -> Message -> String
showEnigmaOperationInternal ec str = displayEnigmaOperation ec str displayOpts{format="internal"}


-- Encoding display ==========================================================

{-|
Show the conventionally formatted encoding of a 'Message' by an (initial) Enigma machine configuration.

>>> let cfg = configEnigma "c-β-V-VI-VIII" "CDTJ" "AE.BF.CM.DQ.HU.JN.LX.PR.SZ.VW" "05.16.05.12"
>>> putStr $ displayEnigmaEncoding cfg "FOLGENDES IST SOFORT BEKANNTZUGEBEN"
RBBF PMHP HGCZ XTDY GAHG UFXG EWKB LKGJ
-}
-- TBD: Add new arguments for formatting (and use in cli)
displayEnigmaEncoding :: EnigmaConfig -> Message -> String
displayEnigmaEncoding ec str = postproc $ enigmaEncoding ec (message str)
        where
                -- Standard formatting of encoded messages
                --postproc :: String -> String
                postproc = unlines . chunksOf 60 . unwords . chunksOf 4

{-# DEPRECATED showEnigmaEncoding "This has been replaced by 'displayEnigmaEncoding'" #-}
{-|
Equivalent to 'displayEnigmaEncoding'.
-}
showEnigmaEncoding :: EnigmaConfig -> Message -> String
showEnigmaEncoding ec str = displayEnigmaEncoding ec str


