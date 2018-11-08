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

import Crypto.Enigma.Utils
import Crypto.Enigma

-- TBD - Fix name of more detailed display -> ..EnigmConfigSchematic ? <<<
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

-- TBD - Remove when deprecated showEnigmaConfig and showEnigmaConfigInternal are removed
decorate' :: Char -> String
decorate' ch = ch:"\818\773"

markedMapping :: Maybe Int -> Mapping -> (Char -> String) -> String
markedMapping (Just loc) e mf = take loc <> mf.(!!loc) <> drop (loc + 1) $ e
markedMapping Nothing e _    = e


-- Character restriction ----------------------------------------------------

-- If the character isn't in 'letters', treat it as blank (a special case for 'encode' and other functions)
enigmaChar :: Char -> Char
enigmaChar ch = if ch `elem` letters then ch else ' '



-- Machine operation display =================================================

-- REV - Review positioning and exposure (could be needed for cli help) <<<
fmtsInternal = ["internal", "detailed", "schematic"]
fmtsSingle = ["single", "summary"]
fmtsWindows = ["windows", "winds"]
fmtsConfig = ["config", "configuration", "spec", "specification"]
fmtsEncoding = ["encoding"]
--     _FMTS_DEBUG = ['debug']


-- Configuration display -----------------------------------------------------

displayEnigmaConfig :: EnigmaConfig -> Char -> String -> Bool -> (Char -> String) -> String
displayEnigmaConfig ec ch fmt se mf =
    case fmt of
        x | elem x fmtsSingle -> showEnigmaConfig_
        x | elem x fmtsInternal -> showEnigmaConfigInternal_
        x | elem x fmtsWindows -> (windows ec) ++ encs
        x | elem x fmtsConfig -> (show ec) ++ encs
        x | elem x fmtsEncoding -> drop 2 encs
        -- TBD - How to implement debug format?
        _ -> error ("Not known " ++ fmt) -- TBD -- Error handling EnigmaDisplayError('Bad argument - Unrecognized format, {0}'.format(format)) <<<
    where
        ech = enigmaChar ch
        enc = enigmaMapping ec

        encs = if (elem ech letters) && (se || elem fmt fmtsEncoding)
                then "  " ++ [ech] ++ " > " ++ [(encode (enigmaMapping ec) ech)]
                else ""

        showEnigmaConfig_ = fmt ech (markedMapping (locCar ech enc enc) enc mf)
                                     (windows ec)
                                     (reverse $ tail.init $ positions ec)
                where
                    fmt ch e ws ps = printf "%s %s  %s  %s" lbl e ws ps'
                        where
                            lbl = if ch == ' ' then "   " else  ch:" >"
                            ps' = unwords $ (printf "%02d") <$> ps

        showEnigmaConfigInternal_ =
                unlines $ [fmt (if ech == ' ' then "" else ech:" >") (markedMapping (head charLocs) letters mf) ' ' 0 ""] ++
                          (zipWith5 fmt (init <> reverse $ ["P"] ++ (show <$> (tail.init $ stages ec)) ++ ["R"])
                                        (zipWith3 markedMapping (tail.init $ charLocs) (stageMappingList ec) (replicate 500 mf)) -- TBD -- Fix replication! <<<
                                        (" " ++ (reverse $ windows ec) ++ replicate (length $ positions ec) ' ')
                                        ([0] ++ ((tail.init $ positions ec)) ++ replicate (length $ positions ec) 0 )
                                        (components ec ++ (tail $ reverse $ components ec))
                          ) ++
                          [fmt (if ech == ' ' then "" else (encode (enigmaMapping ec) ech):" <")
                               (markedMapping (last charLocs) (enigmaMapping ec) mf) ' ' 0 ""]
                where
                    -- ech = enigmaChar ch
                    charLocs = zipWith (locCar ech)
                                   ([letters] ++ stageMappingList ec ++ [enigmaMapping ec])
                                   ([letters] ++ enigmaMappingList ec ++ [enigmaMapping ec])
                    fmt l e w p n = printf "%3.3s %s  %s  %s  %s" l e (w:[]) p' n
                       where
                            p' = if p == 0 then "  " else printf "%02d" (p::Int)

{-# DEPRECATED showEnigmaConfig "This has been replaced by displayEnigmaConfig" #-} -- TBD - Replace doc with deprecation note and supply args <<<
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
showEnigmaConfig ec ch = displayEnigmaConfig ec ch "single" True decorate'

-- TBD - Improve resolution of figure showing mapping <<<
{-# DEPRECATED showEnigmaConfigInternal "This has been replaced by displayEnigmaConfig" #-} -- TBD - Replace doc with deprecation note and supply args <<<
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
showEnigmaConfigInternal ec ch = displayEnigmaConfig ec ch "internal" True decorate'


-- Operation display ---------------------------------------------------------

-- REV - Put `(if elem fmt fmtsInternal then init else id) $` in front of `unlines` to remove final new line of internal?

-- Preprocess a string into a 'Message' (using 'message') and produce a configuration display for the
-- starting configuration and for each character of the message, using the provided configuration display function.
-- Note that while 'showEnigmaOperation' and 'showEnigmaOperationInternal' indicate a 'Message' argument, it is
-- this function, which both call, that applies 'message'.
displayEnigmaOperation :: EnigmaConfig -> Message -> String -> Bool -> (Char -> String) -> Bool -> Int -> String
displayEnigmaOperation ec str fmt se mf ss ns = unlines $ listEnigmaOperation ec str fmt se mf ss ns

-- TBD : Document <<<
-- Generate a list where each element is a step of displayEnigmaOperation
listEnigmaOperation :: EnigmaConfig -> Message -> String -> Bool -> (Char -> String) -> Bool -> Int -> [String]
listEnigmaOperation ec str fmt se mf ss ns = zipWith3 (\n sec scr -> (fmtN ss n) ++ (displayEnigmaConfig sec scr fmt se mf))
                                                      [0..(if ns < 0 then max (length msg) 1 else ns)]
                                                      (iterate step ec)
                                                      (' ':msg ++ [' ',' '..])
                                                where
                                                    fmtN :: Bool -> Int -> String
                                                    fmtN True n = (printf "%03d  " n) ++ (if elem fmt fmtsInternal then "\n" else "")
                                                    fmtN False _ = ""
                                                    msg = message str

{-# DEPRECATED showEnigmaOperation "This has been replaced by displayEnigmaOperation" #-} -- TBD - Replace doc with deprecation note and supply args <<<
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
showEnigmaOperation ec str = displayEnigmaOperation ec str "single" False decorate' False (-1)
-- displayEnigmaOperation

-- REV: Trial alternate doc commenting using block comments <<<
{-# DEPRECATED showEnigmaOperationInternal "This has been replaced by displayEnigmaOperation" #-} -- TBD - Replace doc with deprication note and supply args <<<
{-|
Show a schematic of an Enigma machine's internal configuration (see 'showEnigmaConfigInternal' for details)
and for each subsequent configuration as it processes each letter of a 'Message'.

>>> putStr $ showEnigmaOperationInternal (configEnigma "b-γ-V-VIII-II" "LFAP" "UX.MO.KZ.AY.EF.PL" "03.17.04.11") "KR"
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
as one displayed in the example for 'showEnigmaConfigInternal', where it is explained in more detail.
-}
showEnigmaOperationInternal :: EnigmaConfig -> Message -> String
showEnigmaOperationInternal ec str = displayEnigmaOperation ec str "internal" False decorate' False (-1)


-- Encoding display ==========================================================

-- REV - Move postproc here
-- TBD - Add new arguments for formatting (and use in cli)
displayEnigmaEncoding :: EnigmaConfig -> Message -> String
displayEnigmaEncoding ec str = postproc $ enigmaEncoding ec (message str)

{-# DEPRECATED showEnigmaEncoding "This has been replaced by displayEnigmaEncoding" #-} -- TBD - Replace doc with deprication note and supply args <<<
-- | Show the conventionally formatted encoding of a 'Message' by an (initial) Enigma machine configuration.
--
--   >>> let cfg = configEnigma "c-β-V-VI-VIII" "CDTJ" "AE.BF.CM.DQ.HU.JN.LX.PR.SZ.VW" "05.16.05.12"
--   >>> putStr $ showEnigmaEncoding cfg "FOLGENDES IST SOFORT BEKANNTZUGEBEN"
--   RBBF PMHP HGCZ XTDY GAHG UFXG EWKB LKGJ
showEnigmaEncoding :: EnigmaConfig -> Message -> String
showEnigmaEncoding ec str = displayEnigmaEncoding ec str


