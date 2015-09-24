{-# OPTIONS_HADDOCK show-extensions #-}
{-|
Module      : Crypto.Enigma.Display
Description : Display of Enigma machine state and encoding.
License     : BSD3
Maintainer  : royl@aldaron.com
Stability   : experimental
Portability : POSIX

A module for rich display of the state state of and encoding performed by Enigma machines defined in "Crypto.Enigma".
-}

module Crypto.Enigma.Display (
        -- * Configuration display
        showEnigmaConfig,
        showEnigmaConfigInternal,
        -- * Operation display
        showEnigmaOperation,
        showEnigmaOperationInternal,
        -- * Encoding display
        showEnigmaEncoding
) where

import Data.Monoid
import Data.Char
import Data.List
import Data.List.Split          (chunksOf)
import Data.String.Utils        (replace)
import Text.Printf              (printf)

import Crypto.Enigma.Utils
import Crypto.Enigma

{-# ANN module ("HLint: ignore Use infix"::String) #-}
{-# ANN module ("HLint: ignore Use mappend"::String) #-}
{-# ANN module ("HLint: error Redundant $"::String) #-}
{-# ANN module ("HLint: ignore Use ."::String) #-}

-- TBD - SPELLCHECK <<<
-- TBD - Fix name of more detaild display -> ..EnigmConfigSchematic ? <<<
-- REV - Final newline in show... functions is a bit inconsistent



-- Helpers ===================================================================


-- Message entry -------------------------------------------------------------

-- Some standard substitions performed by (Kriegsmarine) operators
preproc :: String -> Message
preproc s = filter (`elem` ['A'..'Z']) $ foldl1 fmap (uncurry replace <$> subs) $ toUpper <$> s
            where
                subs = [(" ",""),(".","X"),(",","Y"),("'","J"),(">","J"),("<","J"),("!","X"),
                        ("?","UD"),("-","YY"),(":","XX"),("(","KK"),(")","KK"),
                        ("1","YQ"),("2","YW"),("3","YE"),("4","YR"),("5","YT"),
                        ("6","YZ"),("7","YU"),("8","YI"),("9","YO"),("0","YP")]


-- Message display -----------------------------------------------------------

-- TBD - Don't remove spaces (at least in showEnigmaOperation and instead put a blank line?)
-- Standard formatting of encoded messages
postproc :: String -> String
postproc = unlines . chunksOf 60 . unwords . chunksOf 4


-- Mapping markup -----------------------------------------------------------

-- TBD Move up (closer to encoding?)
-- TBD - Can't use below unless encode handles ch == ' '
-- locate the index of the encoding with enc of ch, in s
locCar :: Char -> String -> Mapping -> Maybe Int
locCar ch s enc = elemIndex (encode enc ch) s

decorate :: Char -> String
decorate ch = ch:"\818\773"
--decorate ch = ['[',ch,']'] -- version that works when Unicode fails to display properly (e.g. IHaskell as of 0.7.1.0)

markedMapping :: Maybe Int -> Mapping -> String
markedMapping (Just loc) e  = take loc <> decorate.(!!loc) <> drop (loc + 1) $ e
markedMapping Nothing e     = e



-- Machine operation display =================================================

-- Preprocess a message and produce a configuration display for the starting configuraiton
-- and for each character of the message, using the provied configuration display function.
showEnigmaOperation_ :: (EnigmaConfig -> Char -> String) -> EnigmaConfig -> String -> String
showEnigmaOperation_ df ec msg = unlines $ zipWith df (iterate step ec) (' ':(preproc msg))


-- Configuration display -----------------------------------------------------

-- | Display a summary of the Enigma machine configuration as its encoding (see 'Mapping'),
--   the letters at the windows (see 'windows'), and the 'Position's of the rotors (see 'positions').
--
--   If a valid 'Message' character is provided, indicate that as input and mark the encoded letter.
--
--   For example, #showEnigmaConfigEG#
--
--   >>> putStr $ showEnigmaConfig (configEnigma "b-γ-V-VIII-II" "LFAQ" "UX.MO.KZ.AY.EF.PL" "03.17.04.11") 'K'
--   K > CMAWFEKLNVG̲̅HBIUYTXZQOJDRPS  LFAQ  10 16 24 07
--
--   shows the process of encoding of the letter __@\'K\'@__ to __@\'G\'@__.
showEnigmaConfig :: EnigmaConfig -> Char -> String
showEnigmaConfig ec ch = fmt ch' (markedMapping (locCar ch' enc enc) enc) (windows ec) (reverse $ tail.init $ positions ec)
                            where
                                ch' = if ch `elem` letters then ch else ' '
                                enc = enigmaMapping ec
                                fmt ch e ws ps = lbl ++ " " ++ e ++ "  " ++ ws ++ "  "++ ps'
                                    where
                                       lbl = if ch == ' ' then "   " else  ch:" >"
                                       ps' = unwords $ (printf "%02d") <$> ps

-- TBD - Add figure from MMA tool showing mapping
-- | Display a summary of the Enigma machine configuration as a schematic showing the encoding (see 'Mapping')
--   performed by each stage (see 'stageMappingList'), along with an indication of the stage
--   (rotor number, @\"P\"@ for plugboard, or @\"R\"@ for reflector), window letter (see 'windows'),
--   'Position' (see 'positions') and 'Name',
--   followed by the encoding for the machine, and preceeded by  a (trivial, no-op) keyboard \"encoding\"
--   for reference.
--
--   If a valid 'Message' character is provided, indicate that as input and mark the letter it is encoded to at
--   each stage; mark its encoding as output.
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
--   * encoded by the first rotor (@\'1\'@), a @\"II\"@ rotor in the @06@ position (and @\'Q\'@ at the window),
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
--   * encoded left unchainged by the plugboard (@\'P\'@), and finally
--   * displayed as __@\'G\'@__
--
--   Note that (as follows from 'Mapping') the position of the marked letter at each stage is the alphabetic position
--   of the marked letter at the previous stage.
showEnigmaConfigInternal :: EnigmaConfig -> Char -> String
showEnigmaConfigInternal ec ch = unlines $
                            [fmt (if ch' == ' ' then "" else ch':" >") (markedMapping (head charLocs) letters) ' ' 0 ""] ++
                            (zipWith5 fmt (init <> reverse $ ["P"] ++ (show <$> (tail.init $ stages ec)) ++ ["R"])
                                          (zipWith markedMapping (tail.init $ charLocs) (stageMappingList ec))
                                          (" " ++ (reverse $ windows ec) ++ replicate (length $ positions ec) ' ')
                                          ([0] ++ ((tail.init $ positions ec)) ++ replicate (length $ positions ec) 0 )
                                          (components ec ++ (tail $ reverse $ components ec))
                            ) ++
                            [fmt (if ch' == ' ' then "" else (encode (enigmaMapping ec) ch'):" <") (markedMapping (last charLocs) (enigmaMapping ec)) ' ' 0 ""]
                        where
                            ch' = if ch `elem` letters then ch else ' '
                            charLocs = zipWith (locCar ch') ([letters] ++ stageMappingList ec ++ [enigmaMapping ec]) ([letters] ++ enigmaMappingList ec ++ [enigmaMapping ec])
                            fmt l e w p n = lbl ++ " " ++ e ++ "  " ++ (w:[]) ++ "  " ++ p' ++ "  " ++ n
                                where
                                    lbl = printf "%3.3s" l :: String
                                    p' = if p == 0 then "  " else printf "%02d" (p::Int)


-- Operation display ---------------------------------------------------------

-- | Show a summary of an Enigma machine configuration (see 'showEnigmaConfig')
--   and for each subsequent configuration as it processes each letter of a message. #showEnigmaOperationEG#
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
--   Note also that the second line of this display is the same as one displayed in the examle for 'showEnigmaConfig'.
showEnigmaOperation :: EnigmaConfig -> String -> String
showEnigmaOperation ec msg = showEnigmaOperation_ showEnigmaConfig ec msg

-- | Show a schematic of an Enigma machine's internal configuration (see 'showEnigmaConfigInternal')
--   and for each subsequent configuration as it processes each letter of a message.
--
--   >>> putStr $ showEnigmaOperationInternal (configEnigma "b-γ-V-VIII-II" "LFAP" "UX.MO.KZ.AY.EF.PL" "03.17.04.11") "KR"
--       ABCDEFGHIJKLMNOPQRSTUVWXYZ
--     P YBCDFEGHIJZPONMLQRSTXVWUAK         UX.MO.KZ.AY.EF.PL
--     1 DMPSWGCROHXLBUIKTAQJZVEYFN  P  06  II
--     2 BJYINTKWOARFEMVSGCUDPHZQLX  A  24  VIII
--     3 ILHXUBZQPNVGKMCRTEJFADOYSW  F  16  V
--     4 YDSKZPTNCHGQOMXAUWJFBRELVI  L  10  γ
--     R ENKQAUYWJICOPBLMDXZVFTHRGS         b
--     4 PUIBWTKJZSDXNHMFLVCGQYROAE         γ
--     3 UFOVRTLCASMBNJWIHPYQEKZDXG         V
--     2 JARTMLQVDBGYNEIUXKPFSOHZCW         VIII
--     1 RMGAWYFJOTPLBZICSHDQNVEKXU         II
--     P YBCDFEGHIJZPONMLQRSTXVWUAK         UX.MO.KZ.AY.EF.PL
--       OHNKJYSBTEDMLCARWPGIXZQUFV
--   <BLANKLINE>
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
--   <BLANKLINE>
--   R > ABCDEFGHIJKLMNOPQR̲̅STUVWXYZ
--     P YBCDFEGHIJZPONMLQR̲̅STXVWUAK         UX.MO.KZ.AY.EF.PL
--     1 NQUEAPMFVJZSGIRYOH̲̅XTCWDLBK  R  08  II
--     2 BJYINTKW̲̅OARFEMVSGCUDPHZQLX  A  24  VIII
--     3 ILHXUBZQPNVGKMCRTEJFADO̲̅YSW  F  16  V
--     4 YDSKZPTNCHGQOMX̲̅AUWJFBRELVI  L  10  γ
--     R ENKQAUYWJICOPBLMDXZVFTHR̲̅GS         b
--     4 PUIBWTKJZSDXNHMFLV̲̅CGQYROAE         γ
--     3 UFOVRTLCASMBNJWIHPYQEK̲̅ZDXG         V
--     2 JARTMLQVDBG̲̅YNEIUXKPFSOHZCW         VIII
--     1 EYUWDHM̲̅RNJZXGAQFBOLTCIVSPK         II
--     P YBCDFEGHIJZPO̲̅NMLQRSTXVWUAK         UX.MO.KZ.AY.EF.PL
--   O < HXETCUMASQNZGKRYJO̲̅IDFWVBPL
--
--   Note that the first block of the display represents the initial configuration of the machine, but does not
--   perform any encoding (as explained in 'step'). Note also that the second block of this display is the same
--   as one displayed in the examle for 'showEnigmaConfigInternal'.
showEnigmaOperationInternal :: EnigmaConfig -> String -> String
showEnigmaOperationInternal ec msg = showEnigmaOperation_ showEnigmaConfigInternal ec msg



-- Encoding display ==========================================================

-- | Show the conventionally formatted encoding of a mssage by an (initial) Enigma machine configuration.
--
--   >>> let cfg = configEnigma "c-β-V-VI-VIII" "CDTJ" "AE.BF.CM.DQ.HU.JN.LX.PR.SZ.VW" "05.16.05.12"
--   >>> putStr $ showEnigmaEncoding cfg "FOLGENDES IST SOFORT BEKANNTZUGEBEN"
--   RBBF PMHP HGCZ XTDY GAHG UFXG EWKB LKGJ
showEnigmaEncoding :: EnigmaConfig -> String -> String
showEnigmaEncoding ec msg = postproc $ enigmaEncoding ec (preproc msg)

