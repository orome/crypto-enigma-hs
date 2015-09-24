module Main where

import System.IO
import           Data.List

import Crypto.Enigma
import Crypto.Enigma.Display

-- TBD - Placeholder
main :: IO ()
main = do
        hSetBuffering stdout NoBuffering

        msg <- getLine
        let cfg = configEnigma "b-γ-V-VIII-II" "LEZO" "UX.MO.KZ.AY.EF.PL" "03.17.04.11"
        putStr $ showEnigmaOperationInternal cfg msg
        putStr $ showEnigmaOperation cfg msg
        putStr $ showEnigmaEncoding cfg msg

        let cfg' = configEnigma "c-β-V-VI-VIII" "NAEM" "AE.BF.CM.DQ.HU.JN.LX.PR.SZ.VW" "05.16.05.12"
        let cfg = configEnigma "c-β-V-VI-VIII" (enigmaEncoding cfg' "QEOB") "AE.BF.CM.DQ.HU.JN.LX.PR.SZ.VW" "05.16.05.12"
        let msg = "KRKR ALLE XX FOLGENDES IST SOFORT BEKANNTZUGEBEN XX ICH HABE FOLGELNBE BEFEHL ERHALTEN XX J ANSTERLE DES BISHERIGXN REICHSMARSCHALLS J GOERING J SETZT DER FUEHRER SIE Y HVRR GRZSSADMIRAL Y ALS SEINEN NACHFOLGER EIN X SCHRIFTLSCHE VOLLMACHT UNTERWEGS X ABSOFORT SOLLEN SIE SAEMTLICHE MASSNAHMEN VERFUEGEN Y DIE SICH AUS DER GEGENWAERTIGEN LAGE ERGEBEN X GEZ X REICHSLEITEI KK TULPE KK J BORMANN J XX OB.D.MMM DURNH FKST.KOM.ADM.UUU BOOIE.KP"
        print $ cfg'
        print $ cfg
        putStr $ showEnigmaEncoding cfg msg
