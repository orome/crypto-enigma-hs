module Main where

import Test.HUnit
import System.Exit
import Crypto.Enigma
import Data.List (sort)



-- See: http://www.enigma.hoerenberg.com/index.php?cat=The%20U534%20messages
testHistoricalMessage :: String -> EnigmaConfig -> Message -> String -> Test
testHistoricalMessage n cfg msg enc = TestCase $ assertEqual ("Error processing historical message " ++ n)
        enc
        (enigmaEncoding cfg msg)

testRotorNames :: Test
testRotorNames = TestCase $ assertEqual "Invalid rotor list"
        (sort ["I","II","III","IV","V","VI","VII","VIII","\946","\947"])
        (sort rotors)

testReflectorNames :: Test
testReflectorNames = TestCase $ assertEqual "Invalid reflector list"
        (sort ["A","B","C","b","c"])
        (sort reflectors)

testWindowsInstantiation :: String -> String -> String -> String -> Test
testWindowsInstantiation rots winds plug rings = TestCase $ assertEqual "Invalid windows from instantiation"
        winds
        (windows $ configEnigma rots winds plug rings)

test1 :: Test
test1 = TestCase $ assertEqual "Should be one" 1 1

test2 :: Test
test2 = TestCase $ assertEqual "Shold both be zero" 0 0

main :: IO ()
main = do
        -- print rotors
        -- print reflectors
        results <- runTestTT $ TestList [
                testRotorNames,
                testReflectorNames,
                testWindowsInstantiation "b-β-V-VIII-II" "XQVI" "UX.MO.KZ.AY.EF.PL" "03.17.24.11",
                testWindowsInstantiation "C-V-VIII-III" "MUM" "AY.EF.PL" "09.16.24",
                testWindowsInstantiation "c-β-VIII-VII-IV" "LMOI" "" "21.01.19.01",
                testHistoricalMessage "U534-P1030662"
                        (configEnigma "c-β-V-VI-VIII" "WIIJ" "AE.BF.CM.DQ.HU.JN.LX.PR.SZ.VW" "01.01.05.12")
                        "UUUVIRSIBENNULEINSYNACHRXUUUSTUETZPUNKTLUEBECKVVVCHEFVIERXUUUFLOTTXXMITUUUVIERSIBENNULZWOUNDUUUVIERSIBENNULDREIZURFLENDERWERFTLUEBECKGEHENXFONDORTFOLGTWEITERESX"
                        "LIRZMLWRCDMSNKLKBEBHRMFQFEQAZWXBGBIEXJPYFCQAAWSEKDEACOHDZKCZTOVSYHFNSCMAIMIMMAVJNLFXEWNPUIRINOZNCRVDHCGKCYRVUJQPVKEUIVVXGLQMKRJMDMLXLLRLYBKJWRXBQRZWGCCNDOPMGCKJ",
                 testHistoricalMessage "U534-P1030694"
                         (configEnigma "b-γ-IV-III-VIII" "RCPO" "CH.EJ.NV.OU.TY.LG.SZ.PK.DI.QB" "01.01.03.21")
                         "PLLEVONVONZEHNXSIDIXXHAFENWARNEMUENDEFEINDBESETZTCHV"
                         "VBBHSYTWZEEDGKYCAKYVWBWUUZVZIGCTBZLZYUHYWILFYUPBIPCM",
                 testHistoricalMessage "U534-P1030681 (Dönitz)"
                         (configEnigma "c-β-V-VI-VIII" (enigmaEncoding (configEnigma "c-β-V-VI-VIII" "NAEM" "AE.BF.CM.DQ.HU.JN.LX.PR.SZ.VW" "05.16.05.12") "QEOB") "AE.BF.CM.DQ.HU.JN.LX.PR.SZ.VW" "05.16.05.12")
                         "KRKRALLEXXFOLGENDESISTSOFORTBEKANNTZUGEBENXXICHHABEFOLGELNBEBEFEHLERHALTENXXJANSTERLEDESBISHERIGXNREICHSMARSCHALLSJGOERINGJSETZTDERFUEHRERSIEYHVRRGRZSSADMIRALYALSSEINENNACHFOLGEREINXSCHRIFTLSCHEVOLLMACHTUNTERWEGSXABSOFORTSOLLENSIESAEMTLICHEMASSNAHMENVERFUEGENYDIESICHAUSDERGEGENWAERTIGENLAGEERGEBENXGEZXREICHSLEITEIKKTULPEKKJBORMANNJXXOBXDXMMMDURNHFKSTXKOMXADMXUUUBOOIEXKP"
                         "LANOTCTOUARBBFPMHPHGCZXTDYGAHGUFXGEWKBLKGJWLQXXTGPJJAVTOCKZFSLPPQIHZFXOEBWIIEKFZLCLOAQJULJOYHSSMBBGWHZANVOIIPYRBRTDJQDJJOQKCXWDNBBTYVXLYTAPGVEATXSONPNYNQFUDBBHHVWEPYEYDOHNLXKZDNWRHDUWUJUMWWVIIWZXIVIUQDRHYMNCYEFUAPNHOTKHKGDNPSAKNUAGHJZSMJBMHVTREQEDGXHLZWIFUSKDQVELNMIMITHBHDBWVHDFYHJOQIHORTDJDBWXEMEAYXGYQXOHFDMYUXXNOJAZRSGHPLWMLRECWWUTLRTTVLBHYOORGLGOWUXNXHMHYFAACQEKTHSJW",
                test1, test2]
        if (errors results + failures results == 0) then
                exitWith ExitSuccess
        else
                exitWith (ExitFailure 1)