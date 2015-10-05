module Main where

import Test.HUnit
import System.Exit
import Crypto.Enigma
import Data.List (sort)

{-# ANN module ("HLint: ignore Use mappend"::String) #-}



testRotorNames :: Test
testRotorNames = TestCase $ assertEqual "Invalid rotor list"
        (sort ["I","II","III","IV","V","VI","VII","VIII","\946","\947"])
        (sort rotors)

testReflectorNames :: Test
testReflectorNames = TestCase $ assertEqual "Invalid reflector list"
        (sort ["A","B","C","b","c"])
        (sort reflectors)

testWindowsInstantiation :: String -> String -> String -> String -> Test
testWindowsInstantiation rots winds plug rings = TestCase $ assertEqual ("Invalid windows from instantiation for " ++ show cfg)
        winds
        (windows cfg)
                where cfg = configEnigma rots winds plug rings

testReadShowIsNoOp :: EnigmaConfig -> Test
testReadShowIsNoOp cfg = TestCase $ assertEqual ("Read/Show implementation not compliant for " ++ show cfg)
        cfg
        (read (show cfg) :: EnigmaConfig)

testPlugboardIsOwnInverse :: Name -> Message -> Test
testPlugboardIsOwnInverse plugs msg = TestCase $ assertEqual ("Plugboard is not self-inverse for " ++ plugs)
        msg
        (enigmaEncoding (configEnigma "----" "AAAA" plugs "01.01.01.01") msg)

testWindowsStepping :: EnigmaConfig -> [String] -> Test
testWindowsStepping cfg windss = TestCase $ assertEqual ("Incorrect series of window letters for " ++ show cfg)
        windss
        (take 500 $ map windows $ iterate step cfg)

testStageMappings :: EnigmaConfig -> [Mapping] -> Test
testStageMappings cfg maps = TestCase $ assertEqual ("Incorrect mappings for " ++ show cfg)
        maps
        (stageMappingList cfg)

-- See: http://www.enigma.hoerenberg.com/index.php?cat=The%20U534%20messages
testHistoricalMessage :: String -> EnigmaConfig -> Message -> String -> Test
testHistoricalMessage hmn cfg msg enc = TestCase $ assertEqual ("Error processing historical message " ++ hmn)
        enc
        (enigmaEncoding cfg msg)

testTest :: Test
testTest = TestCase $ assertEqual "Should be one" 1 1


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
                testReadShowIsNoOp (configEnigma "b-β-VI-I-III" "AIQM" "AI.JF.DQ.HU.LX.PR.SZ" "22.17.04.22"),
                testReadShowIsNoOp (configEnigma "C-I-II-III" "PLA" "~" "15.01.06"),
                testPlugboardIsOwnInverse "AI.JF.CM.DQ.HU.LX.PR.SZ" "UNCHANGEDUNCHANGEDUNCHANGED",
                testPlugboardIsOwnInverse "EL.MU.XZ.PW.HY.OF" "THESAMETHESAMETHESAMETHESAMETHESAMETHESAMETHESAMETHESAME",
                testStageMappings (configEnigma "c-γ-V-I-II" "LIAQ" "AI.JF.CM.DQ.HU.LX.PR.SZ" "03.01.04.22")
                        ["IBMQEJGUAFKXCNORDPZTHVWLYS","DKATJFOIPXNWZCGQMBYRHVLESU","UFMHNPIOJGTYCQWRZBKAXVSDLE","MHKVFZDPSOEBIGXWUCNRTJYALQ","YDSKZPTNCHGQOMXAUWJFBRELVI","RDOBJNTKVEHMLFCWZAXGYIPSUQ","PUIBWTKJZSDXNHMFLVCGQYROAE","XLRGKENBMVCYASJHZTIUQDPOWF","TRMXZBJDGISYCEHFNPWKAVOULQ","CRNAXFOUHEBWQKGIPTYDZVLJSM","IBMQEJGUAFKXCNORDPZTHVWLYS"],
                testWindowsStepping (configEnigma "B-III-I-II" "AAA" "" "01.01.01.01")
                        ["AAA","AAB","AAC","AAD","AAE","ABF","ABG","ABH","ABI","ABJ","ABK","ABL","ABM","ABN","ABO","ABP","ABQ","ABR","ABS","ABT","ABU","ABV","ABW","ABX","ABY","ABZ","ABA","ABB","ABC","ABD","ABE","ACF","ACG","ACH","ACI","ACJ","ACK","ACL","ACM","ACN","ACO","ACP","ACQ","ACR","ACS","ACT","ACU","ACV","ACW","ACX","ACY","ACZ","ACA","ACB","ACC","ACD","ACE","ADF","ADG","ADH","ADI","ADJ","ADK","ADL","ADM","ADN","ADO","ADP","ADQ","ADR","ADS","ADT","ADU","ADV","ADW","ADX","ADY","ADZ","ADA","ADB","ADC","ADD","ADE","AEF","AEG","AEH","AEI","AEJ","AEK","AEL","AEM","AEN","AEO","AEP","AEQ","AER","AES","AET","AEU","AEV","AEW","AEX","AEY","AEZ","AEA","AEB","AEC","AED","AEE","AFF","AFG","AFH","AFI","AFJ","AFK","AFL","AFM","AFN","AFO","AFP","AFQ","AFR","AFS","AFT","AFU","AFV","AFW","AFX","AFY","AFZ","AFA","AFB","AFC","AFD","AFE","AGF","AGG","AGH","AGI","AGJ","AGK","AGL","AGM","AGN","AGO","AGP","AGQ","AGR","AGS","AGT","AGU","AGV","AGW","AGX","AGY","AGZ","AGA","AGB","AGC","AGD","AGE","AHF","AHG","AHH","AHI","AHJ","AHK","AHL","AHM","AHN","AHO","AHP","AHQ","AHR","AHS","AHT","AHU","AHV","AHW","AHX","AHY","AHZ","AHA","AHB","AHC","AHD","AHE","AIF","AIG","AIH","AII","AIJ","AIK","AIL","AIM","AIN","AIO","AIP","AIQ","AIR","AIS","AIT","AIU","AIV","AIW","AIX","AIY","AIZ","AIA","AIB","AIC","AID","AIE","AJF","AJG","AJH","AJI","AJJ","AJK","AJL","AJM","AJN","AJO","AJP","AJQ","AJR","AJS","AJT","AJU","AJV","AJW","AJX","AJY","AJZ","AJA","AJB","AJC","AJD","AJE","AKF","AKG","AKH","AKI","AKJ","AKK","AKL","AKM","AKN","AKO","AKP","AKQ","AKR","AKS","AKT","AKU","AKV","AKW","AKX","AKY","AKZ","AKA","AKB","AKC","AKD","AKE","ALF","ALG","ALH","ALI","ALJ","ALK","ALL","ALM","ALN","ALO","ALP","ALQ","ALR","ALS","ALT","ALU","ALV","ALW","ALX","ALY","ALZ","ALA","ALB","ALC","ALD","ALE","AMF","AMG","AMH","AMI","AMJ","AMK","AML","AMM","AMN","AMO","AMP","AMQ","AMR","AMS","AMT","AMU","AMV","AMW","AMX","AMY","AMZ","AMA","AMB","AMC","AMD","AME","ANF","ANG","ANH","ANI","ANJ","ANK","ANL","ANM","ANN","ANO","ANP","ANQ","ANR","ANS","ANT","ANU","ANV","ANW","ANX","ANY","ANZ","ANA","ANB","ANC","AND","ANE","AOF","AOG","AOH","AOI","AOJ","AOK","AOL","AOM","AON","AOO","AOP","AOQ","AOR","AOS","AOT","AOU","AOV","AOW","AOX","AOY","AOZ","AOA","AOB","AOC","AOD","AOE","APF","APG","APH","API","APJ","APK","APL","APM","APN","APO","APP","APQ","APR","APS","APT","APU","APV","APW","APX","APY","APZ","APA","APB","APC","APD","APE","AQF","BRG","BRH","BRI","BRJ","BRK","BRL","BRM","BRN","BRO","BRP","BRQ","BRR","BRS","BRT","BRU","BRV","BRW","BRX","BRY","BRZ","BRA","BRB","BRC","BRD","BRE","BSF","BSG","BSH","BSI","BSJ","BSK","BSL","BSM","BSN","BSO","BSP","BSQ","BSR","BSS","BST","BSU","BSV","BSW","BSX","BSY","BSZ","BSA","BSB","BSC","BSD","BSE","BTF","BTG","BTH","BTI","BTJ","BTK","BTL","BTM","BTN","BTO","BTP","BTQ","BTR","BTS","BTT","BTU","BTV","BTW","BTX","BTY","BTZ","BTA","BTB","BTC","BTD","BTE","BUF","BUG","BUH","BUI","BUJ","BUK","BUL","BUM","BUN","BUO","BUP","BUQ","BUR","BUS","BUT","BUU","BUV","BUW","BUX","BUY","BUZ","BUA","BUB","BUC","BUD","BUE","BVF"],
                testWindowsStepping (configEnigma "c-γ-V-I-II" "LIAQ" "AI.JF.CM.DQ.HU.LX.PR.SZ" "03.01.04.22")
                        ["LIAQ","LIAR","LIAS","LIAT","LIAU","LIAV","LIAW","LIAX","LIAY","LIAZ","LIAA","LIAB","LIAC","LIAD","LIAE","LIBF","LIBG","LIBH","LIBI","LIBJ","LIBK","LIBL","LIBM","LIBN","LIBO","LIBP","LIBQ","LIBR","LIBS","LIBT","LIBU","LIBV","LIBW","LIBX","LIBY","LIBZ","LIBA","LIBB","LIBC","LIBD","LIBE","LICF","LICG","LICH","LICI","LICJ","LICK","LICL","LICM","LICN","LICO","LICP","LICQ","LICR","LICS","LICT","LICU","LICV","LICW","LICX","LICY","LICZ","LICA","LICB","LICC","LICD","LICE","LIDF","LIDG","LIDH","LIDI","LIDJ","LIDK","LIDL","LIDM","LIDN","LIDO","LIDP","LIDQ","LIDR","LIDS","LIDT","LIDU","LIDV","LIDW","LIDX","LIDY","LIDZ","LIDA","LIDB","LIDC","LIDD","LIDE","LIEF","LIEG","LIEH","LIEI","LIEJ","LIEK","LIEL","LIEM","LIEN","LIEO","LIEP","LIEQ","LIER","LIES","LIET","LIEU","LIEV","LIEW","LIEX","LIEY","LIEZ","LIEA","LIEB","LIEC","LIED","LIEE","LIFF","LIFG","LIFH","LIFI","LIFJ","LIFK","LIFL","LIFM","LIFN","LIFO","LIFP","LIFQ","LIFR","LIFS","LIFT","LIFU","LIFV","LIFW","LIFX","LIFY","LIFZ","LIFA","LIFB","LIFC","LIFD","LIFE","LIGF","LIGG","LIGH","LIGI","LIGJ","LIGK","LIGL","LIGM","LIGN","LIGO","LIGP","LIGQ","LIGR","LIGS","LIGT","LIGU","LIGV","LIGW","LIGX","LIGY","LIGZ","LIGA","LIGB","LIGC","LIGD","LIGE","LIHF","LIHG","LIHH","LIHI","LIHJ","LIHK","LIHL","LIHM","LIHN","LIHO","LIHP","LIHQ","LIHR","LIHS","LIHT","LIHU","LIHV","LIHW","LIHX","LIHY","LIHZ","LIHA","LIHB","LIHC","LIHD","LIHE","LIIF","LIIG","LIIH","LIII","LIIJ","LIIK","LIIL","LIIM","LIIN","LIIO","LIIP","LIIQ","LIIR","LIIS","LIIT","LIIU","LIIV","LIIW","LIIX","LIIY","LIIZ","LIIA","LIIB","LIIC","LIID","LIIE","LIJF","LIJG","LIJH","LIJI","LIJJ","LIJK","LIJL","LIJM","LIJN","LIJO","LIJP","LIJQ","LIJR","LIJS","LIJT","LIJU","LIJV","LIJW","LIJX","LIJY","LIJZ","LIJA","LIJB","LIJC","LIJD","LIJE","LIKF","LIKG","LIKH","LIKI","LIKJ","LIKK","LIKL","LIKM","LIKN","LIKO","LIKP","LIKQ","LIKR","LIKS","LIKT","LIKU","LIKV","LIKW","LIKX","LIKY","LIKZ","LIKA","LIKB","LIKC","LIKD","LIKE","LILF","LILG","LILH","LILI","LILJ","LILK","LILL","LILM","LILN","LILO","LILP","LILQ","LILR","LILS","LILT","LILU","LILV","LILW","LILX","LILY","LILZ","LILA","LILB","LILC","LILD","LILE","LIMF","LIMG","LIMH","LIMI","LIMJ","LIMK","LIML","LIMM","LIMN","LIMO","LIMP","LIMQ","LIMR","LIMS","LIMT","LIMU","LIMV","LIMW","LIMX","LIMY","LIMZ","LIMA","LIMB","LIMC","LIMD","LIME","LINF","LING","LINH","LINI","LINJ","LINK","LINL","LINM","LINN","LINO","LINP","LINQ","LINR","LINS","LINT","LINU","LINV","LINW","LINX","LINY","LINZ","LINA","LINB","LINC","LIND","LINE","LIOF","LIOG","LIOH","LIOI","LIOJ","LIOK","LIOL","LIOM","LION","LIOO","LIOP","LIOQ","LIOR","LIOS","LIOT","LIOU","LIOV","LIOW","LIOX","LIOY","LIOZ","LIOA","LIOB","LIOC","LIOD","LIOE","LIPF","LIPG","LIPH","LIPI","LIPJ","LIPK","LIPL","LIPM","LIPN","LIPO","LIPP","LIPQ","LIPR","LIPS","LIPT","LIPU","LIPV","LIPW","LIPX","LIPY","LIPZ","LIPA","LIPB","LIPC","LIPD","LIPE","LIQF","LJRG","LJRH","LJRI","LJRJ","LJRK","LJRL","LJRM","LJRN","LJRO","LJRP","LJRQ","LJRR","LJRS","LJRT","LJRU","LJRV","LJRW","LJRX","LJRY","LJRZ","LJRA","LJRB","LJRC","LJRD","LJRE","LJSF","LJSG","LJSH","LJSI","LJSJ","LJSK","LJSL","LJSM","LJSN","LJSO","LJSP","LJSQ","LJSR","LJSS","LJST","LJSU","LJSV","LJSW","LJSX","LJSY","LJSZ","LJSA","LJSB","LJSC","LJSD","LJSE","LJTF","LJTG","LJTH","LJTI","LJTJ","LJTK","LJTL","LJTM","LJTN","LJTO","LJTP","LJTQ","LJTR","LJTS","LJTT","LJTU","LJTV","LJTW","LJTX","LJTY","LJTZ","LJTA","LJTB","LJTC","LJTD","LJTE","LJUF","LJUG","LJUH","LJUI","LJUJ","LJUK","LJUL","LJUM","LJUN","LJUO","LJUP","LJUQ","LJUR","LJUS","LJUT","LJUU","LJUV"],
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
                 testTest]
        if (errors results + failures results == 0) then
                exitWith ExitSuccess
        else
                exitWith (ExitFailure 1)