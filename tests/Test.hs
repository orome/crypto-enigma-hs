import           Crypto.Enigma
import           System.Exit   (exitFailure, exitSuccess)

-- TBD - Switch to detailed-0.9 <<<

cfg :: EnigmaConfig
cfg = read "c-γ-V-I-II XMGS UX.MO.KZ.AY.EF.PL 03.17.04.01"
--cfg' = read "c-γ-V-I-VII XMGS UX.MO.KZ.AY.EF.PL 03.17.04.01" :: EnigmaConfig

s :: String
s = "b-γ-V-I-VI XMGS UX.MO.KZ.AY.EF.PL 17.11.04.01"

test1X :: Bool
test1X = s == show (read s :: EnigmaConfig)

test2X :: Bool
test2X = cfg == (read $ show cfg :: EnigmaConfig)

-- testZZ :: Bool
-- testZZ = cfg' == (read $ show cfg :: EnigmaConfig)

runTest :: Bool
runTest =
        test1X &&
        test2X

-- TBD - Placeholder
main :: IO ()
main = if runTest then exitSuccess else exitFailure
