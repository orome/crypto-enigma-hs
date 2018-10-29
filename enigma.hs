import System.Environment
import Data.List

--import Crypto.Enigma.Utils
import Crypto.Enigma
import Crypto.Enigma.Display

main = do
   args@(command:message:other) <- getArgs
   progName <- getProgName

   putStrLn "The program name is:"
   putStrLn progName
   putStrLn "The arguments are:"
   mapM putStrLn args
   putStrLn "The command is:"
   putStrLn command
   putStrLn "The message is:"
   putStrLn message

   let cfg = configEnigma "c-β-V-VI-VIII" "CDTJ" "AE.BF.CM.DQ.HU.JN.LX.PR.SZ.VW" "05.16.05.12"
   putStr $ showEnigmaEncoding cfg "FOLGENDES IST SOFORT BEKANNTZUGEBEN"
   putStrLn $ "RBBF PMHP HGCZ XTDY GAHG UFXG EWKB LKGJ"
   putStr $ showEnigmaEncoding cfg message

