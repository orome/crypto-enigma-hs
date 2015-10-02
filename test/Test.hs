module Main where

import Test.HUnit
import System.Exit
import Crypto.Enigma


test1 = TestCase $ assertEqual "Should be one" 1 1

test2 :: Test
test2 = TestCase $ assertEqual "Shold both be zero" 0 0

test3 :: Test
test3 = TestCase $ assertEqual "Should be same"
        "XQVI"
        (windows $ configEnigma "b-β-V-VIII-II" "XQVI" "UX.MO.KZ.AY.EF.PL" "03.17.24.11")

-- main :: IO Counts
-- main = runTestTT $ TestList [test1, test2, test1, test3]

main :: IO Counts
main = do
  results <- runTestTT $ TestList [test1, test2, test3, test3]
  if (errors results + failures results == 0)
    then
      exitWith ExitSuccess
    else
      exitWith (ExitFailure 1)