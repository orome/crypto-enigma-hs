module Main where

import Test.HUnit
import System.Exit
import Crypto.Enigma



testRotorNames :: Test
testRotorNames = TestCase $ assertEqual "Invalid rotor list"
        ["I","II","III","IV","V","VI","VII","VIII","\946","\947"]
        rotors

testReflectorNames :: Test
testReflectorNames = TestCase $ assertEqual "Invalid reflector list"
        ["A","B","C","b","c"]
        reflectors

test1 = TestCase $ assertEqual "Should be one" 1 1

test2 :: Test
test2 = TestCase $ assertEqual "Shold both be zero" 0 0

test3 :: Test
test3 = TestCase $ assertEqual "Should be same"
        "XQVI"
        (windows $ configEnigma "b-β-V-VIII-II" "XQVI" "UX.MO.KZ.AY.EF.PL" "03.17.24.11")


main :: IO ()
main = do
        print rotors
        print reflectors
        results <- runTestTT $ TestList [testRotorNames, testReflectorNames, test1, test2, test3, test3]
        if (errors results + failures results == 0) then
                exitWith ExitSuccess
        else
                exitWith (ExitFailure 1)