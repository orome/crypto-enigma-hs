module Main where

-- import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import Control.Monad (unless, replicateM)
import System.Exit
import Data.List (sort,intercalate)
import Text.Printf (printf)

import Crypto.Enigma
import Crypto.Enigma.Display

{-# ANN module ("HLint: ignore Use mappend"::String) #-}


capitals = ['A'..'Z']
badchars = " .,'><!?-:()1234567890" ++ ['a'..'z'] ++  "$@&^=|~"
anychars = capitals ++ badchars


-- ASK - Can I provide the number of stages as an argument (parameterize) ? <<<
-- TBD - Consistent formatting and labeling of tests <<<
instance Arbitrary EnigmaConfig where
        arbitrary = do
                nc <- choose (3,4)  -- This could cover a wider range
                ws <- replicateM nc (elements capitals)
                cs <- replicateM nc (elements rotors)
                uk <- elements reflectors
                rs <- replicateM nc (choose (1,26))
--                 Positive x <- arbitrary
--                 Positive y <- arbitrary
                return $ configEnigma (intercalate "-" (uk:cs))
                                      ws
                                      "UX.MO.KZ.AY.EF.PL"  -- TBD - Generate plugboard and test <<<
                                      (intercalate "." $ (printf "%02d") <$> (rs :: [Int]))

data BadChar = BadChar {chr :: Char} deriving Show
instance Arbitrary BadChar where
    arbitrary = do
        f <-  elements badchars
        return $ BadChar f

prop_ReadShowIsNoOp :: EnigmaConfig -> Bool
prop_ReadShowIsNoOp cfg = cfg == (read (show cfg) :: EnigmaConfig)

prop_EncodeEncodeIsMessage :: EnigmaConfig -> String -> Bool
prop_EncodeEncodeIsMessage cfg str = enigmaEncoding cfg (enigmaEncoding cfg str) == message str

prop_NoEncodeIsMessage :: String -> Bool
prop_NoEncodeIsMessage str = enigmaEncoding (configEnigma "----" "AAAA" "" "01.01.01.01") str == message str

prop_BadCharIsBlankInConfig :: EnigmaConfig -> BadChar -> Bool
prop_BadCharIsBlankInConfig cfg bchr = showEnigmaConfig cfg (chr bchr) == showEnigmaConfig cfg ' '

-- TBD - Confirm that this actually tests ==
prop_StepEqual :: EnigmaConfig -> Bool
prop_StepEqual cfg = step cfg == step cfg

-- TBD - Add test of stepped encoding start (see test_encoding_stepped_start) in Python version <<<

main :: IO ()
main = do
        putStrLn "\n==== QuickCheck Tests"
        putStrLn "\nExample EnigmaConfig test values:"
        sample (arbitrary :: Gen EnigmaConfig)
        sample (arbitrary :: Gen EnigmaConfig)
        putStrLn "\nExample Message test values:"
        sample (arbitrary :: Gen Message)
        putStrLn "\nQuickCheck - read.show is id:"
        result <- verboseCheckWithResult stdArgs { maxSuccess = 10, chatty = True } prop_ReadShowIsNoOp
        unless (isSuccess result) exitFailure
        result <- quickCheckWithResult stdArgs { maxSuccess = 200, chatty = True } prop_ReadShowIsNoOp
        unless (isSuccess result) exitFailure
        putStrLn "\nQuickCheck - encoding of encoding is message:"
        result <- verboseCheckWithResult stdArgs { maxSuccess = 5, chatty = True } prop_EncodeEncodeIsMessage
        unless (isSuccess result) exitFailure
        result <- quickCheckWithResult stdArgs { maxSuccess = 100, chatty = True } prop_EncodeEncodeIsMessage
        unless (isSuccess result) exitFailure
        putStrLn "\nQuickCheck - no-op incoding is message:"
        result <- verboseCheckWithResult stdArgs { maxSuccess = 5, chatty = True } prop_NoEncodeIsMessage
        unless (isSuccess result) exitFailure
        result <- quickCheckWithResult stdArgs { maxSuccess = 100, chatty = True } prop_NoEncodeIsMessage
        unless (isSuccess result) exitFailure
        putStrLn "\nQuickCheck - bad chars blank in config:"
        result <- verboseCheckWithResult stdArgs { maxSuccess = 10, chatty = True } prop_BadCharIsBlankInConfig
        unless (isSuccess result) exitFailure
        result <- quickCheckWithResult stdArgs { maxSuccess = 200, chatty = True } prop_BadCharIsBlankInConfig
        unless (isSuccess result) exitFailure
        putStrLn "\nQuickCheck - step once is equal:"
        result <- verboseCheckWithResult stdArgs { maxSuccess = 10, chatty = True } prop_StepEqual
        unless (isSuccess result) exitFailure
        result <- quickCheckWithResult stdArgs { maxSuccess = 200, chatty = True } prop_StepEqual
        unless (isSuccess result) exitFailure
        putStrLn "\n"
