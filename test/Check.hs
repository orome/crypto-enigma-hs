{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
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

capitals = elements ['A'..'Z']

-- TBD - Can I provide the number of stages as an argument (parameterize) ? <<<
-- TBD - Consistent formatting and labeling of tests <<<
-- ASK - Retina figures? <<<
instance Arbitrary EnigmaConfig where
        arbitrary = do
                nc <- choose (3,4)  -- This could cover a wider range
                ws <- replicateM nc capitals
                cs <- replicateM nc (elements rotors)
                uk <- elements reflectors
                rs <- replicateM nc (choose (1,26))
--                 Positive x <- arbitrary
--                 Positive y <- arbitrary
                return $ configEnigma (intercalate "-" (uk:cs))
                                      ws
                                      "UX.MO.KZ.AY.EF.PL"  -- TBD - Generate plugboard and test <<<
                                      (intercalate "." $ (printf "%02d") <$> (rs :: [Int]))

-- REV - Requires TypeSynonymInstances, FlexibleInstances; find a better way <<<
instance Arbitrary Message where
        arbitrary = do
          l <- choose (1,200)
          replicateM l capitals

prop_ReadShowIsNoOp :: EnigmaConfig -> Bool
prop_ReadShowIsNoOp cfg = cfg == (read (show cfg) :: EnigmaConfig)

prop_EncodeEncodeIsMessage :: EnigmaConfig -> Message -> Bool
prop_EncodeEncodeIsMessage cfg msg = enigmaEncoding cfg (enigmaEncoding cfg msg) == msg




main :: IO ()
main = do
        putStrLn "\n==== QuickCheck Tests"
        putStrLn "\nExample EnigmaConfig test values:"
        sample (arbitrary :: Gen EnigmaConfig)
        sample (arbitrary :: Gen EnigmaConfig)
        putStrLn "\nExample Message test values:"
        sample (arbitrary :: Gen Message)
        putStrLn "\nQuickCheck - read.show is id:"
        result <- verboseCheckWithResult stdArgs { maxSuccess = 10, chatty = True }  prop_ReadShowIsNoOp
        unless (isSuccess result) exitFailure
        result <- quickCheckWithResult stdArgs { maxSuccess = 200, chatty = True }  prop_ReadShowIsNoOp
        unless (isSuccess result) exitFailure
        putStrLn "\nQuickCheck - encoding of encoding is message:"
        result <- verboseCheckWithResult stdArgs { maxSuccess = 5, chatty = True } prop_EncodeEncodeIsMessage
        unless (isSuccess result) exitFailure
        result <- quickCheckWithResult stdArgs { maxSuccess = 100, chatty = True } prop_EncodeEncodeIsMessage
        unless (isSuccess result) exitFailure
        putStrLn "\n"
