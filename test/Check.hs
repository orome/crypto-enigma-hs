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
                                      "UX.MO.KZ.AY.EF.PL"
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
        putStrLn "\nQuickCheck Tests"
        -- sample (arbitrary :: Gen EnigmaConfig)
        -- sample (arbitrary :: Gen Message)
        -- verboseCheck prop_ReadShowIsNoOp
        putStrLn "\nQuickCheck - read.show is id:"
        result <- quickCheckWithResult stdArgs { maxSuccess = 100, chatty = True }  prop_ReadShowIsNoOp
        unless (isSuccess result) exitFailure
        -- quickCheckWith stdArgs { maxSuccess = 500 } prop_ReadShowIsNoOp
        putStrLn "\nQuickCheck - encoding of encoding is message:"
        result <- quickCheckWithResult stdArgs { maxSuccess = 100, chatty = True } prop_EncodeEncodeIsMessage
        unless (isSuccess result) exitFailure
        putStrLn "\n\n"
