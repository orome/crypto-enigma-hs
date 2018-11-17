{-# OPTIONS_HADDOCK hide, prune #-}
{-|
Module      : Crypto.Enigma.Utils
-}

module Crypto.Enigma.Utils where

import Data.Char (chr, ord)
import Data.List (sort)


-- Some character utilities --------------------------------------------------

letters :: String
letters = ['A'..'Z']

numA0 :: Char -> Int
numA0 ch = ord ch - ord 'A'

chrA0 :: Int -> Char
chrA0 i = chr (i + ord 'A')

ordering :: Ord a => [a] -> [Int]
ordering xs = snd <$> sort (zip xs [0..])


-- General encoding logic ----------------------------------------------------

-- encode a single character
encode :: String -> Char -> Char
-- encode e ' ' = ' '
-- encode e ch = e !! (numA0 ch)
encode m ch = if i `elem` [0..(length m)-1] then (m !! i) else ' ' where i = numA0 ch

-- standard simple-substitution cypher encoding
encode' :: String -> String -> String
encode' m s = (encode m) <$> s


-- Patch ANSI escapes --------------------------------------------------------

--  REV: Remove/review patch of escaping: https://github.com/carymrobbins/intellij-haskforce/issues/372 <<<
escape_ mu ch = ("\ESC[") ++ mu ++ [ch] ++ ("\ESC[") ++ "0m"