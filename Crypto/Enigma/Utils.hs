{-# OPTIONS_HADDOCK hide, prune #-}
{-|
Module      : Crypto.Enigma.Utils
-}
module Crypto.Enigma.Utils where

import Data.Char (chr, ord)
import Data.List (sort)


-- Some character utilities --------------------------------------------------

-- REV - Could make this [MsgChar]
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
encode e ' ' = ' '
encode e ch = e !! (numA0 ch)

-- standard simple-substitution cypher encoding
encode' :: String -> String -> String
encode' e s = (encode e) <$> s
