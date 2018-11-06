{-# OPTIONS_HADDOCK hide, prune #-}
{-|
Module      : Crypto.Enigma.Utils
-}

{-# LANGUAGE Safe, CPP #-}
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


-- REV - Idiomaic approach to general conditional redefinitions? <<<
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:errorWithoutStackTrace
#if __GLASGOW_HASKELL__ < 800
error' = error
#else
error' = errorWithoutStackTrace
#endif