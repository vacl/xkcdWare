module XKCDGenerator
( calcEntropyBits
, generatePP
) where

import Data.Char (toUpper)
import System.Random (randomRIO)

calcEntropyBits :: (Integral a) => [String] -> a -> a
calcEntropyBits [] _ = 0
calcEntropyBits words n
  | n <= 0    = 0
  | otherwise = floor $ logBase 2 combinations
  where
    combinations = fromIntegral $ flip (^) n $ toInteger $ length words

generatePP :: (Integral a) => [String] -> a -> IO String
generatePP words n
  | n <= 0    = return ""
  | otherwise = do
      cur    <- randElem words
      others <- generatePP words (n-1)
      return $ (upperCamel cur) ++ others

upperCamel :: String -> String
upperCamel ""     = ""
upperCamel (c:cs) = (toUpper c) : cs

randElem :: [a] -> IO a
randElem ls = fmap (ls!!) randIdx
  where randIdx = randomRIO (0, (length ls) - 1)

