module XKCDGenerator
( generatePP
) where

import Data.Char (toUpper)
import System.Random (randomRIO)

generatePP :: [String] -> Int -> IO String
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

