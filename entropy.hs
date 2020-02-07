import System.Environment (getArgs)
import System.IO
import XKCDGenerator (calcEntropyBits)

main = do
  args <- getArgs
  let (n, file) = parse args
  handle <- openFile file ReadMode
  words <- hGetContents handle
  putStrLn . show $ calcEntropyBits (lines words) n
  hClose handle

parse :: [String] -> (Int, String)
parse (n:[]) = (read n, "wordlists/default")
parse (n:file:[]) = (read n, file)
parse _ = error("Usage: entropy <n> [filename]")

