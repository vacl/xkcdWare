import System.Environment (getArgs)
import System.IO
import XKCDGenerator (generatePP)

main = do
  args <- getArgs
  let (n, file) = parse args
  handle <- openFile file ReadMode
  words <- hGetContents handle
  pp   <- generatePP (lines words) n
  putStrLn pp
  hClose handle

parse :: [String] -> (Int, String)
parse (n:[]) = (read n, "wordlists/default")
parse (n:file:[]) = (read n, file)
parse _ = error("Usage: xkcdWare <n> [filename]")

