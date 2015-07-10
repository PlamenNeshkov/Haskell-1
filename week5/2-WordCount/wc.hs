import System.Environment
import Data.Char

countLines = show . length . lines
countWords = show . length . words
countChars = show . length

type Which = String
count :: Which -> String -> String
count "-l" text = countLines text ++ " lines."
count "-w" text = countWords text ++ " words."
count "-c" text = countChars text ++ " characters."

main = do
  (which:filePath:_) <- getArgs
  contents <- readFile filePath
  putStrLn (count which contents)
