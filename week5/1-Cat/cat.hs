import System.Environment

main = do
  (filePath:_) <- getArgs
  contents <- readFile filePath
  putStrLn contents
