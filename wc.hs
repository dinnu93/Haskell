import System.Environment

main = do
  args <- getArgs
  file <- readFile $ args !! 0
  putStr $ " " ++ (show $ lineCount file) ++ " "
  putStr $ (show $ wordCount file) ++ " "
  putStr $ (show $ charCount file) ++ " "
  putStrLn $ args !! 0 

lineCount :: String -> Int
lineCount = length . lines

wordCount :: String -> Int
wordCount = length . words

charCount :: String -> Int
charCount = length
