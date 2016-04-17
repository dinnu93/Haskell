main = do
  putStrLn "Enter the file path that you need a Word Count of"
  path <- getLine
  file <- readFile path
  putStr $ (show $ lineCount file) ++ " "
  putStr $ (show $ wordCount file) ++ " "
  putStr $ (show $ charCount file) ++ "\n"
  

lineCount :: String -> Int
lineCount = length . lines

wordCount :: String -> Int
wordCount = length . words

charCount :: String -> Int
charCount = length
