main = do
  putStrLn "Enter the file path that you need a Word Count of"
  path <- getLine
  file <- readFile path
  putStrLn $ "Line Count is : " ++ (show $ length $ lines file)
  putStrLn $ "Word Count is : " ++ (show $ wordCount file)
  putStrLn $ "Character Count : " ++ (show $ charCount file)
  

lineCount :: String -> Int
lineCount = length .lines

wordCount :: String -> Int
wordCount = sum . map length . map words . lines

charCount :: String -> Int
charCount = length 
