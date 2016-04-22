import Data.Char

main = putStrLn "Please enter a number: " >> (readLn >>= (\n -> putStrLn $ show $ n+1)) 

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter ((<10) . length) .lines 
  
caps :: String -> String
caps = map toUpper

respPalindrome :: String -> String
respPalindrome = unlines . map isPalindrome . lines
  where isPalindrome s = show $ s == reverse s
