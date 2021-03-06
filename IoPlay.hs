import Data.Char

main = (++) <$> getLine <*> getLine 

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter ((<10) . length) .lines 
  
caps :: String -> String
caps = map toUpper

respPalindrome :: String -> String
respPalindrome = unlines . map isPalindrome . lines
  where isPalindrome s = show $ s == reverse s
