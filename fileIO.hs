import System.IO
import Data.List
import System.Random

main = do
  todoItem <- getLine
  appendFile "todo.txt" (todoItem ++ "\n")
  
    
 
