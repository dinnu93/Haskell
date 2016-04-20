import System.IO
import Data.List

main = do
  todoItem <- getLine
  appendFile "todo.txt" (todoItem ++ "\n")
  
    
 
