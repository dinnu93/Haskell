-- Implementation of basic algorithms in Haskell

-- Insertion Sort

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert $ insertionSort xs
  where insert [] = [x]
        insert (y:ys)
          | x < y = x : y : ys
          | otherwise = y : insert ys
            
  

