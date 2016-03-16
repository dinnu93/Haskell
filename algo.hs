-- Implementation of basic algorithms in Haskell

-- Insertion Sort

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x $ insertionSort xs

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
          | x < y = x : y : ys
          | otherwise = y : insert x ys
            
  

-- Quick Sort

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort (filter (<x) xs) ++ [x] ++ quickSort (filter (>x) xs) 
  
