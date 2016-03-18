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

-- Merge Sort

-- Merge step for two sorted lists

merge :: Ord a => [a] -> [a] -> [a]
merge [] ls = ls
merge ls [] = ls
merge xs@(x:xss) ys@(y:yss)
  | x < y = x : merge xss ys
  | otherwise = y : merge xs yss

-- Sort Step
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort ls = merge (mergeSort firstHalf) (mergeSort secondHalf)
  where halfLen = ceiling $ (fromIntegral $ length ls)/2
        firstHalf = map (ls !!) [0..halfLen-1]
        secondHalf = map (ls !!) [halfLen..(length ls)-1]

        
-- Quick Sort

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort (filter (<x) xs) ++ [x] ++ quickSort (filter (>x) xs) 
  
