import qualified Data.List as L

doubleMe :: Int -> Int
doubleMe x = x + x

doubleUs :: Int -> Int -> Int
doubleUs x y = (doubleMe x) + (doubleMe y)

doubleSmallNumber :: Int -> Int
doubleSmallNumber x = if x > 100
                         then x
                         else x*2
                              
doubleSmallNumber' :: Int -> Int
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

boomBangs :: [Int] -> [String]
boomBangs ls = [ if x > 10 then "BANG!" else "BOOM!" | x <- ls, odd x]

length' ::  [a] -> Integer
length' ls = sum [1 | _ <- ls]

square :: Int -> Int
square x = x*x

rightTriangles :: [(Int, Int, Int)]
rightTriangles = [(z,y,x) | x <- [1..10], y <- [1..x],z <- [1..y], y^2 + z^2 == x^2, x + y + z == 24]

factorial :: (Integral a) => a -> a
factorial 0 =  1
factorial n = n * (factorial (n-1))

sumtorial :: (Integral a) => a -> a
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

lucky :: (Eq a,Num a) => a -> String
lucky 7 = "Lucky Number Seven!"
lucky x = "Don't you know my lucky number dummy?"

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

firstLetter :: String -> String
firstLetter [] = "Empty string!"
firstLetter a@(x:_) = "The first letter of " ++ a ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a  -> String
bmiTell weight height
  | bmi <= 18.5 = "You're underweight, Eat something!"
  | bmi <= 25.0 = "You're normal, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Eat less!"
  | otherwise = "You're a whale, Congratulations!"
  where bmi = weight / (height^2) 

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

maxList :: (Ord a) => [a] -> a
maxList [] = error "No max element in the empty list dummy!"
maxList [x] = x
maxList (x:xs) = max' x (maxList xs)

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r^2
  in sideArea + 2 * topArea

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x:replicate' (n-1) x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallSorted = quicksort (filter (<=x) xs)
      bigSorted = quicksort (filter (>x) xs)
  in smallSorted ++ [x] ++ bigSorted

-- Higher Order Functions

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred  = compare 100 

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipwith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipwith' _ [] _ = []
zipwith' _ _ [] = []
zipwith' f (x:xs) (y:ys) = (f x y) : zipwith' f xs ys

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs 

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f acc [] = acc
foldl' f acc (x:xs) =  foldl' f (f acc x) xs

sum' :: (Num a) => ([a] -> a)
sum' = foldl (+) 0 

reverse' :: [a] -> [a]
reverse' ls = foldl (flip (:)) [] ls

elem' :: (Eq a) => a -> [a] -> Bool                         
elem' e (x:xs) = foldl (\acc x -> (x == e) || acc) False xs

sqrtSums :: Int
sqrtSums = (+) 1 $ length $ takeWhile (<=1000) $ scanl1 (+) $ map sqrt [1..] 

factor :: (Integral a) => a -> a -> Bool
factor n x = mod n x == 0 

--Point Free Style Function
negativeMap :: (Num a) => ([a] -> [a])
negativeMap  = map (negate . abs) 

-- Using Data.List Module
uniqElements :: (Eq a) => ([a] -> Int)
uniqElements = length . L.nub

data AJFriends = Dinnu | Pranav | Venkat deriving (Show)

-- and say I have a function like this,

isDinnu :: AJFriends -> Bool
isDinnu Dinnu = True
isDinnu _    = False

pairWiseIntSum :: [[Int]] -> [Int]
pairWiseIntSum intList =  map sum $ L.transpose intList


everyOther :: [a] -> [a]
everyOther [] = []
everyOther ls  = map (ls !!) $ filter even [0 .. (length ls)-1]

ano :: [[Int]] -> [Int]
ano = foldl1 (zipWith (+))

-- Gives list of factors of a given number n under (sqrt n) 
factorList :: (Integral a) => a -> [a]
factorList x = filter (factor x) [2..(floor (sqrt (fromIntegral x)))]

-- Gives nameValue of any name spelled in small letetrs 
nameValue :: String -> Int
nameValue  = sum . (map findChar) 

findChar :: Char -> Int
findChar c
  | c `elem` charList = v
  | otherwise = error "Just enter small letters Dumbass!"
  where Just v =  L.findIndex (== c) charList
        charList = ' ':['a'..'z']

-- Prime number verification
prime :: (Integral a) => a -> Bool
prime x
  | x == 1 = False
  | otherwise = null (factorList x) 

primesBelowN :: (Integral a) => a -> [a]
primesBelowN n = filter prime [1..n]

largestDivisible :: (Integral a) => a
largestDivisible = head (filter (`factor`3829) [10000,9999..])

-- List of Fibonacci numbers until F(n) -> [F(n)..]

fibList :: (Integral a) => a -> [a]
fibList 0 = [0]
fibList 1 = [0,1]
fibList n = lastList ++ [sum (drop (len-2) lastList)]
  where lastList = fibList (n-1)
        len = length lastList

fib :: (Integral a) => (a -> a)
fib = (\n -> last (fibList n))


  
-- Look Up for Collatz Conjecture
hailstone :: (Integral a) => a -> a
hailstone n
  | mod n 2 == 0 = div n 2
  | otherwise = 3*n + 1

hailstoneSeq :: (Integral a) => a -> [a]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)


intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (_:xs) = 1 + intListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo [] = []
sumEveryTwo (x:[]) = [x]
sumEveryTwo (x:(y:zs)) = (x+y) : sumEveryTwo zs

hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1
        
-- Algebraic Data Types : Chapter-2

data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
           deriving Show
             
shoe :: Thing
shoe = Shoe

isSmall :: Thing -> Bool
isSmall Ship = False
isSmall King = False
isSmall _ = True

data FailableDouble = Failure
                    | OK Double
                    deriving Show

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero x  = case x of
                       Failure -> 0
                       OK d -> d

data Person = Person String Int Thing
            deriving Show

dinesh :: Person
dinesh = Person "Dinesh" 22 King

ajjai :: Person
ajjai = Person "Ajjai" 32 Ship

getAge :: Person -> Int
getAge (Person _ a _) = a

getName :: Person -> String
getName (Person a _ _) =  a  

personList = [dinesh, ajjai]

personDesc = [ getName(person) ++ " : "  ++ (show (getAge(person))) | person <- personList]

-- Recursive Data Types

-- Lists

data IntList = Empty | Cons Int IntList

ls :: IntList
ls = (Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty))))

intListSum :: IntList -> Int
intListSum Empty = 0
intListSum (Cons i ls) = i + intListSum ls

intListProduct :: IntList -> Int
intListProduct Empty = 1
intListProduct (Cons i ls) = i * intListProduct ls

-- Trees

data Tree = Leaf Char
          | Node Tree Int Tree
          deriving Show

tree :: Tree
tree = Node (Leaf 'a') 1 (Node (Leaf 'b') 2 (Leaf 'c'))
