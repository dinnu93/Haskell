{-# LANGUAGE FlexibleInstances #-}

import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.Function as F
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Applicative
import Data.Array 

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

-- Function using Data Modules

-- Gives nameValue of any name spelled in small letetrs 
nameValue :: String -> Int
nameValue  = sum . (map findChar) 

findChar :: Char -> Int
findChar c 
  | C.isLetter c  =  C.ord (C.toLower c) - C.ord 'a' + 1 
  | otherwise = 0

-- splitWords splits words using <space> as delimeter
splitWords :: String -> [String]
splitWords s = filter (not . any C.isSpace) $ L.groupBy ((==) `F.on` C.isSpace) s


-- Encoder and Decoder functions for simple sentences
encode :: Int -> String -> String
encode shift = map (C.chr . (+ shift) . C.ord)

decode :: Int -> String -> String
decode shift = encode (negate shift) 

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

data IntList = Empty
             | Cons Int IntList
             deriving Show

ls :: IntList
ls = (Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty))))

intListSum :: IntList -> Int
intListSum Empty = 0
intListSum (Cons i ls) = i + intListSum ls

intListProduct :: IntList -> Int
intListProduct Empty = 1
intListProduct (Cons i ls) = i * intListProduct ls

squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons l ls) = Cons (l^2) $ squareAll ls              

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList f (Cons l ls) = Cons (f l) $ mapIntList f ls

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList p (Cons l ls)
  | p l = Cons l $ filterIntList p ls
  | otherwise = filterIntList p ls


-- Polymorphic Data Types

data List t = E
            | C t (List t)
            deriving Show
            
mapList :: (a -> b) -> List a -> List b 
mapList _ E = E
mapList f (C x xs) = C (f x) $ mapList f xs

filterList :: (a -> Bool) -> (List a) -> (List a) 
filterList _ E = E
filterList p (C x xs) 
    | p x = C x $ filterList p xs
    | otherwise = filterList p xs


stringFilter :: [String] -> [String]
stringFilter = filter (C.isUpper . midLetter)   
  where halfLen s = div (length s) 2
        midLetter s
          | even (length s) = (s !! (halfLen s - 1))
          | otherwise = (s !! halfLen s)

-- Maybe Type Constructor Maybe

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

-- Type Classes & Polymorphism

-- adding data type to a type class

data Foo = F Int | G Char

instance Eq Foo where
  (F i1) == (F i2) = i1 == i2
  (G c1) == (G c2) = c1 == c2
  _ == _ = False

  foo1 /= foo2 = not (foo1 == foo2)

-- Eq being a special function we can define the above type class like this

data Foo' = F' Int | G' Char
          deriving (Eq, Ord, Show)

                   
-- Our own type class example

class Listable a where
  toList :: a -> [Int]

instance Listable Int where
  toList x = [x]

instance Listable Bool where
  toList True = [1]
  toList False = [0]

instance Listable [Int] where
  toList = id

data Tree a = Leaf | Node a (Tree a) (Tree a)
            deriving Show

instance Listable (Tree Int) where
  toList Leaf = []
  toList (Node x l r) = toList l ++ [x] ++ toList r

sumL x = sum $ toList x

instance (Listable a, Listable b) => Listable (a,b) where
  toList (a,b) = toList a ++ toList b
  
-- Sieve of Erathothenes

-- Sieves out primes from a increasingly sorted list of Integers
sieveOut :: [Integer] -> [Integer]
sieveOut [] = []
sieveOut (x:xs) = x : sieveOut (filter (\y -> mod y x /= 0) xs)

-- Gives primes below N using sieve of erathothenes
sievePrimes :: Integer -> [Integer]
sievePrimes n
  | n < 2 = error "No Primes below 2" 
  |otherwise = sieveOut [2..n]

-- Gives the nth prime Number
nthPrime :: Int -> Integer
nthPrime n
  | n < 1 = error "Invalid input"
  | otherwise = last . take n . sieveOut $ [2..]

-- Lazy Evaluation

f1 :: Maybe a -> [Maybe a]
f1 m = [m, m]

f2 :: Maybe a -> [a]
f2 Nothing = []
f2 (Just x) = [x]

-- Data.Map as M

phoneBook =   
  [("betty","555-2938")  
  ,("betty","342-2492")  
  ,("bonnie","452-2928")  
  ,("patsy","493-2928")  
  ,("patsy","943-2929")  
  ,("patsy","827-9162")  
  ,("lucille","205-2928")  
  ,("wendy","939-8282")  
  ,("penny","853-2492")  
  ,("penny","555-2111")  
  ]
  
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing 

phoneBookMap = M.fromListWith (\x y -> x ++ " , " ++ y) phoneBook

-- Our own implementation of Map.fromList
fromList' :: (Ord k) => [(k,v)] -> M.Map k v
fromList' = foldr (\(k,v) acc -> M.insert k v acc) M.empty

-- Data.Set as S

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"  
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"

set1 = S.fromList text1
set2 = S.fromList text2

intersect = S.intersection set1 set2

-- Types & Typeclasses

data Point = Point Float Float
           deriving (Show, Eq) 

data Line = Line Point Point
          deriving (Show, Eq)

data Circle = Circle Point Float
            deriving (Show, Eq)

--Adds two Points
addP :: Point -> Point -> Point
addP (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

-- Record Syntax

data Car = Car { company :: String,
                 model :: String,
                 year :: Integer
               } deriving Show

-- Type Synonyms

type AssocList k v = [(k,v)]

-- Student locker

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = M.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case M.lookup lockerNumber map of
    Nothing -> Left $ "Locker Number " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
                          then Right code
                          else Left $ "Locker " ++ show lockerNumber ++ " already taken!"
                                  

lockers :: LockerMap  
lockers = M.fromList   
  [(100,(Taken,"ZD39I"))  
  ,(101,(Free,"JAH3I"))  
  ,(103,(Free,"IQSA9"))  
  ,(105,(Free,"QOTSA"))  
  ,(109,(Taken,"893JJ"))  
  ,(110,(Taken,"99292"))  
  ]  

-- Typeclasses

-- Fraction Type for dealing with fractions like 1/2 & 1/3 because evaluating
-- them will give an approximation we don't need.

data Fraction = Fraction Integer Integer

instance Show Fraction where
  show (Fraction x y)
    | y == 0 = "Undefined"
    | x == 0 = show 0
    | y == 1 = show x
    | otherwise = show (div x g) ++ "/" ++ show (div y g)
    where g = gcd x y

instance Eq Fraction where
  (==) (Fraction x1 y1) (Fraction x2 y2)
    | (y1 == 0) || (y2 == 0) = False
    | (x1 == 0) && (x2 == 0) = True
    | otherwise = ((div x1 g1) == (div x2 g2)) && ((div y1 g1) == (div y2 g2))
    where g1 = gcd x1 y1
          g2 = gcd x2 y2
  
  
instance Num Fraction where
  (+) (Fraction x1 y1) (Fraction x2 y2) = Fraction (x1*y2 + x2*y1) (y1*y2)
  (*) (Fraction x1 y1) (Fraction x2 y2) = Fraction (x1*x2) (y1*y2)
  abs (Fraction x y) = Fraction (abs x) y
  signum (Fraction x y) = Fraction (signum x) 1
  fromInteger x = Fraction x 1
  negate (Fraction x y) = Fraction (negate x) y


-- Functors

instance Functor Tree where
  fmap f Leaf = Leaf
  fmap f (Node x lTree rTree) = Node (f x) (fmap f lTree) (fmap f rTree)



-- Monoids

instance Monoid Integer where
  mempty = 1
  mappend = (*)

instance Monoid Bool where
  mempty = True
  mappend = (&&)

-- Employee

data Employee = Employee Name String
              deriving Show 

type Name = String

m_name1, m_name2 :: Maybe Name
m_name1 = Nothing
m_name2 = Just "Brent"

m_phone1, m_phone2 :: Maybe String
m_phone1 = Nothing
m_phone2 = Just "555-1234"

ex01 = Employee <$> m_name1 <*> m_phone1
ex02 = Employee <$> m_name1 <*> m_phone2
ex03 = Employee <$> m_name2 <*> m_phone1
ex04 = Employee <$> m_name2 <*> m_phone2

-- Monads

check :: Int -> Maybe Int
check n | n < 10 = Just n
        | otherwise = Nothing

halve :: Int -> Maybe Int
halve n | even n = Just $ n `div` 2
        | otherwise = Nothing



         
