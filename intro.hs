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

factorial :: Integer -> Integer
factorial 0 =  1
factorial n = n * (factorial (n-1))

sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

-- Look Up for Collatz Conjecture
hailstone :: Integer -> Integer
hailstone n
  | mod n 2 == 0 = div n 2
  | otherwise = 3*n + 1

hailstoneSeq :: Integer -> [Integer]
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

-- Exercise Problem Solutions of cis194 first chapter

-- Credit Card Validator

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (quot n 10) ++ [(mod n 10)]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)
    
lastSecond :: [a] -> a
lastSecond ls = head (drop ((length ls)-2) ls)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther zs = (doubleEveryOther (take ((length zs)-2) zs)) ++ [2*(lastSecond zs), (last zs)]

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum (toDigits x)) + (sumDigits xs)

validate :: Integer -> Bool
validate n = (mod (sumDigits (doubleEveryOther (toDigits n))) 10) == 0

-- Towers of Hanoi Problem

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 first last temp = [(first,last)]
hanoi n first last temp = (hanoi (n-1) first temp last) ++ (hanoi 1 first last temp) ++ (hanoi (n-1) temp last first)
 
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
