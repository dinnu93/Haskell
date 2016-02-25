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

length' :: Num a => [t] -> a
length' ls = sum [1 | _ <- ls]

square :: Int -> Int
square x = x*x

rightTriangles :: [(Int, Int, Int)]
rightTriangles = [(z,y,x) | x <- [1..10], y <- [1..x],z <- [1..y], y^2 + z^2 == x^2, x + y + z == 24]

factorial :: Integer -> Integer
factorial n = if n == 0
                 then 1
                 else n * (factorial (n-1))
