
doubleMe x = x + x

doubleUs x y = (doubleMe x) + (doubleMe y)

doubleSmallNumber x = if x > 100
                         then x
                         else x*2
                              
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

boomBangs ls = [ if x > 10 then "BANG!" else "BOOM!" | x <- ls, odd x]

length' ls = sum [1 | _ <- ls]

square x = x*x

rightTriangles = [(z,y,x) | x <- [1..10], y <- [1..x],z <- [1..y], y^2 + z^2 == x^2, x + y + z == 24]
