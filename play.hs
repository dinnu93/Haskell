data List t = Empty
            | Cons t (List t)
            deriving Show
            
mapL :: (a -> b) -> (List a) -> (List b) 
mapL _ Empty = Empty
mapL f (Cons x xs) = Cons (f x) $ mapL f xs

filterL :: (a -> Bool) -> (List a) -> (List a) 
filterL _ Empty = Empty
filterL p (Cons x xs) 
    | p x = Cons x $ filterL p xs
    | otherwise = filterL p xs
