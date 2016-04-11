module Geometry.Cuboid
       (volume
       , area 
       ) where

volume :: Float -> Float -> Float -> Float
volume l b h = l * b * h

area :: Float -> Float -> Float -> Float
area l b h = 2 * (rectangleArea l b + rectangleArea b h + rectangleArea l h)

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b
