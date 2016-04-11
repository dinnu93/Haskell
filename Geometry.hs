module Geometry
       (
         sphereVolume
       , sphereArea
       , cubeVolume
       , cubeArea
       , cuboidArea
       , cuboidVolume
       ) where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0/3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2)

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea l b h = 2 * (l*b + b*h + l*h)

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume l b h = l * b * h

