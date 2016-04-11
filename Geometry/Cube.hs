module Geometry.Cube
       (volume
       , area 
       ) where

import qualified Geometry.Cuboid as Cuboid
import qualified Geometry as Geometry

volume :: Float -> Float
volume s = Cuboid.volume s s s

area :: Float -> Float
area s = Cuboid.area s s s


