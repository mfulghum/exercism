module Triangle (TriangleType(..), triangleType) where

import Data.List

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType a b c =
  let tri = sort (a : b : c : [])
  in if sum tri == 0 || (sum . init) tri < last tri
        then Illegal
        else case (length . nub) tri of
               1 -> Equilateral
               2 -> Isosceles
               3 -> Scalene
               _ -> Illegal
