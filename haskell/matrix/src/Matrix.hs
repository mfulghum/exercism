module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V (fromList, toList)

type Matrix a = [Vector a]

flatten :: Matrix a -> Vector a
flatten = V.fromList . concat . map V.toList

fromList :: [[a]] -> Matrix a
fromList inputList =
  [V.fromList listRow | listRow <- inputList]

parseLine :: Read a => String -> [a]
parseLine line =
  case reads line of
    [] -> []
    (x,xs): _ -> x : parseLine xs

fromString :: Read a => String -> Matrix a
fromString matrixString =
  [V.fromList $ parseLine rowLine |
   rowLine <- lines matrixString]

row :: Int -> Matrix a -> Vector a
row n = flip (!!) n

rows :: Matrix a -> Int
rows = length

cols :: Matrix a -> Int
cols matrix =
  if rows matrix /= 0
     then length $ matrix !! 0
     else 0

shape :: Matrix a -> (Int, Int)
shape matrix = (rows matrix, cols matrix)

transpose :: Matrix a -> Matrix a
transpose matrix =
  let flatMatrix = concat $ map V.toList matrix
  in [V.fromList [flatMatrix !! n |
      n <- [m,m+cols matrix..length flatMatrix-1]] |
      m <- [0..cols matrix-1]]

column :: Int -> Matrix a -> Vector a
column n = flip (!!) n . transpose

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (m,n) matrix =
  let flatMatrix = concat $ map V.toList matrix
  in [(V.fromList . take n . drop (n * j)) flatMatrix | j <- [0..m-1]]
