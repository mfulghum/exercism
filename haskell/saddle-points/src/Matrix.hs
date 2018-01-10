module Matrix (saddlePoints) where

import Data.List (maximumBy, minimumBy)
import Data.Function (on)
import Data.Array (Array, bounds, assocs)

row m = filter ((== m) . fst . fst) . assocs
col n = filter ((== n) . snd . fst) . assocs

arrayMin :: (Foldable t, Ord c) => t (a, c) -> c
arrayMin = snd . minimumBy (compare `on` snd)

arrayMax :: (Foldable t, Ord c) => t (a, c) -> c
arrayMax = snd . maximumBy (compare `on` snd)

rowMax n matrix =
  let matrixRow = row n matrix
  in [fst entry | entry <- matrixRow, snd entry == arrayMax matrixRow]

colMin n matrix =
  let matrixCol = col n matrix
  in [fst entry | entry <- matrixCol, snd entry == arrayMin matrixCol]

saddlePoints matrix =
  let (m, n) = snd $ bounds matrix
      maxRows = concat [rowMax i matrix | i <- [0..m]]
      minCols = concat [colMin j matrix | j <- [0..n]]
  in [a | a <- maxRows, b <- minCols, a == b]

