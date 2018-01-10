module ETL (transform) where

import Data.Map (Map, assocs, fromList)
import Data.Char

replicatedScores score letters =
  replicate (length letters) score

transform :: Map a String -> Map Char a
transform scoreLetters =
  (fromList . concat) [zip (map toLower letters) (replicatedScores score letters) | (score, letters) <- assocs scoreLetters]

