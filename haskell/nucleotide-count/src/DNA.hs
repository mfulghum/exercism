module DNA (count, nucleotideCounts) where

import Data.Map (Map, fromList)
import Data.Either

validateNucleotide :: Char -> Bool
validateNucleotide nucleotide =
  elem nucleotide "ACGT"

validateStrand :: String -> Bool
validateStrand strand =
  and $ map validateNucleotide strand
  
count :: Char -> String -> Either String Int
count nucleotide strand =
  if validateNucleotide nucleotide && validateStrand strand
     then Right (sum [1 | x <- strand, x == nucleotide])
     else Left "Invalid input"

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts strand =
  if validateStrand strand
     then Right (fromList [(nucleotide, head (rights [count nucleotide strand])) | nucleotide <- "ACGT"])
     else Left "Invalid input"
