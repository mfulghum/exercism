module DNA (toRNA) where

import Data.Maybe

toRNA :: String -> Maybe String
toRNA dna =
  let rna = mapMaybe transcribeNucleotide dna
  in if length rna == length dna
    then Just rna
    else Nothing

transcribeNucleotide :: Char -> Maybe Char
transcribeNucleotide nucleotide =
  case nucleotide of
    'C' -> Just 'G'
    'G' -> Just 'C'
    'T' -> Just 'A'
    'A' -> Just 'U'
    _  -> Nothing
