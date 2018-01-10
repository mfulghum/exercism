module Anagram (anagramsFor) where

import Data.Char
import Data.List

frequencyAccum [] _ _ = []
frequencyAccum [x] resultList acc = resultList ++ [(x, acc)]
frequencyAccum (x:xs) resultList acc =
  if x == head xs
     then frequencyAccum xs resultList (acc + 1)
     else frequencyAccum xs (resultList ++ [(x, acc)]) 1

frequency input = frequencyAccum ((sort . map toLower) input) [] 1

anagramsFor :: String -> [String] -> [String]
anagramsFor input options =
  let inputLower = map toLower input
      inputFreq = frequency input
  in [anagram | anagram <- options, frequency anagram == inputFreq, inputLower /= (map toLower anagram)]
