module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits

data Allergen = Eggs | Peanuts | Shellfish | Strawberries | Tomatoes | Chocolate | Pollen | Cats
  deriving (Show, Enum, Eq)

allergies :: Int -> [Allergen]
allergies score = filter (flip isAllergicTo score) $ enumFrom Eggs

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = testBit score (fromEnum allergen)
