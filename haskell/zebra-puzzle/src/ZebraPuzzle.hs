module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Control.Monad
import Data.List (permutations, zip5)
import Data.Maybe

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show, Enum)

data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Eq, Show, Enum)
data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Show, Enum)
data Drink = Coffee | Tea | Milk | OrangeJuice | Water
  deriving (Eq, Show, Enum)
data Smoke = Kools | Chesterfields | LuckyStrike | Parliaments | OldGold
  deriving (Eq, Show, Enum)

data House = House Resident Color Pet Drink Smoke
  deriving (Show, Eq)

defHouse :: (Resident, Color, Pet, Drink, Smoke) -> House
defHouse (resident, color, pet, drink, smoke) =
  House resident color pet drink smoke

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

possibleCombinations :: (Enum a) => [[a]]
possibleCombinations = permutations . enumFrom $ toEnum 0

isConnected left leftValue right rightValue =
  guard $ elem (leftValue, rightValue) (zip left right)
leftOf left leftValue right rightValue =
  guard $ elem (leftValue, rightValue) (zip left (tail right))
rightOf left leftValue right rightValue =
  guard $ elem (leftValue, rightValue) (zip (tail left) right)
nextTo left leftValue right rightValue =
  mplus (leftOf left leftValue right rightValue)
        (rightOf left leftValue right rightValue)
inPosition left leftValue pos =
  guard $ left !! pos == leftValue

results = do
-- Handle cases where only color is involved first
  color <- (possibleCombinations :: [[Color]])
  -- 6: The green house is immediately to the right of the ivory house
  rightOf color Green color Ivory

-- Next handle cases where only residents and color are involved
  resident <- (possibleCombinations :: [[Resident]])
  -- 2: The Englishman lives in the red house
  isConnected resident Englishman color Red
  -- 10: The Norwegian lives in the first house
  inPosition resident Norwegian 0
  -- 15: The Norwegian lives next to the blue house
  nextTo resident Norwegian color Blue

-- Add drinks to the mix
  drink <- (possibleCombinations :: [[Drink]])
  -- 4: Coffee is drunk in the green house
  isConnected drink Coffee color Green
  -- 5: The Ukrainian drinks tea
  isConnected resident Ukrainian drink Tea
  -- 9: Milk is drunk in the middle house
  inPosition drink Milk 2

-- Next handle pets
  pet <- (possibleCombinations :: [[Pet]])
  -- 3: The Spaniard owns the dog
  isConnected resident Spaniard pet Dog

-- Finally handle cigarette brands
  smoke <- (possibleCombinations :: [[Smoke]])
  -- 7: The Old Gold smoker owns snails
  isConnected smoke OldGold pet Snails
  -- 8: Kools are smoked in the yellow house
  isConnected smoke Kools color Yellow
  -- 11: The man who smokes Chesterfields lives in the house next to the man with the fox
  nextTo smoke Chesterfields pet Fox
  -- 12: Kools are smoked in the house next to the house where the horse is kept
  nextTo smoke Kools pet Horse
  -- 13: The Lucky Strike smoker drinks orange juice
  isConnected smoke LuckyStrike drink OrangeJuice
  -- 14: The Japanese smokes Parliaments
  isConnected resident Japanese smoke Parliaments

  return $ map defHouse $ zip5 resident color pet drink smoke

solveWaterDrinker :: [House] -> Resident
solveWaterDrinker (x@(House resident _ _ drink _):xs)
  | drink == Water = resident
  | otherwise = solveWaterDrinker xs

solveZebraOwner :: [House] -> Resident
solveZebraOwner (x@(House resident _ pet _ _):xs)
  | pet == Zebra = resident
  | otherwise = solveZebraOwner xs

solve :: Solution
solve =
  let simulation = head results
  in Solution (solveWaterDrinker simulation)
              (solveZebraOwner simulation)

