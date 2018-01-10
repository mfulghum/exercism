module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import Data.Map (Map, fromList, lookup)
import Data.Maybe
import Data.List

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

plant :: Char -> Plant
plant plantChar =
  case plantChar of
   'C' -> Clover
   'G' -> Grass
   'R' -> Radishes
   'V' -> Violets
   _ -> error "Invalid plant character in string!"

defaultStudents = ["Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"]

studentPlants :: Int -> Int -> [Int]
studentPlants numStudents student =
  student * 2 :
  student * 2 + 1 :
  (numStudents + student) * 2 :
  (numStudents + student) * 2 + 1 :
  []

defaultGarden :: String -> Map String [Plant]
defaultGarden = garden defaultStudents

garden :: [String] -> String -> Map String [Plant]
garden students inputString =
  let plants = map plant $ (concat . lines) inputString
      numStudents = div (length plants) 4
      sortedStudents = sort students
  in fromList [(sortedStudents !! student, [plants !! num | num <- studentPlants numStudents student]) | student <- [0..numStudents-1]]

lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants student gardenData = fromJust $ Data.Map.lookup student gardenData
