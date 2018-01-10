module School (School, add, empty, grade, sorted) where

import Data.List

type School = [(Integer, String)]

add :: Integer -> String -> School -> School
add studentGrade studentName school =
  school ++ [(studentGrade, studentName)]

empty :: School
empty = []

grade :: Integer -> School -> [String]
grade gradeLevel school =
  sort [studentName | (studentGrade, studentName) <- school, studentGrade == gradeLevel] 

sorted :: School -> [(Integer, [String])]
sorted school =
  let gradeLevels = sort $ nub (map fst school)
  in [(gradeLevel, grade gradeLevel school) | gradeLevel <- gradeLevels]
