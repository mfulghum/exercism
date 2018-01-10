module House (rhyme) where

import Data.List (intercalate)

subject = ["horse and the hound and the horn",
           "farmer sowing his corn",
           "rooster that crowed in the morn",
           "priest all shaven and shorn",
           "man all tattered and torn",
           "maiden all forlorn",
           "cow with the crumpled horn",
           "dog",
           "cat",
           "rat",
           "malt",
           "house that Jack built."]

action = ["belonged to",
          "kept",
          "woke",
          "married",
          "kissed",
          "milked",
          "tossed",
          "worried",
          "killed",
          "ate",
          "lay in"]

subjects :: Int -> [String]
subjects n = drop (length subject - n - 1) subject

actions :: Int -> [String]
actions n = "is" : drop (length action - n) action

verse :: Int -> String
verse n = concat [(if v == "is" then "This " else "that ") ++
                  v ++ " the " ++ s ++ "\n" |
                  (v,s) <- zip (actions n) (subjects n)]

rhyme :: String
rhyme = intercalate "\n" $ map verse [0..length subject - 1]
