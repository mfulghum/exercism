module FoodChain (song) where

import Data.Maybe
import Data.List (intercalate)

actions = ["It wriggled and jiggled and tickled inside her."
          ,"How absurd to swallow a bird!"
          ,"Imagine that, to swallow a cat!"
          ,"What a hog, to swallow a dog!"
          ,"Just opened her throat and swallowed a goat!"
          ,"I don't know how she swallowed a cow!"]

things = ["spider"
         ,"bird"
         ,"cat"
         ,"dog"
         ,"goat"
         ,"cow"]

verseSwallow :: Int -> Maybe String
verseSwallow n
  | n < 0 || n >= 6 = Nothing
  | n == 0 = Just $
    "She swallowed the " ++ (things !! n) ++
    " to catch the fly."
  | n == 1 = Just $
    "She swallowed the " ++ (things !! n) ++
    " to catch the " ++ (things !! (n-1)) ++
    " that " ++ (drop 3 $ actions !! 0)
  | otherwise = Just $
    "She swallowed the " ++ (things !! n) ++
    " to catch the " ++ (things !! (n-1)) ++ "."

verseStart :: Int -> Maybe String
verseStart n
  | n < 0 = Just $
    "I know an old lady who swallowed a fly."
  | n == 6 = Just $
    "I know an old lady who swallowed a horse."
  | n >= 0 = Just $
    "I know an old lady who swallowed a " ++
    (things !! n) ++ ".\n" ++ (actions !! n)

verseMiddle :: Int -> Maybe String
verseMiddle n
  | n < 0 || n >= 6 = Nothing
  | otherwise = Just $
    intercalate "\n" . reverse . catMaybes $
    [verseSwallow idx | idx <- [0..n]]

verseEnd :: Int -> Maybe String
verseEnd n
  | n < 6 = Just $
    "I don't know why she swallowed the fly. Perhaps she'll die.\n"
  | otherwise = Just $
    "She's dead, of course!\n"

verse :: Int -> String
verse n =
  intercalate "\n" $
  maybeToList (verseStart n) ++
  maybeToList (verseMiddle n) ++
  maybeToList (verseEnd n)

song :: String
song = intercalate "\n" [verse n | n <- [(-1)..6]]
