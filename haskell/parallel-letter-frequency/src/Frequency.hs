module Frequency (frequency) where

import Control.Parallel.Strategies
import Data.Map  (Map, insertWith, empty, unionsWith)
import Data.Text (Text)
import Data.Char (isLetter)
import qualified Data.Text as T

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts =
  let strategy = parBuffer nWorkers rpar
      parCount = withStrategy strategy . map countLetters
  in unionsWith (+) $ parCount texts

normalizeText :: Text -> Text
normalizeText = T.toLower . T.filter isLetter

tallyCharacter :: Map Char Int -> Char -> Map Char Int
tallyCharacter charMap char = insertWith (+) char 1 charMap

countLetters :: Text -> Map Char Int
countLetters input = T.foldl tallyCharacter empty $ normalizeText input
