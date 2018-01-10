module Bowling (score, BowlingError(..)) where

import Data.List
import Data.Maybe

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

data Frame = Open Int | Spare Int | Strike Int | Bonus Int | Invalid Int Int
  deriving (Eq, Show)

frameRolls [] roll output = output
frameRolls (x:[]) roll output
  | x < 0 || x > 10 = output ++ [Invalid roll x]
  | x == 10 = output ++ [Strike roll]
  | length output >= 10 = output ++ [Bonus roll]
  | otherwise = output ++ [Invalid roll x]
frameRolls (x:y:xs) roll output
  | x < 0 || x > 10 = output ++ [Invalid roll x]
--  | length output >= 10 = frameRolls (y:xs) (roll + 1) $ output ++ [Bonus roll]
  | x == 10 = frameRolls (y:xs) (roll + 1) $ output ++ [Strike roll]
  | y < 0 || x + y > 10 = output ++ [Invalid (roll + 1) y]
  | x + y == 10 = frameRolls xs (roll + 2) $ output ++ [Spare roll]
  | x + y < 10 = frameRolls xs (roll + 2) $ output ++ [Open roll]

isInvalid :: Frame -> Bool
isInvalid (Invalid _ _) = True
isInvalid _ = False

isStrike :: Frame -> Bool
isStrike (Strike _) = True
isStrike _ = False

isSpare :: Frame -> Bool
isSpare (Spare _) = True
isSpare _ = False

isOpen :: Frame -> Bool
isOpen (Open _) = True
isOpen _ = False

invalidToBowlingError :: Frame -> BowlingError
invalidToBowlingError (Invalid index value) = InvalidRoll index value
invalidToBowlingError _ = error "This frame is not a bowling error"

scoreFrame :: [Int] -> Frame -> Int
scoreFrame rolls frame =
  case frame of
    Invalid _ _ -> 0
    Strike n -> sum . take 3 . drop n $ rolls
    Spare n -> sum . take 3 . drop n $ rolls
    Open n -> sum . take 2 . drop n $ rolls
    Bonus _ -> 0

score :: [Int] -> Either BowlingError Int
score rolls
  | length frames == 0 = Left $ IncompleteGame
  | isInvalid $ last frames = Left $ invalidToBowlingError (last frames)
  | length frames < 10 = Left $ IncompleteGame
  | isStrike (frames !! 9) && length frames < 11 = Left $ IncompleteGame
  | isStrike (frames !! 9) && isStrike (frames !! 10) && length frames /= 12 = Left $ IncompleteGame
  | isSpare (frames !! 9) && length frames /= 11 = Left $ IncompleteGame
  | isOpen (frames !! 9) && length frames > 10 = Left $ InvalidRoll 20 0
  | otherwise = Right (sum $ map (scoreFrame rolls) (take 10 frames))
  where frames = frameRolls rolls 0 []
        

