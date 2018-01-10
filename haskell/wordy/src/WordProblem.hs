module WordProblem (answer) where

import Data.List (isInfixOf)
import Data.Maybe

data Operator = Add | Subtract | Multiply | Divide deriving (Show, Eq, Enum)

readIntMaybe :: String -> Maybe Integer
readIntMaybe str =
  case reads str :: [(Integer, String)] of
    [(x, _)] -> Just x
    _ -> Nothing

readOpMaybe :: String -> Maybe Operator
readOpMaybe str
  | isInfixOf "plus" str = Just Add
  | isInfixOf "minus" str = Just Subtract
  | isInfixOf "mult" str = Just Multiply
  | isInfixOf "div" str = Just Divide
  | otherwise = Nothing

parseWords [] number _ = number
parseWords (x:xs) number op
  | isJust newOp = parseWords xs number newOp
  | isJust newNumber =
      let newNum = fromJust newNumber
          oldNum = fromJust number
          output = case op of
            Just Add -> Just $ oldNum + newNum
            Just Subtract -> Just $ oldNum - newNum
            Just Multiply -> Just $ oldNum * newNum
            Just Divide -> Just $ div oldNum newNum
            Nothing -> Just newNum
      in parseWords xs output Nothing
  | otherwise = parseWords xs number op
  where newOp = readOpMaybe x
        newNumber = readIntMaybe x

answer str =
  let strWords = words str
  in if null $ mapMaybe readOpMaybe strWords
     then Nothing
     else parseWords strWords Nothing Nothing
