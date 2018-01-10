module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show, Enum)

data Robot = Robot { bearing :: Bearing
                   , coordinates :: (Integer, Integer)
                   } deriving (Show)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coords = Robot direction coords

step :: Robot -> Char -> Robot
step robot command =
  let direction = bearing robot
  in case command of
       'L' -> mkRobot (turnLeft direction) (coordinates robot)
       'R' -> mkRobot (turnRight direction) (coordinates robot)
       'A' -> advance robot

simulate :: Robot -> String -> Robot
simulate robot [] = robot
simulate robot (x:xs) = simulate (step robot x) xs

turnLeft :: Bearing -> Bearing
turnLeft initial =
  toEnum (mod (fromEnum initial - 1) 4)::Bearing

turnRight :: Bearing -> Bearing
turnRight initial =
  toEnum (mod (fromEnum initial + 1) 4)::Bearing

advance :: Robot -> Robot
advance robot =
  let direction = bearing robot
      x = (fst . coordinates) robot
      y = (snd . coordinates) robot
  in case direction of
       North -> mkRobot direction (x, y + 1)
       East -> mkRobot direction (x + 1, y)
       South -> mkRobot direction (x, y - 1)
       West -> mkRobot direction (x - 1, y)
