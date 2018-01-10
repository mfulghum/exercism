module Robot (Robot, mkRobot, resetName, robotName) where

import Data.IORef
import Control.Monad (liftM, liftM2)
import System.Random (randomRs, newStdGen)

type Robot = IORef String

generateName :: IO String
generateName = do
  chars <- liftM (take 2 . randomRs('A','Z')) newStdGen
  numbers <- liftM (take 3 . randomRs('0','9')) newStdGen
  let name = chars ++ numbers
  return name

mkRobot :: IO Robot
mkRobot = generateName >>= newIORef

resetName :: Robot -> IO ()
resetName robot = do
  name <- generateName
  writeIORef robot name

robotName :: Robot -> IO String
robotName robot = readIORef robot
