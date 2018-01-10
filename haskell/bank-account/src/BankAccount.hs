module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Data.Maybe
import Data.IORef

type BankAccount = IORef (Maybe Integer)

closeAccount :: BankAccount -> IO ()
closeAccount account = writeIORef account Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance account = do
  currentBalance <- readIORef account
  return currentBalance

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance account amount =
  let updateBalance Nothing = (Nothing, Nothing)
      updateBalance (Just balance) = (Just $ balance + amount,
                                      Just $ balance + amount)
  in atomicModifyIORef' account updateBalance
  
openAccount :: IO BankAccount
openAccount = newIORef $ Just 0
