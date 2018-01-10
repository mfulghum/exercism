module Clock (clockHour, clockMin, fromHourMin, toString) where

import Text.Printf

data Clock = Clock Integer Integer
  deriving (Show, Eq)

instance Num Clock where
  fromInteger min =
    fromHourMin (div wrappedMin 60) (mod wrappedMin 60)
    where wrappedMin = mod min 1440
  negate (Clock hr min) =
    fromInteger $ 1440 - (hr * 60 + min)
  (+) (Clock hr1 min1) (Clock hr2 min2) =
    fromInteger $ (hr1 + hr2) * 60 + min1 + min2
  (*) = undefined
  abs = undefined
  signum = undefined
  

clockHour :: Clock -> Integer
clockHour (Clock hr _) = hr 

clockMin :: Clock -> Integer
clockMin (Clock _ min) = min

fromHourMin :: Integer -> Integer -> Clock
fromHourMin hr min = Clock (flip mod 24 $ hr + div min 60) (mod min 60)

toString :: Clock -> String
toString (Clock hr min) = printf "%02d:%02d" hr min
