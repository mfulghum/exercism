module Triangle (rows) where

positions :: [[Float]]
positions = [[(1 - n)/2..(n - 1)/2] | n <- [1..]]

row :: Int -> [Integer]
row 0 = [1]
row n = [foldr ((+) . fst) 0 $ filter snd $ zip (row $ n-1) $
        map ((==0.5) . abs . flip (-) pos) $ positions !! (n-1) |
         pos <- positions !! n]

rows :: Int -> [[Integer]]
rows n
  | n <= 0 = []
  | otherwise = [row idx | idx <- [0..n-1]]
