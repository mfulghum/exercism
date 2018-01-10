module Base (rebase) where

fromBase :: Integral a => a -> [a] -> a
fromBase inputBase inputDigits =
  let baseValues = reverse [inputBase^n | n <- [0..length inputDigits - 1]]
  in sum $ zipWith (*) inputDigits baseValues

toBase :: Integral a => a -> a -> [a]
toBase outputBase inputValue =
  let baseValue = reverse $ takeWhile (<= inputValue) [outputBase^n | n <- [0..]]
  in chewOutput inputValue baseValue []
     where chewOutput val [] outputDigits = outputDigits
           chewOutput val (x:xs) outputDigits =
             chewOutput (rem val x) xs (outputDigits ++ [div val x])

rebase :: Integral a => a -> a -> [a] -> Maybe [a]
rebase inputBase outputBase inputDigits =
  if inputBase <= 1 ||
     outputBase <= 1 ||
     any (< 0) inputDigits ||
     any (>= inputBase) inputDigits
     then Nothing
     else Just (toBase outputBase $ fromBase inputBase inputDigits)
