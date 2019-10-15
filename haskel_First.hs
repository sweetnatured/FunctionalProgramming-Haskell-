toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]


toDigits 0 = []
toDigits x 
    
  | x <=0 = []
  | otherwise = toDigits ( x `div` 10) ++ [x `mod` 10]

toDigitsRev 0 = []
toDigitsRev x 

  | x <=0 = []
  | otherwise = x `mod` 10 : toDigitsRev(x `div` 10)

doubleEveryOther :: [Integer] -> [Integer]  
doubleEveryOther [] = []  
doubleEveryOther (z:y:zs) = z : 2 * y : doubleEveryOther(zs)
  
cntlist [] = []
cntlist(t:rs) = toDigits(t) ++ cntlist(rs)


sumlist [] = 0
sumlist(t:rs) = t + sumlist(rs)

result r = sumlist(cntlist(doubleEveryOther(toDigitsRev(r))))

validate f
  | result (f) `mod` 10 == 0 = True
  | otherwise = False

