doubleUs x y = doubleMe x + doubleMe y
doubleMe x = x + x
doubleSmallNumber x = if x > 100 then x else doubleMe x
doubleSmallNumber' x = (if x > 100 then x else doubleMe x) + 1

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <-st, elem c ['A'..'Z']]

sayMe :: (Integral a) => a -> String 
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a 
factorial 0 = 1 
factorial n = n * factorial (n - 1)

sayMe' :: (Integral a) => a -> String 
sayMe' x
  | x == 1 = "One!"
  | x == 2 = "Two!"
  | x == 3 = "Three!"
  | x == 4 = "Four!"
  | x == 5 = "Five!"
  | otherwise = "Not between 1 and 5"

calcBmis :: (RealFloat a) => [(a, a)] -> [a] 
calcBmis xs = [bmi w h | (w, h) <- xs] 
  where bmi weight height = weight / height ^ 2

toLb :: (Fractional a) => a -> a
toLb kg = kg * 2.2046

toKg :: (Fractional a) => a -> a
toKg lb = lb / 2.2046

toMeter :: (Fractional a) => a -> a
toMeter ft = ft / 3.2808

toFeet :: (Fractional a) => a -> a
toFeet m = m * 3.2808


bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You are underweight, you emi, you !"
  | bmi <= normal = "You're supposedly normal.  Pffft, I bet you're ugly!"
  | bmi <= fat    = "You're fat! Lose some weight, fatty!"
  | otherwise     = "You're a whale, congratulations!!!"
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        fat = 30.0

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea


maximum' :: (Ord a) => [a] -> a 
maximum' [] = error "maximum of empty list" 
maximum' [x] = x 
maximum' (x:xs) = max x (maximum' xs)


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
