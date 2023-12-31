-- charDigitToInt :: Char -> Maybe Integer
-- charDigitToInt c
--    | c == '0' = 0
--    | c == '1' = 1
--    | c == '2' = 2
--    | c == '3' = 3
--    | c == '4' = 4
--    | c == '5' = 5
--    | c == '6' = 6
--    | c == '7' = 7
--    | c == '8' = 8
--    | c == '9' = 9
--    | otherwise = Nothing

-- toDigits :: Integer -> [Integer]
-- toDigits n = map charDigitToInt $ show n
--
-- Above shows how Haskell's typing system steers you in the right direction by forcing me to face
-- non-digit characters and the consequences that would have for the code.

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' [h] = [h]
doubleEveryOther' (h:h2:xs) = h:2*h2:doubleEveryOther' xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOther' . reverse

sumDigit :: Integer -> Integer
sumDigit n
    | n <= 0 = 0
    | otherwise = sumDigit (n `div` 10) + (n `mod` 10)

sumDigits :: [Integer] -> Integer
sumDigits l = sum $ map sumDigit l

validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg ->  [Move]
hanoi 0 a b c = []
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)

main :: IO ()
main = do
    print $ toDigits 1234
    print $ toDigitsRev 1234
    print $ toDigits 0
    print $ toDigits (-17)
    print $ doubleEveryOther [8, 7, 6, 5]
    print $ doubleEveryOther [1, 2, 3]
    print $ sumDigits [16, 7, 12, 5]
    print $ validate 4012888888881881
    print $ validate 4012888888881882
    print $ hanoi 3 "a" "b" "c"
