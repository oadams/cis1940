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

main :: IO ()
main = do
    print $ toDigits 1234
    print $ toDigitsRev 1234
    print $ toDigits 0
    print $ toDigits (-17)
