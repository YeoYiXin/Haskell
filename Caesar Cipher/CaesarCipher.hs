import Data.Char
import Data.List

table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

--exercise 1
let2nat :: Char -> Int
let2nat x = ord x - ord 'a'

--exercise 2
nat2let :: Int -> Char
nat2let x = chr (x + ord 'a')

--exercise 3
shift :: Int -> Char -> Char
shift x c | let2nat c >= 25 = nat2let ((let2nat 'a') - 1 + x)
          | let2nat c < 0 = c 
          | otherwise = nat2let ((let2nat c) + x) 

--exercise 4*
encode :: Int -> String -> String
encode x ns = [shift x a | a <- ns]

--exercise 5**
unshift :: Int -> Char -> Char
unshift x c = if isAlpha c then 
                 if (let2nat c) - x < 0 then nat2let ((let2nat c) - x + 26) else shift (-x) c
              else c
decode :: Int -> String -> String
decode x ns = [unshift x a | a <- ns]

--exercise 6
lowers :: String -> Int
lowers ns = length [x | x <- ns, x >= 'a' && x <= 'z']

--exercise 7
count :: Char -> String -> Int
count x ns = length [a | a <- ns, a == x] 

--exercise 8
percent :: Int -> Int -> Float
percent x y = (fromIntegral x / fromIntegral y) * 100

--exercise 9
freqs :: String -> [Float]
freqs ns = [percent (count a ns) n | a <- ['a'..'z']]
 where n = lowers ns

--exercise 10
rotate :: Int -> [a] -> [a]
rotate n ns= drop n ns ++ take n ns

--exercise 11**
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

--exercise 12**
--position :: Eq a => a -> [a] -> Int
--position x xs = go 0 xs
-- where
-- go n (y:ys) | x == y = n
--             | otherwise = go (n+1) ys
position :: Eq a => a -> [a] -> Int
position x xs = head [i | (x',i) <- zip xs [0..], x == x']

--exercise 13****
crack :: String -> String
crack xs = decode factor xs
  where
    factor = head $ sortOn chiSquared [0..25]   -- Find the rotation factor with minimum chi-squared
    chiSquared n = chisqr (rotate n freqsXs) table   -- Calculate the chi-squared for a given rotation factor
    freqsXs = freqs xs    -- Calculate letter frequencies in the input string













 