import Data.Char
type Bit = Int

--exercise 1
tobin :: Int -> [Bit]
tobin 0 = []
tobin n = tobin s ++ [m]
 where
   s = n `div` 2 
   m = n `mod` 2

--exercise 2
make8 :: [Bit] -> [Bit]
--make8 ns | length ns == 8 = ns
--         | otherwise = make8 (ns ++ [0]) 

--from book
make8 bits = take 8 (bits ++ repeat 0)

--exercise 3
tobin' :: Int -> [Bit]
tobin' 0 = []
tobin' n = [m] ++ tobin' s
 where
   s = n `div` 2 
   m = n `mod` 2

encode :: String -> [Bit]
encode "" = []
encode (n:ns) = make8 (tobin' (ord n)) ++ encode ns

--exercise 4
frombin :: [Bit] -> Int
frombin ns = sum [w * b | (w,b) <- zip doub ns]
  where doub = iterate (*2) 1 --[1,2,4,8,16,32,,,,]

--exercise 5**
--chop8 [1,0,1,1,1,0,1,1,1,1,1,1,1,1,1,1,0]
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

--exercise 6**
decode :: [Bit] -> String
decode = map (chr . frombin) . chop8

--exercise 7
send :: String -> String
send = decode . encode























