import Data.Char
type Bit = Int

tobin :: Int -> [Bit]
tobin 0 = []
tobin n = tobin s ++ [m]
 where
   s = n `div` 2 
   m = n `mod` 2

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

tobin' :: Int -> [Bit]
tobin' 0 = []
tobin' n = [m] ++ tobin' s
 where
   s = n `div` 2 
   m = n `mod` 2

encode :: String -> [Bit]
encode "" = []
encode (n:ns) = make8 (tobin' (ord n)) ++ encode ns

frombin :: [Bit] -> Int
frombin ns = sum [w * b | (w,b) <- zip doub ns]
  where doub = iterate (*2) 1 --[1,2,4,8,16,32,,,,]

--chop8 [1,0,1,1,1,0,1,1,1,1,1,1,1,1,1,1,0]
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . frombin) . chop8

send :: String -> String
send = decode . encode























