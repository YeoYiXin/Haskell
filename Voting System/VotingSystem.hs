import Data.List (sort, transpose)
type Party = String
type Ballot = [Party]

--question 1
count :: Eq a => a -> [a] -> Int
count n xs = length [a | a <- xs, n == a]

--question 2
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

--question 3
frequency :: Eq a => [a] -> [(Int, a)]
frequency xs = [(count b xs, b) | b <- rmdups xs] 

--question 4
results :: [Party] ->[(Int, Party)]

votes :: [Party]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

results = sort . frequency

--question 5
winner :: [Party] -> Party
winner = snd . last . results


----------------------------------------------------------------------
--Alternative votes*
--question 6
rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= []) 

--question 7*
remove :: Eq a => a -> [[a]] -> [[a]]
remove n = map (filter (/= n))

--question 8***
rank :: [Ballot] -> [Party]

ballots :: [Ballot]
ballots = [b1,b2,b3,b4,b5,b6]
b1 = ["Blue", "Green"]
b2 = ["Green", "Blue", "Red"]
b3 = ["Blue"]
b4 = ["Red", "Green"]
b5 = ["Blue", "Red", "Green"]
b6 = ["Green", "Red"]

--make the b1 to b6 into an array first, then count no of votes for each colour, then take only the name
rank = map snd . results . map head

--question 9***
election :: [Ballot] -> Party
election bs = case rank (rmempty bs) of
                [c] -> c
                (c:cs) -> election (remove c bs)













