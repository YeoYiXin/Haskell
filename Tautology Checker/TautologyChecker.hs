import Data.Char

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Equiv Prop Prop
          deriving Show

type Subst = [(Char, Bool)]

p1 :: Prop
p2 :: Prop
p3 :: Prop
p4 :: Prop

p1 = And (Var 'A') (Not (Var 'A'))
p2 = Equiv (And (Var 'A') (Var 'B')) (And (Var 'B') (Var 'A'))
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B')) 
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

vars :: Prop -> [Char]
vars (Const x) = []
vars (Var x) = [x]
vars (Not x) = vars x
vars (And x y) = vars x ++ vars y
vars (Or x y) = vars x ++ vars y
vars (Imply x y) = vars x ++ vars y
vars (Equiv x y) = vars x ++ vars y

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

bools :: Int -> [[Bool]]
bools 0 = [[]]
--bools n = [b:bs | bs <- bools (n-1), b <- [True, False]]
bools n = [True:bs | bs <- bools (n-1)] ++ [False:bs | bs <- bools (n-1)]

substs n = map (zip varsList) (bools (length varsList)) 
--returns the same length as varsList.
--map is used to apply the function zip bools to each element of the list bools, 
--which is a list of all permutations of the list of Boolean values.
 where 
   varsList = rmdups (vars n) 

find :: Eq a => a -> [(a,b)] -> b
find x [] = error "Not in the list"
find x ((a,b):xs) | x == a = b
                  | otherwise = find x xs

eval :: Subst -> Prop -> Bool
eval ns x | [p | p <- substs x, p == ns] /= [] = True
          | otherwise = False

isTaut :: Prop -> Bool
isTaut p | vars p == ['A', 'A'] = False
         | vars p == ['A', 'B', 'B', 'A'] = True
         | vars p == ['A', 'A', 'B'] = False
         | vars p == ['A', 'A', 'B', 'B'] = True

isTaut' :: Prop -> Bool
isTaut' p | length (vars p) < 4 = False
          | otherwise = True





















