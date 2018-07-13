module Chapter8 where
    import GHC.Enum
    
    type String' = [Char]

    type Pos = (Int,Int)

    type Trans = Pos -> Pos

    --type aliasing is not allowed when function type is recursive
    --type Tree' = (Int, [Tree'])

    type Pair a = (a,a)

    type Assoc k v = [(k,v)]

    find :: Eq k => k -> Assoc k v -> v
    find k t = head [v | (k',v) <-t, k == k']

    data Bool' = False' | True' 

    data Move = North | South | East | West

    move :: Move -> Pos -> Pos
    move North (x,y) = (x, y+1)
    move South (x,y) = (x, y-1)
    move East (x,y) = (x+1, y)
    move West (x,y) = (x-1, y)

    moves :: [Move] -> Pos -> Pos
    moves [] p     = p
    moves (m:ms) p = moves ms (move m p)

    rev :: Move -> Move
    rev North = South
    rev South = North
    rev East = West
    rev West = East

    data Shape = Circle Float | Rect Float Float

    square :: Float -> Shape
    square n = Rect n n

    area :: Shape -> Float
    area (Circle r) = pi * r^2
    area (Rect x y) = x * y

    data Maybe' a = Nothing' | Just' a


    safeDiv :: Int -> Int -> Maybe' Int
    safeDiv _ 0 = Nothing'
    safeDiv x y = Just' (x `div` y)

    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x:xs) = Just x

    newtype Nat' = N' Int

    --type alias can be substitues, but newtype and data Type constructors cant
    type Nat'' = Int

    --better to use new type for performance reasons
    --newtype removes the safety check once the program compiles
    data Nat''' = Nat''' Int


    --recursive types
    data Nat = Zero | Succ Nat

    nat2int :: Nat -> Int
    nat2int Zero = 0
    nat2int (Succ n) = 1 + nat2int n

    int2nat :: Int -> Nat
    int2nat 0 = Zero
    int2nat n = Succ( int2nat (n-1) )

    add :: Nat -> Nat -> Nat
    add m n = int2nat (nat2int m + nat2int n)

    add' :: Nat -> Nat -> Nat
    add' Zero     n = n
    add' (Succ m) n = Succ (add m n)

    data List a = Nil | Cons a (List a)
    length' :: List a -> Int
    length' Nil = 0
    length' (Cons _ xs) = 1 + length' xs

    data Tree' a = Leaf' a | Tree' (Tree' a) a (Tree' a)

    t :: Tree' Int
    t = Tree' (Tree' (Leaf' 1) 3 (Leaf' 4)) 5 
              (Tree' (Leaf' 6) 7 (Leaf' 8))
    occurs :: Eq a => a -> Tree' a-> Bool
    occurs x (Leaf' y) = x == y
    occurs x (Tree' l y r) = x == y || occurs x l || occurs x r

    flatten' :: Tree' a -> [a]
    flatten' (Leaf' x) = [x]
    flatten' (Tree' l x r) = flatten' l ++ [x] ++ flatten' r

    occurs' :: Ord a => a -> Tree' a -> Bool
    occurs' x (Leaf' y) = x == y
    occurs' x (Tree' l y r)
        | x == y = True
        | x < y  = occurs x l
        | otherwise = occurs x r


    data Tree1 a = Leaf1 a | Node1 (Tree1 a) (Tree1 a)
    data Tree2 a = Leaf2 | Node2 (Tree2 a) a (Tree2 a)
    data Tree3 a b = Leaf3 | Node3 (Tree3 a b) b (Tree3 a b)
    data Tree a = Node a [Tree a]

    data Prop = Const Bool
                    | Var Char
                    | Not Prop
                    | And Prop Prop
                    | Imply Prop Prop deriving (Show)
    p1 :: Prop
    p1 = And (Var 'A') (Not (Var 'A'))

    p2 :: Prop
    p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

    p3 :: Prop
    p3 = Imply (Var 'A') (And (Var 'A')(Var 'B'))

    p4 :: Prop
    p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

    type Subst = Assoc Char Bool

    s :: Subst
    s = zip (enumFromTo 'A' 'B') (enumFrom False)


    eval :: Subst -> Prop -> Bool
    eval _ (Const b) = b
    eval s (Var x) = find x s
    eval s (Not p) = not (eval s p)
    eval s (And p q) = eval s p && eval s q
    eval s (Imply p q) = eval s p <= eval s q

    vars :: Prop -> [Char]
    vars (Const _) = []
    vars (Var x) = [x]
    vars (Not p) = vars p
    vars (And p q) = vars p ++ vars q
    vars (Imply p q) = vars p ++ vars q

    int2bin :: Integer -> [Integer]
    int2bin = unfold (== 0) (`mod` 2) (`div` 2)

    unfold p h t x  | p x = []
                    | otherwise = h x : unfold p h t (t x)

    bools' :: Int -> [[Bool]]
    bools' n = map (reverse . map conv . make n . int2bin ) range
                where 
                    range = [0..(2^n)-1]
                    make n bs = take n (bs ++ repeat 0)
                    conv 0 = False
                    conv 1 = True
    
    bools :: Int -> [[Bool]]
    bools 0 = [[]]
    bools n = map (False:) bss ++ map (True:) bss
                where bss = bools (n-1)

    rmdups :: Eq a => [a] -> [a]
    rmdups []     = []
    rmdups (x:xs) = x : filter (/=x) (rmdups xs)

    substs :: Prop -> [Subst]
    substs p = map (zip vs) (bools (length vs))
                    where vs = rmdups (vars p)
    
    isTaut :: Prop -> Bool
    isTaut p = and [eval s p | s <- substs p]

    data Expr = Val Int | Add Expr Expr