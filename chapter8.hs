module Chapter8 where
    type String' = [Char]

    type Pos = (Int,Int)

    type Trans = Pos -> Pos

    -- type aliasing is not recursive
    -- type Tree = (Int, [Tree])

    type Pair a = (a,a)

    type Assoc k v = [(k,v)]

    find :: Eq k => k -> Assoc k v -> v
    find k t = head [ v | (k', v) <- t, k == k']

    data Bool' = False' | True' deriving (Show)

    data Move = North | South | East | West deriving (Show)

    move :: Move -> Pos -> Pos
    move North (x,y) = (x,y+1) 
    move South (x,y) = (x,y-1)
    move East (x,y) = (x+1,y)
    move West (x,y) = (x-1,y)

    moves :: [Move] -> Pos -> Pos
    moves [] p = p 
    moves (x:xs) p = moves xs (move x p)

    rev :: Move -> Move
    rev North = South
    rev South = North
    rev East = West
    rev West = East

    data Shape = Circle Float | Rect Float Float deriving (Show)

    square :: Float -> Shape
    square n = Rect n n

    area :: Shape -> Float
    area (Circle r) = pi * r^2
    area (Rect m n) = m * n 


    -- data constructors are already in lowest evaluated form
    -- unlike a function, something needs to be applied to make a function in its loweset form
    data Maybe' a = Nothing' | Just' a deriving (Show)

    safediv :: Int -> Int -> Maybe' Int
    safediv _ 0 = Nothing'
    safediv m n = Just' (m `div` n)

    safehead :: [a] -> Maybe' a
    safehead [] = Nothing'
    safehead xs = Just' (head xs) 

    -- enforce compiler rules, cant mistake synonyms
    newtype Nat' = N Int

    -- this is an alias, and can make a mistake using the integer value
    -- type Nat = Int

    -- this is not as efficient, using newtype, the compiler strips the type from execution
    -- data Nat = N Int

    -- recursive data type
    data Nat = Zero | Succ Nat deriving (Show)

    nat2int :: Nat -> Int
    nat2int Zero = 0
    nat2int (Succ n) = 1 + nat2int n

    int2nat :: Int -> Nat
    int2nat 0 = Zero
    int2nat n = Succ (int2nat (n-1))

    x :: Nat
    x = int2nat 10

    y :: Nat
    y = int2nat 5

    z :: Nat
    z = int2nat 10

    add' :: Nat -> Nat -> Nat
    add' m n = int2nat (nat2int m + nat2int n)


    data List' a = Nil | Cons a (List' a)

    len :: List' a -> Int
    len Nil = 0
    len (Cons _ xs) = 1 + len xs

    data Tree' a = Leaf' a | Node' (Tree' a) a (Tree' a) deriving (Show)

    t :: Tree' Int
    t = Node' (Node' (Leaf' 1) 3 (Leaf' 4)) 5 (Node' (Leaf' 6) 7 (Leaf' 8))

    occurs' :: Eq a => a -> Tree' a -> Bool
    occurs' x (Node' l x' r) = x' == x || occurs' x l || occurs' x r
    occurs' x (Leaf' x') = x' == x

    occurs :: Ord a => a -> Tree' a -> Bool
    occurs x' (Leaf' x) = x == x'
    occurs x' (Node' l x r)
        | x == x' = True
        | x' < x  = occurs x' l 
        | x' > x  = occurs x' r 

    flatten :: Tree' a -> [a]
    flatten (Leaf' x) = [x]
    flatten (Node' l x r) = flatten l ++ [x] ++ flatten r

    data Tree1 a = Leaf1 | Node1 (Tree1 a) (Tree1 a)
    data Tree2 a = Leaf2 | Node2 (Tree2 a) a (Tree2 a)
    data Tree3 a b = Leaf3 a | Node3 (Tree3 a b) b (Tree3 a b)
    data Tree4 a = Node4 a [Tree4 a]

    not' :: Bool' -> Bool'
    not' True' = False'
    not' False' = True'

    class Eq' a where
        (===) :: a -> a -> Bool'
        (/==) :: a -> a -> Bool'
        x /== y = not' (x === y)

    
    if' :: Bool' -> a -> a -> a
    if' True'  x _ = x
    if' False' _ y = y

    instance Eq' Bool' where
        False' === False' = True'
        True' === True'   = True'
        _      === _      = False'

    data Ordering' = LT' | EQ' | GT' deriving (Show)

    class Eq' a => Ord' a where
        (.<.)  :: a -> a -> Bool'
        compare' :: a -> a -> Ordering'
        (.<=.) :: a -> a -> Bool'
        (.>.)  :: a -> a -> Bool'
        (.>=.) :: a -> a -> Bool'
        min' :: a -> a -> a
        max' :: a -> a -> a

        min' x y = if' z x y 
                   where z = x .<=. y 
        
        max' x y = if' z x y
                   where z = y .<=. x
        
    instance Eq' Nat where
        Zero === Zero         = True'
        Zero === _            = False'
        _    === Zero         = False'
        (Succ n) === (Succ m) = n === m

    instance Ord' Nat where

        compare' Zero Zero           = EQ'
        compare' Zero _              = LT'
        compare' _    Zero           = GT'
        compare' (Succ n) (Succ m)   = compare' n m

        (.<.) Zero Zero         = False'
        (.<.) Zero _            = True'
        (.<.) _    Zero         = False'
        (.<.) (Succ n) (Succ m) = n .<. m

        (.<=.) Zero Zero         = True'
        (.<=.) Zero _            = True'
        (.<=.) _    Zero         = False'
        (.<=.) (Succ n) (Succ m) = n .<=. m
        
        (.>.) Zero Zero         = False'
        (.>.) Zero _            = False'
        (.>.) _    Zero         = True'
        (.>.) (Succ n) (Succ m) = n .>. m

        (.>=.) Zero Zero         = True'
        (.>=.) Zero _            = False'
        (.>=.) _    Zero         = True'
        (.>=.) (Succ n) (Succ m) = n .>=. m

    mult :: Nat -> Nat -> Nat
    mult Zero _        = Zero
    mult _    Zero     = Zero
    mult m    (Succ n) = add m (mult m n) 


    mult_nat :: Int -> Int -> Int
    mult_nat x y =  f (mult x' y') where 
                    x' = int2nat x
                    y' = int2nat y
                    f x = nat2int x

    add :: Nat -> Nat -> Nat
    add Zero     n = n
    add (Succ m) n = Succ (add m n)

    -- this version is faster because it only computers the cases once?
    occurs'' :: Ord a => a -> Tree' a -> Bool
    occurs'' x' (Leaf' x) = x == x'
    occurs'' x' (Node' l x r) = case compare x' x of
                                    LT -> occurs'' x' l
                                    GT -> occurs'' x' r
                                    EQ -> True
    
    data Treeq a = Leafq a | Nodeq (Treeq a) (Treeq a) deriving (Show)


    t1 :: Treeq Int
    t1 = Leafq 1
    t2 = Nodeq (Nodeq t1 t1) t1

    len_leaves :: Treeq a -> Int
    len_leaves (Leafq _)     = 1
    len_leaves (Nodeq l r)   = len_leaves l + len_leaves r

    balanced :: Treeq a -> Bool
    balanced (Leafq _) = True
    balanced (Nodeq l r)
        | diff == 0      = True 
        | diff == 1      = True
        | otherwise      = False
        where lSize = len_leaves l
              rSize = len_leaves r
              diff = abs (lSize - rSize)
    
    balance :: [a] -> Treeq a
    balance [x] = Leafq x
    balance xs = Nodeq (balance l) (balance r) 
                    where
                        (l,r) = split xs

    split :: [a] -> ([a],[a])
    split xs =  splitAt n xs
                where 
                    n = length xs `div` 2