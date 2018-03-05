module Chapter6 where
    fac :: Int -> Int
    fac 0 = 1
    fac n | n > 0 = n * fac (n-1)

    (#) :: Int -> Int -> Int
    m # 0 = 0
    m # n = m + (m # (n-1)) 

    product' :: Num a => [a] -> a
    product' [] = 1
    product' (x:xs) = x * (product' xs)

    length' :: [a] -> Int
    length' [] = 0
    length' (_:xs) = 1 + length' xs

    reverse' :: [a] -> [a]
    reverse' [] = []
    reverse' (x:xs) = reverse' xs ++ [x]

    (##) :: [a] -> [a] -> [a]
    [] ## ys = ys
    (x:xs) ## ys = x : (xs ## ys)

    insert :: Ord a => a -> [a] ->[a]
    insert a [] = [a]
    insert a (x:xs)
        | a <= x    = a : x : xs
        | otherwise = x : insert a xs
    
    isort :: Ord a => [a] -> [a]
    isort [] = []
    isort (x:xs) = insert x (isort xs)

    zip' :: [a] -> [b] -> [(a,b)]
    zip' _ [] = []
    zip' [] _ = []
    zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

    drop' :: Int -> [a] -> [a]
    drop' 0 xs = xs
    drop' _ [] = []
    drop' n (_:xs) = drop' (n-1) xs

    fib :: Int -> Int
    fib 0 = 0
    fib 1 = 1
    fib n = fib (n-2) + fib (n-1)
                

    even' :: Int -> Bool
    even' 0 =True
    even' n = odd' (n-1)

    odd' :: Int -> Bool
    odd' 0 = False
    odd' n = even' (n-1)

    evens :: [a] -> [a]
    evens [] = []
    evens (x:xs) = x : odds xs

    odds :: [a] -> [a]
    odds [] = []
    odds (_:xs) = evens xs

    sumdown :: Int -> Int
    sumdown 0 = 0
    sumdown n = n + sumdown (n-1)

    (^.^) :: Int -> Int -> Int
    a ^.^ 0 = 1
    a ^.^ n = a * (a ^.^ (n-1))

    ecluid :: Int -> Int -> Int
    ecluid 0 b = b
    ecluid a 0 = a
    ecluid a b 
        | a >= b = ecluid b (a `rem `b)
        | a <= b = ecluid a (b `rem` a)
    
    ecluid' :: Int -> Int -> Int
    ecluid' a b | a == b = a
                | a < b  = ecluid' a (b-a)
                | b < a  = ecluid' (a-b) b
    
    and' :: [Bool] -> Bool
    and' []         = True
    and' (False:xs) = False
    and' (_:xs)     = and' xs 

    concat' :: [[a]] -> [a]
    concat' []         = []
    concat' (xs:ys)   = xs ++ (concat' ys)

    replicate' :: Int -> a -> [a]
    replicate' 0 _ = []
    replicate' n a = a : replicate' (n-1) a

    (!!!) :: [a] -> Int -> a
    (!!!) (x:_)  0 = x
    (!!!) (x:xs) n = xs !!! (n-1)  

    elem' :: Eq a => a -> [a] -> Bool
    elem' _  []                 = False   
    elem' x' (x:xs) | x' == x   = True
                    | otherwise = elem' x' xs
    
    merge :: Ord a => [a] -> [a] -> [a]
    merge []     ys              = ys
    merge xs     []              = xs
    merge (x:xs) (y:ys) | x <= y = x : merge xs     (y:ys)
                        | y <= x = y : merge (x:xs) ys
    
    msort :: Ord a => [a] -> [a]
    msort []   = []
    msort (x:[]) = x:[]
    msort xs   = merge (msort l) (msort r)
                    where
                        (l,r) = halve xs

    halve :: [a] -> ([a],[a])
    halve xs = (take n xs, drop n xs) where n = (length xs) `div` 2

    sum' :: Num a => [a] -> a
    sum' []     = 0
    sum' (x:xs) = x + sum' xs  

    take' :: Int -> [a] -> [a]
    take' 0 _      = []
    take' n []     = []
    take' n (x:xs) = x : take' (n-1) xs

    last' :: [a] -> a
    last' (x:[]) = x
    last' (x:xs) = last' xs




