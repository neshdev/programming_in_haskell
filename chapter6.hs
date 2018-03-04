module Chapter6 where
    fac :: Int -> Int
    fac 0 = 1
    fac n = n * fac (n-1)

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


