module Chapter4 where
    import GHC.Num

    even :: Integral a => a -> Bool
    even n = n `mod` 2 == 0

    splitAt' :: Int -> [a] -> ([a],[a])
    splitAt' n xs = (take n xs, drop n xs)

    recip :: Fractional a => a -> a
    recip n = 1 / n

    abs :: Int -> Int
    abs n = if n >=0 then n else -n

    abs' :: Int -> Int
    abs' n
        |    n >= 0      = n
        |    otherwise   = -n
    
    signum :: Int -> Int
    signum n =  if n < 0 then -1 
                else if n == 0 then 0 
                else 1
    signnum' :: Int -> Int
    signnum' n
        | n <= 0    = -1
        | n == 0    = 0
        | otherwise = 1
    
    not :: Bool -> Bool
    not True = False
    not False = True

    {-
    (&&) :: Bool -> Bool -> Bool
    True  && True   = True
    True  && False  = False
    False && True   = False
    False && False  = False 
    -}

    {-
    (&&) :: Bool -> Bool -> Bool
    True  && True   = True
    _     && _      = False
    -}

    (&&) :: Bool -> Bool -> Bool
    True  && b   = b 
    False && _   = False

    fst :: (a,b) -> a
    fst (x,_) = x

    snd :: (a,b) -> b
    snd (_,y) = y

    test :: [Char] -> Bool
    test ['a',_,_] = True
    test _         = False
 
    test' :: [Char] -> Bool
    test' ('a':_) = True
    test' _        = False

    head' :: [a] -> a
    head' (x:_) = x

    tail' :: [a] -> [a]
    tail' (_:xs) = xs

    add' :: Int -> Int -> Int
    add' x = (\y -> x + y)

    const'' :: a -> b -> a
    const'' x _ = x


    const' :: a -> b -> a
    const' x = (\_ -> x)

    odds :: Int -> [Int]
    odds n = map f [0..n-1]
             where
                f x = x*2 + 1

    odds' :: Int -> [Int]
    odds' n = map (\x -> x*2 + 1) [0..n-1]


    (#) :: Num a => a -> a -> a
    (#) = \x -> (\y -> x+y)
    x = (1#)
    y = (#1)


    halve :: [a] -> ([a],[a])
    halve xs = splitAt n xs
               where 
                    l = length xs
                    n = l `div` 2

    third :: [a] -> a
    third = undefined

    thirdA :: [a] -> a
    thirdA xs = head (tail (tail xs))

    thirdB :: [a] -> a
    thirdB xs = xs !! 2

    thirdC :: [a] -> a
    thirdC (x:y:z:xz) = z

    (||) :: Bool -> Bool -> Bool
    True || True = True
    True || False = True
    False || True = True
    False || False = False

    mult :: Int -> Int -> Int -> Int
    mult = \x -> (\y-> (\z-> x * y * z))


    luhnDouble :: Int -> Int
    luhnDouble x
        | z > 9     = z - 9
        | otherwise = z
        where z = x*2

    luhn :: Int -> Int -> Int -> Int -> 
    luhn a b c d = val == 0 
                    where
                        a' = luhnDouble a
                        b' = id b
                        c' = luhnDouble c
                        d' = id d
                        val = sum [a',b',c',d'] `mod` 10 
