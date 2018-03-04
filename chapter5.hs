module Chapter5 where

    import Data.Char

    concat' :: [[a]] -> [a]
    concat' xss = [x | xs <- xss, x <- xs]

    firsts :: [(a,b)] -> [a]
    firsts xs = [a | (a,_) <- xs]

    length' :: [a] -> Int
    length' xs = sum [1 | _ <- xs]

    factors :: Int -> [Int]
    factors n = [x | x <- [1..n], n `mod` x == 0]

    prime :: Int -> Bool
    prime n = factors n == [1,n]


    primes :: Int -> [Int]
    primes n = [x | x <- [1..n], prime x]

    find :: Eq a => a -> [(a,b)] -> [b]
    find k t = [v | (k',v) <- t, k' == k]

    pairs :: [a] -> [(a,a)]
    pairs xs = zip xs (tail xs)

    sorted :: Ord a => [a] -> Bool
    sorted xs = and [y>=x | (x,y) <- pairs xs]

    positions :: Eq a => a -> [a] -> [Int]
    positions x xs = [i | (i,x') <- zip [0..] xs, x == x' ]

    positions' x xs = find x (zip xs [0..])

    lowers :: String -> Int
    lowers xs = length [x | x <- xs, 'a' <= x && x <= 'z' ]

    count :: Char -> String -> Int
    count x xs = length [x' | x' <- xs, x' == x]



    let2int :: Char -> Int
    let2int c = ord c - ord 'a'

    int2let :: Int -> Char
    int2let n = chr (ord 'a' + n)

    shift :: Int -> Char -> Char
    shift n c
        | isLower c = int2let ((let2int c + n) `mod` 26)
        | otherwise = c

    encode :: Int -> String -> String
    encode n xs = [shift n x | x <- xs]

    table :: [Float]
    table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
             0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
             6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]
    
    percent :: Int -> Int -> Float
    percent n m = (fromIntegral n / fromIntegral m ) * 100

    freqs :: String -> [Float]
    freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
                    where
                        n = lowers xs
    
    chisq :: [Float] -> [Float] -> Float
    chisq os es = sum [ (o-e)^2/e | (o,e) <- zip os es]

    rotate :: Int -> [a] -> [a]
    rotate n xs = drop n xs ++ take n xs

    crack :: String -> String
    crack xs = encode (-factor) xs
                where
                    factor = head (positions (minimum chitab) chitab)
                    chitab = [chisq (rotate n table') table | n <- [0..25]]
                    table' = freqs xs

    sumsqr = sum [x^2 | x <- [1..100]]


    grid :: Int -> Int -> [(Int,Int)]
    grid n m = [(n',m') | n' <- [0..n], m' <- [0..m] ]


    square :: Int -> [(Int,Int)]
    square n = [ (a,b) | (a,b) <- grid n n , a /= b ]


    replicate' :: Int -> a -> [a]
    replicate' n a = [a | x <- [1..n]]


    pyths :: Int -> [(Int,Int,Int)]
    pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], (x^2 + y^2 == z^2) ]

    perfects :: Int -> [Int]
    perfects n = [x | x <- [1..n], sum (fac x) == x]
                    where
                        fac x = init (factors x)
    

    xs = [1,2]
    ys = [3,4]
    z = concat [ [(x,y) | y <- ys ] | x <- xs  ]

    {-for x in [1,2]
        for y in [3,4]
            (x,y)
    -}

    scalarProduct :: [Int] -> [Int] -> Int
    scalarProduct xs ys = sum [ x*y | (x,y)<- zip xs ys]