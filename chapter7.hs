module Chapter7 where

    import Data.Char

    add :: Int -> Int -> Int
    add x y = x + y

    add' :: Int -> (Int -> Int)
    add' = \x -> (\y -> x + y)

    twice :: (a -> a ) -> a -> a
    twice f x = f (f x)

    map' :: (a->b) -> [a] -> [b]
    map' f xs = [f x | x <- xs ]

    map'' :: (a->b) -> [a] -> [b]
    map'' f [] = []
    map'' f (x:xs) = f x : map'' f xs

    filter' :: (a -> Bool) -> [a] -> [a]
    filter' f [] = []
    filter' f (x:xs)
            | f x       = x : filter' f xs
            | otherwise = filter' f xs
    
    sumsqreven :: [Int] -> Int
    sumsqreven ns = sum (map (^2) (filter even ns))

    foldr' :: (a->b->b) -> b -> [a] -> b
    foldr' _  v []      = v
    foldr' f  v (x:xs)  = f x (foldr' f v xs)

    xs = ['a'..'d']
    
    z :: Char -> [Char] -> [Char]
    z c xs = "(" ++ (show c) ++ ":" ++ xs ++ ")"

    str = foldr' z "[]" xs 


    length' :: [a] -> Int
    length' = foldr' (\_ n -> 1+n) 0

    and' :: [Bool] -> Bool
    and' = foldr' (&&) True

    or' :: [Bool] -> Bool
    or' = foldr' (||) False

    product' :: Num a => [a] -> a
    product' = foldr' (*) 1

    sum' :: Num a => [a] -> a
    sum' = foldr' (+) 0

    
    flip' :: (a->b->c) -> b -> a -> c
    flip' f = \x y -> f y x

    foldl' :: (a->b->a) -> a -> [b] -> a
    foldl' f v [] = v 
    foldl' f v (x:xs) = foldl' f (f v x) xs

    str1 = foldl' (flip z) "[]" xs


    (#) :: (b->c) -> (a->b) -> (a->c)
    f # g = \x -> f (g x)

    id' :: a -> a
    id' = \x -> x

    compose' :: [(a -> a)] -> (a -> a)
    compose' = foldr' (#) id'

    type Bit = Int

    bin2int' :: [Bit] -> Int
    bin2int' bits = sum [w*b | (w,b) <- zip weights bits]
                    where weights = iterate (*2) 1
    
    bin2int :: [Bit] -> Int
    bin2int = foldr (\x y -> x + 2*y) 0 

    int2bit :: Int -> [Bit]
    int2bit 0 = []
    int2bit n = n `mod` 2 : int2bit (n `div` 2)

    make8 :: [Bit] -> [Bit]
    make8 bits = take 8 (bits ++ repeat 0)
