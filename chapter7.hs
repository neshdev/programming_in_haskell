module Chapter7 where

    import Data.Char
    import Data.List
    import GHC.Enum

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

    xs' = ['a'..'d']
    
    z :: Char -> [Char] -> [Char]
    z c xs = "(" ++ (show c) ++ ":" ++ xs ++ ")"

    str = foldr' z "[]" xs' 


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

    foldl'' :: (a->b->a) -> a -> [b] -> a
    foldl'' f v [] = v 
    foldl'' f v (x:xs) = foldl'' f (f v x) xs

    str1 = foldl' (flip z) "[]" xs'


    (#) :: (b->c) -> (a->b) -> (a->c)
    f # g = \x -> f (g x)

    id' :: a -> a
    id' = \x -> x

    compose' :: [(a -> a)] -> (a -> a)
    compose' = foldr' (#) id'

    -- transmit
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

    encode :: String -> [Bit]
    encode = concat . map (make8 . int2bit . ord)

    chop8 :: [Bit] -> [[Bit]]
    chop8 [] = []
    chop8 bits = take 8 bits : chop8 (drop 8 bits)

    decode :: [Bit] -> String
    decode = map (chr . bin2int) . chop8

    transmit :: String -> String
    transmit = decode . channel . encode

    channel :: [Bit] -> [Bit]
    channel = id

    -- first past the post 
    votes :: [String]
    votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

    count :: Eq a => a -> [a] -> Int
    count x = length . filter (== x) 

    rmdups :: Eq a => [a] -> [a]
    rmdups []     = []
    rmdups (x:xs) = x : filter (/=x) (rmdups xs)

    result :: Ord a => [a] -> [(Int,a)]
    result vs = sort [(count x vs, x) | x <-rmdups vs]

    winner :: Ord a => [a] -> a 
    winner = snd . last . result

    -- Alternate vote
    ballots :: [[String]]
    ballots = [["Red","Green"],
                ["Blue"],
                ["Green", "Red", "Blue"],
                ["Blue", "Green", "Red"],
                ["Green"]]
    
    rmempty :: Eq a => [[a]] -> [[a]]
    rmempty = filter (/= [])

    elim :: Eq a => a -> [[a]] -> [[a]]
    elim x = map (filter (/= x))

    rank :: Ord a => [[a]] -> [a]
    rank = map snd . result . map head

    winner' :: Ord a => [[a]] -> a
    winner' bs = case rank (rmempty bs) of
                    [c] -> c
                    (c:cs) -> winner' (elim c bs)

    p :: Eq a => (a->b) -> (a->Bool) -> [a] -> [b]
    p  f pred xs = [f x | x <- xs, pred x]


    p' :: Eq a => (a->b) -> (a->Bool) -> [a] -> [b]
    p' f pred xs = map f (filter pred xs)

    all' :: (a-> Bool) -> [a] -> Bool
    all' f = foldr (\a b -> (f a) && b ) True

    any' :: (a -> Bool) -> [a] -> Bool
    any' f = foldr (\a b -> (f a) || b) False
    
    f1 x = (x `mod` 2 == 1)
    x1 = enumFromThenTo 2 4 10

    takeWhile' :: (a->Bool) -> [a] -> [a]
    takeWhile' _ [] = []
    takeWhile' f (x:xs)
        | f x       = x : takeWhile' f xs
        | otherwise = takeWhile' f [] 
    
    dropWhile' :: (a->Bool) -> [a] -> [a]
    dropWhile' _ [] = []
    dropWhile' f (x:xs)
        | f x       = dropWhile f xs
        | otherwise = x:xs

    mapFoldr :: (a->b) -> [a] -> [b]
    mapFoldr f = foldr (\a b -> (f a) : b ) []

    mapFilter :: (a->Bool) -> [a] -> [a]
    mapFilter f = foldr (\a b -> if f a then a : b else b) []

    dec2int :: [Int] -> Int
    dec2int = foldl (\b a -> b*10 + a ) 0 

    curry' :: ((a,b) -> c) -> a -> b ->c
    curry' f = \x y -> f (x,y)

    uncurry' :: (a -> b -> c) -> (a,b) -> c
    uncurry' f (x,y) = f x y

    unfold p h t x  | p x = []
                    | otherwise = h x : unfold p h t (t x)
    
    int2bin' :: Int -> [Int]
    int2bin' = unfold (== 0) (`mod` 2) (`div` 2)

    chop8' :: [Int] -> [[Int]]
    chop8' = unfold null (take 8) (drop 8)

    mapUnfold :: (a->b) -> [a] -> [b]
    mapUnfold f = unfold null (f . head) tail

    iterate' :: (a->a) -> a -> [a]
    iterate' f = unfold (const False) id f 

    altMap :: (a->b) -> (a->b) -> [a] ->[b]
    altMap _ _ [] = []
    altMap f _ [x] = [f x]
    altMap f g (y1:y2:ys) = f y1 : g y2 : altMap f g ys


    luhnDouble :: Int -> Int
    luhnDouble x
        | n > 9     = n - 9
        | otherwise = n
        where n = x*2

    luhn :: [Int] -> Bool
    luhn ns = f (altMap luhnDouble id ns)
                where
                    f ::  Integral a => [a] -> Bool
                    f xs = (sum xs `mod` 10) == 0
