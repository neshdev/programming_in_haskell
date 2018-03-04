module Chapter3 where
    add' :: (Int,Int) -> Int
    add' (x,y) = x + y

    add'' :: Int -> (Int -> Int)
    add'' x y = x + y


    --function applicatin is left associative
    --function arrow -> is right associative
    mult :: Int -> (Int -> (Int -> Int))
    mult x y z = x * y * z

    zeroto :: Int -> [Int]
    zeroto n = [0..n]

    _1a :: [Char]
    _1a = ['a','b','c']

    _1b :: (Char,Char,Char)
    _1b = ('a','b','c')

    _1c :: [(Bool,Char)]
    _1c = [(False,'0'),(True,'1')]

    _1d :: ([Bool],[Char])
    _1d = ([False,True],['0','1'])

    _1e :: [[a]->[a]]
    _1e = [tail,init,reverse]
    
    bools :: [Bool]
    bools = [True,False]

    nums :: [[Int]]
    nums = [[1,2,3],[4,5,6],[7,8,9]]

    add :: Int -> Int -> Int -> Int
    add x y z = x + y + z

    copy :: a -> (a,a)
    copy a = (a,a)

    apply :: (a->b) -> a -> b
    apply f a = f a

    second :: [a] -> a
    second xs = head (tail xs)

    swap :: (a,b) -> (b,a)
    swap (x,y) = (y,x)

    pair :: a -> b -> (a,b)
    pair a b = (a,b)

    double :: Num a => a -> a
    double x = 2*x

    palindrome :: Eq a => [a] -> Bool
    palindrome xs = reverse xs == xs

    twice :: (a->a) -> a -> a
    twice f x = f (f x)

    {-
        It is impossible to implement Eq on function types because you need to evaluate the function to find the result. Eq constraint
        is a check on the type and not on the runtime.
        To make it possible, the functions would need to take in the same value and output the same value
    -}
