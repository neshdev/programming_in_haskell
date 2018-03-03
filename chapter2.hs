module Chapter2 where 
    double :: Num a => a -> a
    double a = 2*a

    quadruple :: Num a => a -> a
    quadruple a = double (double a)

    factorial n = product [1..n]

    average ns = sum ns `div` length ns

    n = a `div` length xs
        where 
            a = 10
            xs = [1..5]
    
    last' :: [a] -> a
    last' xs = head (reverse xs)

    last'' :: [a] -> a
    last'' xs = xs !! ((length xs) - 1)

    init' :: [a] -> [a]
    init' xs = take (length xs - 1) xs

    init'' :: [a] -> [a]
    init'' xs = reverse (tail (reverse xs))




