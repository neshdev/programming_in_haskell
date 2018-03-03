module Chapter1 where
    double x = x + x


    sum' :: Num a => [a] -> a
    sum' [] = 0
    sum' (n:ns) = n + sum'(ns)


    qsort :: Ord a => [a] -> [a]
    qsort [] = []
    qsort (x:xs) =  qsort smaller ++ [x] ++ qsort larger 
                    where 
                        smaller = [z | z <-xs, z <= x]
                        larger = [z | z <-xs, z > x]
    product' :: Num a => [a] -> a
    product' [] = 1
    product' (x:xs) = x * product' xs
    
    qsortReverse :: Ord a => [a] -> [a]
    qsortReverse [] = []
    qsortReverse (x:xs) =  qsortReverse larger ++ [x] ++ qsortReverse smaller
                    where 
                        smaller = [z | z <-xs, z < x]
                        larger = [z | z <-xs, z >= x]
 
    qsort' :: Ord a => [a] -> [a]
    qsort' [] = []
    qsort' (x:xs) =  qsort' smaller ++ [x] ++ qsort' larger 
                    where 
                        smaller = [z | z <-xs, z < x]
                        larger = [z | z <-xs, z > x]