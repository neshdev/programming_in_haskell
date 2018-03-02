module Chapter1 where
    double x = x + x


    sum' :: Num a => [a] -> a
    sum' [] = 0
    sum' (n:ns) = n + sum'(ns)


    qsort :: Ord a => [a] -> [a]
    qsort [] = []
    qsort (x:xs) =  qsort smaller ++ [x] ++ qsort larger where 
                   smaller = [z | z <-xs, z <= x]
                   larger = [z | z <-xs, z > x]