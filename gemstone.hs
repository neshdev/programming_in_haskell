module GemStone where
    import Data.List    


    intersection :: String -> String -> String
    intersection [] _ = []
    intersection _ [] = []
    intersection (x:xs) (y:ys) 
        | x == y     = x : intersection xs ys
        | x < y      = intersection xs (y:ys)
        | otherwise  = intersection (x:xs) ys
        
    
    divideAndConquer :: [String] -> String
    divideAndConquer []    = []
    divideAndConquer [x] = x
    divideAndConquer [x,y] = intersection x y
    divideAndConquer xs =  intersection (divideAndConquer left) (divideAndConquer right)
                            where left    = take n xs
                                  right   = drop n xs
                                  n       = length xs `div` 2
    

    gemstones :: [String] -> Int
    gemstones xs = length . nub . divideAndConquer $ sortedString
                   where sortedString = map sort xs

    test :: [String]
    test = [ "aa", "aa" ]

    main :: IO ()
    main = print (gemstones test)
        