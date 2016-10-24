--William Sullebarger

numWeird' :: [Int] -> Int
numWeird' [] = 0
numWeird' x  = head x + numWeird'(tail x)

numWeird'' :: [Int] -> Int
numWeird'' x
        | null x                = 0
        | odd(head x)           = 1 + numWeird''(tail x)
        | otherwise             = 0 + numWeird''(tail x)

numWeird''' :: [Int] -> Int
numWeird''' x
        | null x                = 0
        | even(head x)          = 1 + numWeird'''(tail x)
        | otherwise             = 0 + numWeird'''(tail x)

numWeird :: [Int] -> Int
numWeird x
        | length [y | y <- x, odd y] == 3       = numWeird' x
        | otherwise                             = (numWeird'' x) - (numWeird''' x)
