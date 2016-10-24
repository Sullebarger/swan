--William Sullebarger

mergesort []            = []
mergesort [a]           = [a]
mergesort x             = merge (mergesort (lefthalf x)) (mergesort (righthalf x))

merge [] []             = []
merge [] y              = y
merge x []              = x
merge (x:xs) (y:ys)
        | x <= y        = [x] ++ merge xs (y:ys)
        | otherwise     = [y] ++ merge (x:xs) ys

lefthalf x              = take a x
        where a = round ((fromIntegral (length x)) /  2)

righthalf x              = drop a x
        where a = round ((fromIntegral (length x)) / 2)
