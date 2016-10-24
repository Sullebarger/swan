--William Sullebarger

data (Ord a, Eq a) => Tree a = Node a (Tree a) (Tree a) | Empty
        deriving (Show,Eq)

insertTree a Empty              = Node a Empty Empty
insertTree a (Node at left right)
        | a <= at               = Node at (insertTree a left) right
        | otherwise             = Node at left (insertTree a right)
