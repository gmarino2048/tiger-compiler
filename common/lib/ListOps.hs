module ListOps (
    splitOn
) where

splitOn :: (Eq a) => [a] -> a -> [[a]]
splitOn [] _ = []
splitOn xs y = let reversedList = foldl (divideList y) [[]] xs in
    reverse $ map reverse reversedList where

    divideList :: (Eq a) => a -> [[a]] -> a -> [[a]]
    divideList comparator splitList incoming
        | incoming == comparator = [] : splitList
        | otherwise              = (incoming : head splitList) : tail splitList