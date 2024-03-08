module ListOps (
    collect,
    splitOn
) where

collect :: Monad m => [m v] -> m [v]
collect vals = reverse <$> foldl collectValues (pure []) vals where

    collectValues :: Monad m => m [v] -> m v -> m [v]
    collectValues xs v = xs >>= (`collectAppend` v)

    collectAppend :: Monad m => [v] -> m v -> m [v]
    collectAppend list = fmap (: list)


splitOn :: (Eq a) => [a] -> a -> [[a]]
splitOn [] _ = []
splitOn xs y = let reversedList = foldl (divideList y) [[]] xs in
    reverse $ map reverse reversedList where

    divideList :: (Eq a) => a -> [[a]] -> a -> [[a]]
    divideList comparator splitList incoming
        | incoming == comparator = [] : splitList
        | otherwise              = (incoming : head splitList) : tail splitList
