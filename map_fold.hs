novoMap :: (a -> b) -> [a] -> [b]
novoMap f x = foldr (\x xs -> (f x):xs) [] x