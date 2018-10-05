foldrMap :: (a->b)->[a]->[b]
foldrMap f = foldr (\x list -> f x : list) []
--
foldlMap :: (a->b)->[a]->[b]
foldlMap f = foldl (\list x -> list ++ [f x]) []