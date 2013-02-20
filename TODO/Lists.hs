
-- | Convert a nested dimensioned list into a nested plain list.
toList2 :: (ByX f, ByX g) => f (g a) -> [[a]]
toList2 = map toList . toList

-- | Convert a nested dimensioned list into a nested plain list.
toList3 :: (ByX f, ByX g, ByX h) => f (g (h a)) -> [[[a]]]
toList3 = map toList2 . toList

-- | Apply some function to the underlying plain list.
onList :: ByX f => ([a] -> [b]) -> f a -> f b
onList f = fromList . f . toList

-- | Prepend an element to a dimensioned list.
dcons :: ByX f => a -> f a -> f a
dcons = onList . (:)

-- | Length of a dimensioned list.
dlength :: ByX f => f a -> Int
dlength = length . toList

-- | Apply `cross` to a dimensioned list of lists.
dcross :: ByX f => f [a] -> [f a]
dcross = map fromList . cross . toList

-- | Apply zipWith to two like-dimensioned lists.
dzipWith :: ByX f => (a -> b -> c) -> f a -> f b -> f c
dzipWith f as bs = fromList (zipWith f (toList as) (toList bs))
