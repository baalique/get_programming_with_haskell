maxPairMaybe :: Ord a => Maybe (a, a) -> Maybe a
maxPairMaybe Nothing       = Nothing
maxPairMaybe (Just (x, y)) = Just $ max x y

maxPairM :: (Monad m, Ord a) => m (a, a) -> m a
maxPairM = (>>= (\(a, b) -> return $ max a b))
