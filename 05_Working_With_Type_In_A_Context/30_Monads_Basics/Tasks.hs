allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM f a = a >>= (\x -> return $ f x)

allApp :: Monad m => m (a -> b) -> m a -> m b
allApp f a = f >>= (\g -> a >>= (\x -> return $ g x))

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing  _ = Nothing
bind (Just x) f = f x
