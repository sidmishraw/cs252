applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b  
applyMaybe Nothing f  = Nothing  
applyMaybe (Just x) f = f x

test1 = Just 3 `applyMaybe` (\x -> Just $ x * 2) `applyMaybe` (\x -> Just $ x - 1)

test2 = Just 3 `applyMaybe` (\_ -> Nothing) `applyMaybe` (\x -> Just $ x - 1)

