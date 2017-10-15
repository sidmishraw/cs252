mydiv x y =
  x >>= (\numer ->
  y >>= (\denom ->
    if denom > 0
      then Just $ numer `div` denom
      else Nothing))

mydiv' x y = do
  numer <- x
  denom <- y
  if denom > 0
    then return $ numer `div` denom
    else Nothing

test1 = (Just 99) `mydiv` (Just 11)
test1' = (Just 99) `mydiv'` (Just 11)

test2 = (Just 9) `mydiv` (Just 0)
test2' = (Just 9) `mydiv'` (Just 0)


