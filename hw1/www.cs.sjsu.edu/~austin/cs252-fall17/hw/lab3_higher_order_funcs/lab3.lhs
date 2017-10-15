> import Data.List

Experiment with foldl, foldr, and foldl'

First, implement your own version of the foldl function,
defined as myFoldl

-- > myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- > myFoldl _ _ _ = error "TBD"
-- myFoldl is foldl implementation, I belive this becomes the foldl' representation? I believe foldl' is tail-recursive?

> myFoldl :: (a -> b -> a) -> a -> [b] -> a
> myFoldl _ acc [] = acc
> myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

Next, define a function to reverse a list using foldl.

-- > myReverse :: [a] -> [a]
-- > myReverse _ = error "TBD"

-- myReverse using myFoldl

> myReverse :: [a] -> [a]
> myReverse [] = []
> myReverse (x:xs) = myFoldl (\acc element -> element : acc) [] (x:xs)


Now define your own version of foldr, named myFoldr

-- > myFoldr :: (a -> b -> b) -> b -> [a] -> b
-- > myFoldr _ _ _ = error "TBD"

-- myFoldr

> myFoldr :: (a -> b -> b) -> b -> [a] -> b
> myFoldr _ acc [] = acc
> myFoldr f acc (x:xs) = f x (myFoldr f acc xs)


Now try using foldl (the library version, not yours) to sum up the numbers of a large list.
Why is it so slow?

Instead of foldl, try using foldl'.
Why is it faster?
(Read http://www.haskell.org/haskellwiki/Foldr_Foldl_Foldl%27 for some hints)

-- For me all of the versions are slow when computing sum of range [1..10000000000]
-- But foldl' is indeed faster than foldl when computing for ranges [1..100000000] and below.


For an extra challenge, try to implement foldl in terms of foldr.
See http://www.haskell.org/haskellwiki/Foldl_as_foldr for details.

-- according to the website, http://www.haskell.org/haskellwiki/Foldl_as_foldr
-- I notice that the `id` function is used to protect values when passing functions to higher order functions
-- taking that into consideration, the author specifies that
-- foldl f x bs = foldr (\b g x -> g (f x b)) id bs a
--
-- when executed, it will unfold as
{-
  foldl f x bs = foldr (\b g x -> g (f x b)) id bs a
  ---->
    ((\b g x -> g (f x b)) (head bs) (foldr (\b g x -> g (f x b)) id (tail bs))) a
  ------>
    (.................... (foldr (\b g x -> g (f x b)) id [])) .............. ) a
  -------->
    ((\b g x -> g (f x b)) (b1) .... ((\b g x -> g (f x b)) bn-1 id))).............. ) a
  ------>
    partially applied functions
    ((\b g x -> g (f x b)) (b1) .... (id (f x bn-1)))).............. ) a
    
  --->
    ((\b g x -> g (f x b)) (b1) ....( ((\b g x -> g (f x b))) bn-2 (fbn-1 x)).............. ) a
    where fbn-1 = (f x bn-1)
  ---->
    (\b g x -> g (f x b)) (b1) ....( fbn-1 x bn-2 ).............. ) a
    fbn-k = fbn-k+1 (f x bn-k)
  
    fb2 = fb3 (f x b2)
  ----->
  ((\b g x -> g (f x b)) b1 (fb2)) a
  ----->
  (fb2 (f x b1)) a
  ------>
  fb a = will apply a as the missing paramter to the partially applied function hence leading to the computations
-}

> myFoldlUsingFoldr :: (a -> b-> a) -> a -> [b] -> a
> myFoldlUsingFoldr _ acc [] = acc
> myFoldlUsingFoldr f acc xs = myFoldr (\x g y -> g (f y x)) id xs acc


Next, using the map function, convert every item in a list to its absolute value

-- > listAbs :: [Integer] -> [Integer]
-- > listAbs _ = error "TBD"

-- convert every item in a list to its absolute value

> listAbs :: [Integer] -> [Integer]
> listAbs xs = map abs xs


Finally, write a function that takes a list of Integers and returns the sum of
their absolute values.

-- > sumAbs :: [Integer] -> Integer
-- > sumAbs _ = error "TBD"

-- function that takes a list of Integers and returns the sum of
-- their absolute values.

> sumAbs :: [Integer] -> Integer
> sumAbs xs = foldr (+) 0 $ map abs xs

-- testing out the script

> main :: IO ()
> main = do

>   putStrLn "myReverse"
>   putStrLn $ show $ myReverse ([] :: [Int])
>   putStrLn $ show $ myReverse "tea pot"
>   putStrLn $ show $ myReverse [-1,-2,3]

>   putStrLn "listAbs"
>   putStrLn $ show $ listAbs []
>   putStrLn $ show $ listAbs [-1]
>   putStrLn $ show $ listAbs [-1,-2,3]

>   putStrLn "sumAbs"
>   putStrLn $ show $ sumAbs []
>   putStrLn $ show $ sumAbs [-1]
>   putStrLn $ show $ sumAbs [-1,-2,3]
