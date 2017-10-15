-- The following function should take a list of Integers
-- and return the largest.
-- THE DEFINITION MUST BE RECURSIVE.
maxNum :: [Integer] -> Integer

-- empty list has no maxNum
maxNum [] =
    error "Empty list"

-- list with one element will give the maxNum as itself
maxNum (x:[]) = x

-- list with atleast 2 elements will give the maxNum to be the larger
-- let ll = [2,3,15,1,0,8,12]
-- maxNum ll = 15
-- maxNum 2:[3,15,1,0,8,12] = max 2 max of xs

-- variant #1 without the use of max function
-- maxNum (x:xs)
--     | x > max_of_xs = x
--     | otherwise = max_of_xs
--     where max_of_xs = maxNum xs

-- probable variant #2 one liner, trying out max function
maxNum (x:xs) = max x (maxNum xs)

-- Do the game fizzbuzz (http://en.wikipedia.org/wiki/Fizz_buzz).
-- Return a string counting from 1 to the specified number.
-- Replace numbers divisible by 3 with "fizz" and numbers divisible
-- by 5 with "buzz".  If a number is divisible by both 3 and 5,
-- replace it with "fizzbuzz".
fizzbuzz :: Int -> String
-- fizzbuzz n
--     | n == 0 = ""
--     | n == 1 = "1"
--     | n < 0 = error "Invalid argument"
--     | n `mod` 15 == 0 = (fizzbuzz (n-1)) ++ " fizzbuzz"
--     | n `mod` 5 == 0 = (fizzbuzz (n-1)) ++ " buzz"
--     | n `mod` 3 == 0 = (fizzbuzz (n-1)) ++ " fizz"
--     | otherwise = (fizzbuzz (n-1)) ++ " " ++ show n


-- show function is basically the toString show :: a -> String

-- fizzbuzz variation#2
fizzbuzz n
    | n < 0 = error "invalid arg"
    | n == 0 = ""
    | otherwise = (fizzbuzz (n-1)) ++ (lookup' (n `mod` 15) n)

-- Defining a new function named lookup'
lookup' :: Int -> Int -> String
lookup' n m
    | n == 0 = " fizzbuzz"
    | n == 3 = " fizz"
    | n == 5 = " buzz"
    | otherwise = " " ++ show m


main:: IO ()
main = do
    --let x1 = maxNum []
    --print x1

    let x2 = maxNum [2]
    print x2

    let x3 = maxNum [2,3,15,1,0,8,12]
    print x3

    let x4 = fizzbuzz 7
    print x4

    let x5 = fizzbuzz 15
    print x5

    let x6 = fizzbuzz 38
    print x6
