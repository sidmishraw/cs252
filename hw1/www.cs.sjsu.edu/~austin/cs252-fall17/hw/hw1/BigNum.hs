{-
  Name: Sidharth Mishra
  Class: CS 252
  Assigment: HW1
  Date: 9/5/2017 11:59 PM PT
  Description: BigNum implementation as per HW 1 for CS 252
-}

module BigNum (
  BigNum,
  bigAdd,
  bigSubtract,
  bigMultiply,
  bigEq,
  bigDec,
  bigPowerOf,
  prettyPrint,
  stringToBigNum,
) where

-- for debugging, sad debugs Haskell!
import Debug.Trace

type Block = Int -- An Int from 0-999

type BigNum = [Block]

maxblock = 1000

bigAdd :: BigNum -> BigNum -> BigNum
bigAdd x y = bigAdd' x y 0

bigAdd' :: BigNum -> BigNum -> Block -> BigNum
-- c is the carry
bigAdd' [] [] c
  | c > 0 = stringToBigNum $ show c
  | otherwise = []

bigAdd' (x:[]) (y:[]) c = stringToBigNum $ show (x + y + c)

bigAdd' (x:xs) [] c = newBlock : (bigAdd' xs [] carry)
  where newSum = stringToBigNum $ show (x + c)
        newBlock = newSum !! 0
        carry = getCarry newSum
        -- newBlock = trace ("newSum= " ++ (show (newSum !! 0))) $ newSum !! 0
        -- carry = trace ("carry = " ++ (show $ getCarry newSum)) $ getCarry newSum

bigAdd' [] (y:ys) c = bigAdd' (y:ys) [] c

bigAdd' (x:xs) (y:ys) c = newBlock : (bigAdd' xs ys carry)
  where newSum = stringToBigNum $ show (x + y + c)
        newBlock = newSum !! 0
        carry = getCarry newSum
        -- newBlock = trace ("newSum= " ++ (show (newSum !! 0))) $ newSum !! 0
        -- carry = trace ("carry = " ++ (show $ getCarry newSum)) $ getCarry newSum

-- bigAdd' _ _ _ = error "OOPS! Not implemented :P"

-- computes the carry part from the bignum
getCarry :: BigNum -> Block
getCarry bignum
  | (length bignum) >= 2 = bignum !! 1
  | otherwise = 0


bigSubtract :: BigNum -> BigNum -> BigNum
bigSubtract x y =
  if length x < length y
    then error "Negative numbers not supported"
    else reverse $ stripLeadingZeroes $ reverse result
      where result = bigSubtract' x y 0

stripLeadingZeroes :: BigNum -> BigNum
stripLeadingZeroes (0:[]) = [0]
stripLeadingZeroes (0:xs) = stripLeadingZeroes xs
stripLeadingZeroes xs = xs

-- Negative numbers are not supported, so you may throw an error in these cases
bigSubtract' :: BigNum -> BigNum -> Block -> BigNum

bigSubtract' [] [] b
  | b > 0 = stringToBigNum $ show (b * maxblock)
  | otherwise = [0]

bigSubtract' [] (y:ys) b = error "Negative nbrs not supported"

bigSubtract' (x:[]) (y:[]) b
    | (diff >= 0) = stringToBigNum $ show diff
    | otherwise = error "Neg Nbr"
    where diff = (x - b - y)

bigSubtract' (x:xs) [] b
  -- | b > 0 = trace ("3 newBlock = " ++ show (x - b)) $ (x - b) : xs
  | (b > 0) && (x > 0) = (x - b) : (bigSubtract' xs [] 0)
  | (b > 0) && (x == 0)= (((x - b) + (1 * maxblock)) : bigSubtract' xs [] 1)
  | b < 0 = error "Borrow is -ve? Really?"
  -- | otherwise = trace ("4 newBlock = " ++ show (x)) $ (x:xs)
  | b == 0 = (x:xs)

  -- trace ("nBl = " ++ show newBlock) $ newBlock : (bigSubtract' xs ys newB)
bigSubtract' n1@(x:xs) n2@(y:ys) b
  | isvalid == False = error "Neg Nbr" 
  -- | isvalid && b >= 0 = trace ("1 newBlock = " ++ show newBlock) $ newBlock : (bigSubtract' xs ys newB)
  | isvalid && b >= 0 = newBlock : (bigSubtract' xs ys newB)
  | b < 0 = error "Negative borrow ? Really?"
  where (newBlock, newB) = computeDiff x y b
        isvalid = isGreaterEq (reverse n1) (reverse n2) True

-- great that >= works!
-- flawed experiment, can be dropped
isGreaterEq :: BigNum -> BigNum -> Bool -> Bool

isGreaterEq [] [] acc = True && acc

isGreaterEq n1@(x:xs) n2@(y:ys) acc
    | (n1 == []) && (n2 == []) = True && acc
    | (length n1) > (length n2) = True && acc
    | (length n1) < (length n2) = False && acc
    | otherwise = isGreaterEq xs ys ((x >= y) && acc)

computeDiff :: Block -> Block -> Block -> (Block,Block)
computeDiff x y b
  | x >= y = ((x - b) - y, 0)
  | otherwise = (((x - b) + (1 * maxblock) - y), 1)

-- bigSubtract' _ _ _ = error "Your code here"

bigEq :: BigNum -> BigNum -> Bool
bigEq x y = (x == y)
-- bigEq _ _ = error "Your code here"

bigDec :: BigNum -> BigNum
bigDec x = bigSubtract x [1]

bigMultiply :: BigNum -> BigNum -> BigNum

bigMultiply [] _ = [0]
bigMultiply _ [] = [0]
bigMultiply [0] _ = [0]
bigMultiply _ [0] = [0]

-- bigMultiply (x:[]) (y:[]) = stringToBigNum $ show (x * y)

-- repeated addition way
-- slow but proper
bigMultiply x y = product x [0] y
    where product x acc y
            -- | y > [0] = trace ("y = " ++ show y) $ product x (bigAdd acc x) (bigDec y)
            | y > [0] = product x (bigAdd acc x) (bigDec y)
            -- | otherwise = trace ("acc = " ++ prettyPrint acc) acc
            | otherwise = acc

-- bigMultiply _ _ = error "Your code here"

-- tail-recursion ?
-- repDec :: BigNum -> BigNum -> BigNum
-- repDec x y
--     | y > [0] = trace ("y=" ++ show y) $ repDec (bigDec x) (bigDec y)
--     | otherwise = x

bigPowerOf :: BigNum -> BigNum -> BigNum

bigPowerOf [] _ = [1]
bigPowerOf _ [] = [1]
bigPowerOf [0] _ = [1]
bigPowerOf _ [0] = [1]

-- x ^ y
-- bigPowerOf (x:[]) (y:[]) = trace ("Power = " ++ show (x ^ y)) $ stringToBigNum $ show (x ^ y)

-- will use repeated multiplication way here
-- Not optimal -- passes test cases
bigPowerOf x y = powerOf x [1] y
    where powerOf x acc y
            | y > [0] = powerOf x (bigMultiply acc x) (bigDec y)
            | otherwise = acc

-- bigPowerOf _ _ = error "Your code here"

prettyPrint :: BigNum -> String
prettyPrint [] = ""
prettyPrint xs = show first ++ prettyPrint' rest
  where (first:rest) = reverse xs

prettyPrint' :: BigNum -> String
prettyPrint' [] = ""
prettyPrint' (x:xs) = prettyPrintBlock x ++ prettyPrint' xs

prettyPrintBlock :: Int -> String
prettyPrintBlock x | x < 10     = ",00" ++ show x
                   | x < 100    = ",0" ++ show x
                   | otherwise  = "," ++ show x

stringToBigNum :: String -> BigNum
stringToBigNum "0" = [0]
stringToBigNum s = stringToBigNum' $ reverse s

stringToBigNum' :: String -> BigNum
stringToBigNum' [] = []
stringToBigNum' s | length s <= 3 = read (reverse s) : []
stringToBigNum' (a:b:c:rest) = block : stringToBigNum' rest
  where block = read $ c:b:a:[]


