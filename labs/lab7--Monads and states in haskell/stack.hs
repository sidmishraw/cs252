import Control.Monad.State

type Stack = [Int]

pop :: Stack -> (Int,Stack)
pop (x:xs) = (x,xs)

push :: Int -> Stack -> ((),Stack)
push a xs = ((),a:xs)

stackManip :: Stack -> (Int, Stack)  
stackManip stack = let  
    ((),newStack1) = push 3 stack  
    (a ,newStack2) = pop newStack1  
    in pop newStack2 


pop' :: State Stack Int
pop' = State $ \(x:xs) -> (x,xs)

push' :: Int -> State Stack ()
push' a = State $ \xs -> ((),a:xs)

stackManip' :: State Stack Int
stackManip' = do
  push' 3
  a <- pop'
  pop'

