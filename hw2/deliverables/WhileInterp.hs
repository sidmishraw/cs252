{-
  Name: Sidharth Mishra
  Class: CS 252
  Assigment: HW2
  Date: 9/28/2017 11:59 PM
  Description: Interpreter for the WHILE language
-}


module WhileInterp (
  Expression(..),
  Binop(..),
  Value(..),
  testProgram,
  run
) where

import Data.Map (Map)
import qualified Data.Map as Map

-- We represent variables as strings.
type Variable = String

-- The store is an associative map from variables to values.
-- (The store roughly corresponds with the heap in a language like Java).
type Store = Map Variable Value

data Expression =
    Var Variable                            -- x
  | Val Value                               -- v
  | Assign Variable Expression              -- x := e
  | Sequence Expression Expression          -- e1; e2
  | Op Binop Expression Expression
  | If Expression Expression Expression     -- if e1 then e2 else e3
  | While Expression Expression             -- while (e1) e2
  | Not Expression                          -- not e
  | And Expression Expression               -- e and e
  | Or Expression Expression                -- e or e
  deriving (Show)

data Binop =
    Plus     -- +  :: Int  -> Int  -> Int
  | Minus    -- -  :: Int  -> Int  -> Int
  | Times    -- *  :: Int  -> Int  -> Int
  | Divide   -- /  :: Int  -> Int  -> Int
  | Gt       -- >  :: Int -> Int -> Bool
  | Ge       -- >= :: Int -> Int -> Bool
  | Lt       -- <  :: Int -> Int -> Bool
  | Le       -- <= :: Int -> Int -> Bool
  deriving (Show)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show)


-- This function will be useful for defining binary operations.
-- The first case is done for you.
-- Be sure to explicitly check for a divide by 0 and throw an error.
applyOp :: Binop -> Value -> Value -> Value
applyOp Plus (IntVal i) (IntVal j) = IntVal $ i + j
applyOp Minus (IntVal i) (IntVal j) = IntVal $ i - j
applyOp Times (IntVal i) (IntVal j) = IntVal $ i * j
applyOp Divide (IntVal i) (IntVal j) = IntVal $ div i j
applyOp Gt (IntVal i) (IntVal j) = BoolVal $ i > j
applyOp Ge (IntVal i) (IntVal j) = BoolVal $ i >= j
applyOp Lt (IntVal i) (IntVal j) = BoolVal $ i < j
applyOp Le (IntVal i) (IntVal j) = BoolVal $ i <= j
applyOp _ _ _ = error "not supported"


-- Implement this function according to the specified semantics
-- I've modified the implementation a bit, HAVEN"T MODIFIED THE TYPE SIGNATURES
-- I like this method better since it simulates the definition of small step operational semantics
-- evaluate recursively until you get to the normal form i.e the value
evaluate :: Expression -> Store -> (Value, Store)
evaluate (Val v) s = (v, s)
evaluate e s = evaluate e' s'
  where (e', s') = evaluate' e s

-- using small step operational semantics
evaluate':: Expression -> Store -> (Expression, Store)

-- SS-VALUE
evaluate' (Val v) s = (Val v, s)

-- SS-VARIABLE-ACCESS-REDUCTION
evaluate' (Var x) s = case (Map.lookup x s) of
                              (Just v) -> (Val v,s)
                              Nothing -> error "not there in the store!"

-- SS-ASSIGNMENT-REDUCTION 
evaluate' (Assign x (Val v)) s = (Val v, Map.insert x v s)

-- SS-ASSIGNMENT-CONTEXT
evaluate' (Assign x e) s = ((Assign x e'), s')
  where (e', s') = (evaluate' e s)

-- SS-SEQUENCE-REDUCTION
evaluate' (Sequence (Val _) e) s = (e, s)

-- SS-SEQUENCE-CONTEXT
evaluate' (Sequence e1 e2) s = ((Sequence e1' e2), s')
  where (e1', s') = evaluate' e1 s

-- SS-OPERATION-REDUCTION
evaluate' (Op op (Val v1) (Val v2)) s = (Val v, s)
  where v = applyOp op v1 v2

-- SS-OPERATION-CONTEXT #2
evaluate' (Op op (Val v) e) s = ((Op op (Val v) e'),s')
  where (e', s') = evaluate' e s

-- SS-OPERATION-CONTEXT #1
evaluate' (Op op e1 e2) s = ((Op op e1' e2), s')
  where (e1', s') = evaluate' e1 s

-- SS-IF-TRUE-REDUCTION
evaluate' (If (Val (BoolVal True)) e1 e2) s = (e1, s)

-- SS-IF-FALSE-REDUCTION
evaluate' (If (Val (BoolVal False)) e1 e2) s = (e2, s)

-- IF-IntVal not supported (IntVal is not supported in if condition)
evaluate' (If (Val _) e1 e2) s = error "Not supported"

-- SS-IF-CONTEXT
evaluate' (If e1 e2 e3) s = ((If e1' e2 e3), s)
  where (e1', s') = evaluate' e1 s

-- SS-WHILE-CONTEXT
evaluate' (While e1 e2) s = ((If e1 (Sequence e2 (While e1 e2)) (Val (BoolVal False))), s)

-- -- SS-NOT-TRUE-REDUCTION
-- evaluate' (Not (Val (BoolVal True))) s = (Val $ BoolVal False, s)

-- --  SS-NOT-FALSE-REDUCTION
-- evaluate' (Not (Val (BoolVal False))) s = (Val $ BoolVal True, s)

-- -- Not IntVal not supported
-- evaluate' (Not (Val _)) s = error "not supported"

-- SS-NOT-CONTEXT
evaluate' (Not e) s = ((If e (Val $ BoolVal False) (Val $ BoolVal True)), s)

-- --  SS-AND-TRUE-TRUE-REDUCTION
-- evaluate' (And (Val (BoolVal True)) (Val (BoolVal True))) s = (Val $ BoolVal True, s)

-- -- SS-AND-TRUE-FALSE-REDUCTION
-- evaluate' (And (Val (BoolVal True)) (Val (BoolVal False))) s = (Val $ BoolVal False, s)

-- -- SS-AND-FALSE-REDUCTION
-- evaluate' (And (Val (BoolVal False)) _) s = (Val $ BoolVal False, s)

-- -- SS-AND-TRUE-CONTEXT
-- evaluate' (And (Val (BoolVal True)) e) s = (And (Val (BoolVal True)) e', s')
--   where (e', s') = evaluate' e s

-- SS-AND-CONTEXT
evaluate' (And e1 e2) s = (If e1 (If e2 (Val $ BoolVal True) (Val $ BoolVal False)) (Val $ BoolVal False), s)

-- -- SS-OR-FALSE-FALSE-REDUCTION
-- evaluate' (Or (Val (BoolVal False)) (Val (BoolVal False))) s = (Val $ BoolVal False, s)

-- -- SS-OR-FALSE-TRUE-REDUCTION 
-- evaluate' (Or (Val (BoolVal False)) (Val (BoolVal True))) s = (Val $ BoolVal True, s)

-- -- SS-OR-TRUE-REDUCTION
-- evaluate' (Or (Val (BoolVal True)) _) s = (Val $ BoolVal True, s)

-- -- SS-OR-VAL-CONTEXT
-- evaluate' (Or (Val v) e) s = ((Or (Val v) e'), s')
--   where (e', s') = evaluate' e s

-- SS-OR-CONTEXT
evaluate' (Or e1 e2) s = (If e1 (Val $ BoolVal True) (If e2 (Val $ BoolVal True) (Val $ BoolVal False)), s)


-- Evaluates a program with an initially empty state
run :: Expression -> (Value, Store)
run prog = evaluate prog Map.empty

-- The same as run, but only returns the Store
testProgram :: Expression -> Store
testProgram prog = snd $ run prog


