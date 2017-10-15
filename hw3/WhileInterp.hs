{-
  Name: Sidharth Mishra
  Class: CS 252
  Assigment: HW3
  Date: Oct 17, 2017 : 11:59 PM
  Description: Parser and interpreter for the WHILE imperative language
-}

module WhileInterp (
  Expression(..),
  Binop(..),
  Value(..),
  runFile,
  showParsedExp,
  run
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Control.Monad.Except

-- We represent variables as strings.
type Variable = String

--We also represent error messages as strings.
type ErrorMsg = String

-- The store is an associative map from variables to values.
-- (The store roughly corresponds with the heap in a language like Java).
type Store = Map Variable Value

data Expression =
  -- Expressions for the WHILE language
    Var Variable                            -- x
  | Val Value                               -- v
  | Assign Variable Expression              -- x := e
  | Sequence Expression Expression          -- e1; e2
  | Op Binop Expression Expression
  | If Expression Expression Expression     -- if e1 then e2 else e3 endif
  | While Expression Expression             -- while e1 do e2 endwhile
  -- added by sidmishraw for extending the language with and, or and not
  -- as per hw2
  | Not Expression                          -- not e
  | And Expression Expression               -- e and e
  | Or Expression Expression                -- e or e
  deriving (Show)

data Binop =
  -- Binary operators
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
  -- values
    IntVal Int
  | BoolVal Bool
  deriving (Show)


fileP :: GenParser Char st Expression
-- the .imp file parser
fileP = do
  prog <- exprP
  eof
  return prog

exprP = do
-- the expression parser
  e <- exprP'
  rest <- optionMaybe restSeqP
  return $ case rest of
    Nothing   -> e
    Just e' -> Sequence e e'

-- Expressions are divided into terms and expressions for the sake of
-- parsing.  Note that binary operators **DO NOT** follow the expected
-- precedence rules.
--
-- ***FOR 2pts EXTRA CREDIT (hard, no partial credit)***
-- Correct the precedence of the binary operators.
exprP' = do
  spaces
  t <- termP
  spaces
  rest <- optionMaybe restP
  spaces
  case rest of
    Nothing -> return t
    Just (":=", t') -> case t of
      Var varName -> return $ Assign varName t'
      _ -> error "Expected var"
    Just ("and", t') -> return $ And t t' -- for `and`
    Just ("or", t') -> return $ Or t t' -- for `or`
    Just (op, t') -> return $ Op (transOp op) t t'

restSeqP = do
  char ';'
  exprP

-- transOp transforms the operator from the text form to
-- Haskell function
transOp s = case s of
  "+"  -> Plus
  "-"  -> Minus
  "*"  -> Times
  "/"  -> Divide
  ">=" -> Ge
  ">"  -> Gt
  "<=" -> Le
  "<"  -> Lt
  o    -> error $ "Unexpected operator " ++ o

-- Some string, followed by an expression
restP = do
  ch <- string "+"
    <|> string "-"
    <|> string "*"
    <|> string "/"
    <|> try (string "<=") -- for arbitary, lookahead, looks ahead for the "=" without consuming the "<"
    <|> string "<"
    <|> try (string ">=")
    <|> string ">"
    <|> string ":=" -- not really a binary operator, but it fits in nicely here.
    <|> string "and" -- works for and and or since they are binary operators
    <|> string "or"
    <?> "binary operator"
  e <- exprP'
  return (ch, e)

-- All terms can be distinguished by looking at the first character
termP = valP
    <|> notP -- for Not :: not e
    <|> ifP
    <|> whileP
    <|> parenP
    <|> varP
    <?> "value, variable, 'if', 'while', or '(' or 'not'"

notP = do
-- not e
  spaces
  string "not"
  e <- optionMaybe exprP
  case e of
    Nothing -> error "Nothing there"
    Just e' -> return $ Not e'

valP = do
  v <- boolP <|> numberP
  return $ Val v

boolP = do
-- parser for boolean values, parses boolean values in text
-- to provide with Haskell implementation
  bStr <- try (string "true") <|> try (string "false") <|> try (string "skip")
  return $ case bStr of
    "true" -> BoolVal True
    "false" -> BoolVal False
    "skip" -> BoolVal False -- Treating the command 'skip' as a synonym for false, for ease of parsing

numberP = do
-- parser for numbers, parses the numbers in text form
-- into Haskell int values
  num <- many1 digit
  return $ IntVal (read num :: Int)

varP = do
-- x
  varNamep1 <- many1 letter
  varNamep2 <- many (digit <|> char '_')
  return $ Var (varNamep1 ++ varNamep2)

ifP = do
-- if e1 then e2 else e3 endif
-- the terms in if block are going to be from the set
-- {if, then, else, endif}
  spaces
  string "if"
  e1 <- exprP
  string "then"
  e2 <- exprP
  string "else"
  e3 <- exprP
  string "endif"
  return $ If e1 e2 e3

whileP = do
-- for parsing the while loop construct of the form
-- while e1 do e2 endwhile
  spaces
  string "while"
  e1 <- exprP
  string "do"
  e2 <- exprP
  string "endwhile"
  return $ While e1 e2

-- An expression in parens, e.g. (9-5)*2
parenP = do
-- for parsing parentheses expressions
  spaces
  char '('
  e1 <- exprP
  char ')'
  rest <- optionMaybe restP
  case rest of
    Nothing -> return e1
    Just (":=", e2) -> case e1 of
      Var varName -> return $ Assign varName e2
      _ -> error "Expected var"
    Just ("and", e2) -> return $ And e1 e2 -- for `and`
    Just ("or", e2) -> return $ Or e1 e2 -- for `or`
    Just (op, e2) -> return $ Op (transOp op) e1 e2

-- This function will be useful for defining binary operations.
-- Unlike in the previous assignment, this function returns an "Either value".
-- The right side represents a successful computaton.
-- The left side is an error message indicating a problem with the program.
-- The first case is done for you.
applyOp :: Binop -> Value -> Value -> Either ErrorMsg Value
applyOp Plus (IntVal i) (IntVal j) = Right $ IntVal $ i + j
applyOp Minus (IntVal i) (IntVal j) = Right $ IntVal $ i - j
applyOp Times (IntVal i) (IntVal j) = Right $ IntVal $ i * j
applyOp Divide (IntVal i) (IntVal 0) = Left "Can't Divide by 0"
applyOp Divide (IntVal i) (IntVal j) = Right $ IntVal $ div i j
applyOp Gt (IntVal i) (IntVal j) = Right $ BoolVal $ i > j
applyOp Ge (IntVal i) (IntVal j) = Right $ BoolVal $ i >= j
applyOp Lt (IntVal i) (IntVal j) = Right $ BoolVal $ i < j
applyOp Le (IntVal i) (IntVal j) = Right $ BoolVal $ i <= j
applyOp op _ _ = Left $ "the operation `" ++ show op ++ "` is not supported for the given operands"

-- As with the applyOp method, the semantics for this function
-- should return Either values.  Left <error msg> indicates an error,
-- whereas Right <something> indicates a successful execution.
evaluate :: Expression -> Store -> Either ErrorMsg (Value, Store)
-- using small step operational semantics
evaluate (Val v) s = Right (v, s)
evaluate e s = case evaluate' e s of
  Left msg -> Left msg
  Right (e', s') -> evaluate e' s'

evaluate':: Expression -> Store -> Either ErrorMsg (Expression, Store)
-- helper function for evaluating using small step semantics
evaluate' (Val v) s = Right (Val v, s) -- SS-VALUE

evaluate' (Var x) s = case Map.lookup x s of -- SS-VARIABLE-ACCESS-REDUCTION
  Nothing -> Left "not there in the store!"
  (Just v) -> Right (Val v,s)

evaluate' (Assign x (Val v)) s = Right (Val v, Map.insert x v s) -- SS-ASSIGNMENT-REDUCTION

evaluate' (Assign x e) s = case evaluate' e s of -- SS-ASSIGNMENT-CONTEXT
  Left msg -> Left msg
  Right (e', s') -> Right (Assign x e', s')

evaluate' (Sequence (Val _) e) s = Right (e, s) -- SS-SEQUENCE-REDUCTION

evaluate' (Sequence e1 e2) s = case evaluate' e1 s of -- SS-SEQUENCE-CONTEXT
  Left msg -> Left msg
  Right (e1', s') -> Right (Sequence e1' e2, s')

evaluate' (Op op (Val v1) (Val v2)) s = case applyOp op v1 v2 of -- SS-OPERATION-REDUCTION
  Left msg -> Left msg
  Right v -> Right (Val v, s)

-- for defining operator precedence start
-- Times and Divide have highest precedence amongst the operators
-- and hence have the ability to alter the AST to their advantage
evaluate' (Op Minus v e) s = Right (Op Plus v (Op Times (Val (IntVal (-1))) e), s)
evaluate' (Op Times (Val v) (Op lowop e1 e2)) s = Right (Op lowop (Op Times (Val v) e1) e2, s)
evaluate' (Op Divide (Val v) (Op lowop e1 e2)) s = Right (Op lowop (Op Divide (Val v) e1) e2, s)
-- for defining operator precedence end

evaluate' (Op op (Val v) e) s = case evaluate' e s of -- SS-OPERATION-CONTEXT #2
  Left msg -> Left msg
  Right (e', s') -> Right (Op op (Val v) e',s')

evaluate' (Op op e1 e2) s = case evaluate' e1 s of -- SS-OPERATION-CONTEXT #1
  Left msg -> Left msg
  Right (e1', s') -> Right (Op op e1' e2, s') 

evaluate' (If (Val (BoolVal True)) e1 _) s = Right (e1, s) -- SS-IF-TRUE-REDUCTION

evaluate' (If (Val (BoolVal False)) _ e2) s = Right (e2, s) -- SS-IF-FALSE-REDUCTION

evaluate' (If (Val (IntVal value)) e1 e2) s = Left $ "Non-boolean value '" ++ show value ++ "' used as a conditional" -- IF-IntVal not supported (IntVal is not supported in if condition)

evaluate' (If e1 e2 e3) s = case evaluate' e1 s of  -- SS-IF-CONTEXT
  Left msg -> Left msg
  Right (e1', s') -> Right (If e1' e2 e3, s')

evaluate' (While e1 e2) s = Right (If e1 (Sequence e2 (While e1 e2)) (Val (BoolVal False)), s) -- SS-WHILE-CONTEXT

evaluate' (Not e) s = Right (If e (Val $ BoolVal False) (Val $ BoolVal True), s) -- SS-NOT-CONTEXT

evaluate' (And e1 e2) s = Right (If e1 (If e2 (Val $ BoolVal True) (Val $ BoolVal False)) (Val $ BoolVal False), s) -- SS-AND-CONTEXT

evaluate' (Or e1 e2) s = Right (If e1 (Val $ BoolVal True) (If e2 (Val $ BoolVal True) (Val $ BoolVal False)), s) -- SS-OR-CONTEXT

-- Evaluates a program with an initially empty state
run :: Expression -> Either ErrorMsg (Value, Store)
run prog = evaluate prog Map.empty

showParsedExp fileName = do
  p <- parseFromFile fileP fileName
  case p of
    Left parseErr -> print parseErr
    Right exp -> print exp

runFile fileName = do
  p <- parseFromFile fileP fileName
  case p of
    Left parseErr -> print parseErr
    Right exp ->
      case run exp of
        Left msg -> print msg
        Right (v,s) -> print $ show s


