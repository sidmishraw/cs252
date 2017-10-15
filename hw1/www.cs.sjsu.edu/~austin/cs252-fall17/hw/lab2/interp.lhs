This File is _literate Haskell_.
That means that (in some sense) code and comments are reversed.
By default, everything that I type is actually a comment.

To write code, I preface it with a 'greater than' symbol.
Here we define the expressions in our language:

> data Exp = ETrue
>          | EFalse
>          | Eif Exp Exp Exp
>          | ENum Int
>          | Succ Exp
>          | Pred Exp
>          | EZero
>   deriving Show

When an expression is evaluated, it returns a value.

> data Val = VTrue
>          | VFalse
>          | VNum Int
>          | VZero
>   deriving Show

The evaluate function takes an expression and returns a value
The VTrue case has been done for you.
You must complete the other cases.

> evaluate :: Exp -> Val
> evaluate ETrue = VTrue
> evaluate EFalse = VFalse
> evaluate (Eif cond expTrue expFalse) = case (evaluate cond) of VTrue -> (evaluate expTrue)
>                                                                VFalse -> (evaluate expFalse)
>                                                                (VNum n) -> error "Not supported!"
> evaluate (ENum n) = VNum n
> evaluate (EZero) = VZero

> evaluate (Succ exp) =
    case (evaluate exp) of (VNum i) -> VNum (i + 1)
                            _ -> error "Not supported"

> evaluate (Pred exp) =
    case (evaluate exp) of (VNum i) -> VNum (i - 1)
                            _ -> error "Not supported"

> evaluate _ = error "OOPS! Something doesn't seem right!"

succ utility function

> evaluate (Succ (ENum n)) = succ' (ENum n)
> evaluate (Pred (ENum n)) = pred' (ENum n)

> succ' :: Exp -> Val
> succ' ETrue = error "Not valid value"
> succ' EFalse = error "Not valid value"
> succ' (ENum n) = (VNum (n + 1))

pred utility function

> pred' :: Exp -> Val
> pred' ETrue = error "Not valid value"
> pred' EFalse = error "Not valid value"
> pred' (ENum n) = (VNum (n - 1))

zero utility function

> zero :: Val
> zero = VZero

And here we have a couple of programs to test.
prog1 should evaluate to VTrue and prog2 should evaluate to VFalse

> prog1 = Eif ETrue ETrue EFalse
> prog2 = Eif (Eif ETrue EFalse ETrue) ETrue (Eif ETrue EFalse ETrue)

> prog3 = Succ (ENum 3)
> prog4 = Pred (ENum 10)
> prog5 = EZero

The "stucky" condition

> prog6 = Eif (Succ (ENum 6)) ETrue EFalse
> prog7 = Eif (Pred (ENum 8)) ((Succ (ENum 6))) EFalse
> prog8 = Eif (prog1) ((Succ (ENum 6))) EFalse

The following lines evaluate the test expressions and display the results.
Note the type of main.  'IO ()' indicates that the function performs IO and returns nothing.
The word 'do' begins a block of code, were you can effectively do sequential statements.
(This is a crude generalization, but we'll talk more about what is going on in this function
when we deal with the great and terrible subject of _monads_.)

> main :: IO ()
> main = do
>   putStrLn $ "Evaluating '" ++ (show prog1) ++ "' results in " ++ (show $ evaluate prog1)
>   putStrLn $ "Evaluating '" ++ (show prog2) ++ "' results in " ++ (show $ evaluate prog2)
>
>   putStrLn $ "Evaluating '" ++ (show prog3) ++ "' results in " ++ (show $ evaluate prog3)
>   putStrLn $ "Evaluating '" ++ (show prog4) ++ "' results in " ++ (show $ evaluate prog4)
>
>   putStrLn $ "Evaluating '" ++ (show prog5) ++ "' results in " ++ (show $ evaluate prog5)

The code below fails because of condition should only evaluate to VTrue or VFalse in if conditions.
putStrLn $ "Evaluating '" ++ (show prog6) ++ "' results in " ++ (show $ evaluate prog6)

The code below fails because of condition should only evaluate to VTrue or VFalse in if conditions.
putStrLn $ "Evaluating '" ++ (show prog7) ++ "' results in " ++ (show $ evaluate prog7)

>   putStrLn $ "Evaluating '" ++ (show prog8) ++ "' results in " ++ (show $ evaluate prog8)


Once you have the evaluate function working
you add in support the expressions 'succ e', 'pred e', and 'zero'.
With this change, it is possible for evaluate to get 'stuck',
e.g. pred true.
For a first pass, simply use the error function in these cases.



