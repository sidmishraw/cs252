# Haskell cheatsheet and cookbook
### Author: Sidharth Mishra <sidmishraw@gmail.com>


Compiler name `ghc` : Glasgow Haskell Compilation System
Interpreter name `ghci`

When ghci loads up, it loads up the module named `Prelude` by default.
All the modules loaded by Haskell are shown before the `>` symbol on the prompt of ghci.
```
Prelude>
```

To set the prompt to something specific, use
```
:set prompt "ghci> "
```

This will set the prompt to the stuff you need.

```
Prelude> :set prompt "ghci> "
ghci> 
```

Boolean:
--------

`True`

`False`

Relational operators:
----------
`==`      Equal to

`/=`      Not Equal to

`>`       Greater than

`<`       Lesser than

`>=`      Greater than Equal to

`<=`      Lesser than Equal to



For instance, * is a function that takes two numbers and multiplies them. As you've seen, we call it by sandwiching it between them. This is what we call an `infix` function. Most functions that aren't used with numbers are `prefix` functions.

Infix and Prefix notations for `+`
```
ghci> 5 + 3
8
ghci> (+) 5 3
8
ghci>
```


### succ
The succ function takes anything that has a defined successor and returns that successor.

```
ghci> succ 'a'
'b'
ghci> succ 34
35
ghci> succ "a"

<interactive>:37:1: error:
    • No instance for (Enum [Char]) arising from a use of ‘succ’
    • In the expression: succ "a"
      In an equation for ‘it’: it = succ "a"
ghci> 
```

### min and max
The functions min and max take two things that can be put in an order (like numbers!). min returns the one that's lesser and max returns the one that's greater.


function declaration:
`let` used to declare block scoped variables(immutable)

let is needed in `ghci` and not in a script

```
Prelude> let f x = x + 1
```
```
Prelude> f 3
4
```


IO marks the function could have output (will modify the IO stream)
Hello world program
```
main :: IO ()
main = do
  putStrln "Hello World"
```

Base Types:
* Int - bounded int
* Integer - unbounded
* Float
* Double
* Bool
* Char

Lists
• Comma separated, as in Java.
• Some useful operators:
– `++` concatenation
– `:` prepend an item
– `!!` get an element at the given index
– `head` first item
– `tail` rest of the list
– `last` last item
– `init` the beginning part of the list

> Note: x is immutable, so everytime haskell is generating new lists

```
ghci> let x = [1,2,3,4,5]

ghci> head x
1

ghci> init x
[1,2,3,4]

ghci> tail x
[2,3,4,5]

ghci> last x
5

ghci> x !! 2
3

ghci> (!!) x 2
3

ghci> (++) x [55]
[1,2,3,4,5,55]

ghci> x ++ 56

<interactive>:46:1: error:
    • Non type-variable argument in the constraint: Num [a]
      (Use FlexibleContexts to permit this)
    • When checking the inferred type
        it :: forall a. (Num [a], Num a) => [a]

ghci> x ++ [56]
[1,2,3,4,5,56]

ghci> x : [0]

<interactive>:48:1: error:
    • Non type-variable argument in the constraint: Num [a]
      (Use FlexibleContexts to permit this)
    • When checking the inferred type
        it :: forall a. (Num [a], Num a) => [[a]]
ghci> x : 0

<interactive>:49:1: error:
    • Non type-variable argument in the constraint: Num [[a]]
      (Use FlexibleContexts to permit this)
    • When checking the inferred type
        it :: forall a. (Num [[a]], Num a) => [[a]]
ghci> x : 0

<interactive>:50:1: error:
    • Non type-variable argument in the constraint: Num [[a]]
      (Use FlexibleContexts to permit this)
    • When checking the inferred type
        it :: forall a. (Num [[a]], Num a) => [[a]]
ghci> x : [0]

<interactive>:51:1: error:
    • Non type-variable argument in the constraint: Num [a]
      (Use FlexibleContexts to permit this)
    • When checking the inferred type
        it :: forall a. (Num [a], Num a) => [[a]]
ghci> x : [00]

<interactive>:52:1: error:
    • Non type-variable argument in the constraint: Num [a]
      (Use FlexibleContexts to permit this)
    • When checking the inferred type
        it :: forall a. (Num [a], Num a) => [[a]]

ghci> x
[1,2,3,4,5]

ghci> 0 : x
[0,1,2,3,4,5]

ghci> 
```


Adding type signature

```
inc :: Int -> Int
inc x = x + 1
```


Haskell pattern matching, top down, ordering of the conditions matter.
```haskell
inc :: Int -> Int

-- Condition 1
inc x | x < 0 =
    error "no negative nums"

-- Condition 2inc :: Int -> Int
inc x | x < 0 =
error "no negative nums"
inc x = x + 1
inc x = x + 1
```

haskell functions are curried, they take only 1 arg and return a function
(==) :: Eq a => a -> a -> Bool