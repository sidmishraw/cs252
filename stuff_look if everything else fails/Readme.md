# CS 252 - Notes
## Advanced Programming Language Principles
### Instructor: Dr. Thomas Austin, SJSU
### Author: Sidharth Mishra



## Haskell intro from [Learn you a Haskell for greater good](http://learnyouahaskell.com/introduction)

Terms

* Haskell is a purely functional programming language.

* In purely functional programming you don't tell the computer what to do as such but rather you tell it what stuff is.

* If a function is called twice with the same parameters, it's guaranteed to return the same result. That's called `referential transparency` and not only does it allow the compiler to reason about the program's behavior, but it also allows you to easily deduce (and even prove) that a function is correct and then build more complex functions by gluing simple functions together.

  Basically this means that treat each function as a black box, given the same set of inputs, it must always produce the same set of outputs.


* Haskell is lazy, doesn't evaluate functions unless needed.

My Haskell cheatsheet and cookbook is located here: [haskell_cb.md](./haskell_cb.md)

Haskell is statically typed but Scheme is not, although they are both purely functional languages.

All functions of Haskell are fully `curried` basically all functions are single paramters and return a function.