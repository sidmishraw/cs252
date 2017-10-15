Let the language be defined as:

// expression
e ::= true | false | n
    | if e then e else e
    | succ e
    | pred e
    | secret e

`e` is an expression and it needs to be evaluated to give a result

e => <value> is read as `e evaluates to <value>`

// values
v ::= true | false | n

v => v


// H and L are labels for High security and Low security
k, l ::= H | L

R ::= v*


// succ function
    e => n   n'=n+1
------------------
succ e => n'

// pred function
     e => n   n' = n - 1
------------------
pred e => n'

// ^ is just used for superscript here, not exponentation
e => v^k

// which can be written as
e => R


// secret function
e => R
------------                  [Sk-Sec]
secret e => v^H


so,
by default, `v => v^L` since it is public
only the application of the secret funciton will produce a v^H

v => v^L                       [Sk-Val]

succ 7 => 8^L

succ (secret 7) => 8^H


secret evaluates to secret and public to public


so the functions get updated to:


// succ function -- secret keeper version
    e => n^k   n'^k = n^k + 1
------------------                      [Sk-succ]
succ e => n'^k



// pred function -- secret keeper version
     e => n^k   n'^k = n^k - 1
------------------                          [Sk-pred]
pred e => n'^k



# case 1
  e1 => true^H    e2=> v^H
------------------------------
if e1 then e2 else e3  => v^H

# case 2
  e1 => true^L    e2=> v^L
------------------------------
if e1 then e2 else e3  => v^L


// generalizing

e1 => true^k    e2 => R
------------------------------                [Sk-if true]
if e1 then e2 else e3  => R



e1 => false^k    e3 => R
------------------------------                [Sk-if false]
if e1 then e2 else e3 => R


But this genral form can be attacked by doing a


if secret e'
then true
else false

if secret e' evaluates to True^H then result is true else false
So we are kinda able to dedude the value of the (secret e') in turn e'

So we define the Join operator (join) as:

H join H = H
H join L = H
L join H = H
L join L = L


Using the join operator to defend the attack:



e1 => true^k    e2 => v^l
------------------------------                [Sk-if true]
if e1 then e2 else e3  => v^(k join l)



e1 => false^k    e3 => v^l
------------------------------                [Sk-if false]
if e1 then e2 else e3 => v^(k join l)


Note: the "---" lines represent a condition, the top part is the condition needed for the
bottom part to hold


H - equivalent (~H) the same except for the secret bits
--------------------------------------------------------------
e1 (~H) e2
----------            if v1 = v2
v1 (~H) v2

secret e1 (~H) secret e2


succ e1 (~H) succ e2    {if e1 (~H) e2}

pred e1 (~H) pred e2    {if e1 (~H) e2}

if e1 then e2 else e3 (~H) if e4 then e5 else e6

    {
      if  e1 (~H) e4
          e2 (~H) e5
          e3 (~H) e6
    }


All items that are not under secret should hold only if their values are equal


Termination-Insensitive Noninterference theorem:
------------------------------------------------

lets consider the scenario:


e1 = if (secret true) then 1 else 0
e2 = if (secrer 10) then 1 else 0


e1 => 1

but e2 fails to evaluate since secret 10 => 10^H and we don't have rules for this kind of condition

```If e1 (~H) e2 and e1 => R1 and e2 => R2 then R1 (~H) R2```


So,


R1 (~H) R2
------------
v1^L (~H) v2^L      if v1 = v2

v1^H (~H) v2^H


### Proof: The proof is by induction on a derivation of `e1 => R1` and case analysis on the last rule used in that derivation.
```If e1 (~H) e2 and e1 => R1 and e2 => R2 then R1 (~H) R2```

## case [Sk-Val]: Then e1 = v1 and e2 = v2
we know that

    e1 => v1 ^ L
    e2 => v1 ^ L
    e1 = e2         because e1 (~H) e2
    
    therefore,
      v1 = v2
      R1 = R2

## Case [Sk-Sec] then e1 = secret e1'

