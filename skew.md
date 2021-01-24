SKEWA
```
    *((x y) z)            -> *(x y z)
    *(K x y)              -> x
#    *(x y)               -> (*x y)
#    *(x y)               -> (x *y)
    *(S x y z)           -> (x z (y z))
    *(E A(n) t f x_1 … x_n)   -> (f x_1 … x_n)
    *(W A(n) x_1 ... x_n) -> x_n # i think this is enough to build `if` and `mock`
    *(A(n) f x)             -> (f A(n+1) x) # this is probably dumb
    *x                      -> ! # can this be `x` to define termination?
```
