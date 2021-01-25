SKI calculus + CB + numbers + jets + reduction order + termination definition
probably gonna be called SKEIN or something? idk
```
    *((x y) z)                -> *(x y z)
    *(K x y)                  -> x
    *(S x y z)                -> (x z (y z))
    *(E A(n) t f x_0 ... x_n) -> (f *x_0 … *x_n)
    *(W A(i) (x_0 ... x_n))   -> x_i # i think this is enough to build `if` and `mock`
    *(A(n) f x)               -> (f A(n+1) x) # this is probably dumb
    *x                        -> x # halt execution

    reduction is defined as reaching a fixpoint? should we have explicit *'s on the
    right hand rules and not use fixpoint, so that we can just do
    (E A(1) t f (K x K)) for quoting x?
```
