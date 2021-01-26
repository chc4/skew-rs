SKI calculus + CB + numbers + jets + reduction order + termination definition
probably gonna be called SKEIN or something? idk
```
    *((x y) z)                -> *(x y z)
    *(K x y)                  -> x
    *(S x y z)                -> (x z (y z))
    *(E A(n) t f x_0 ... x_n) -> (f *x_0 … *x_n)
    *(W A(i) (x_0 ... x_n))   -> x_i # i think this is enough to build `if` and `mock`
    *(X A(n) f)               -> (f A(n+1)) # this is probably dumb
    *x                        -> x # halt execution

    there's something dumb where this is different from SKI because SKI is defined
    as (K x) returns a function that only returns x. we define only reducing
    when we have both x and y. i think this is exposed to the interpreter thanks to
    (W A(0) *(K x)) being either K or (K x) depending on how you implement it.
    ??? think about this ??? is this a problem ??? is it actually observably ???

    skew by design doesn't have structural equality - this adds atom equality
    do we also want structural equality? how does pattern matching work otherwise?
    you can still do tagged unions - do we also need structural pattern matching
    for anything? (len list) can use repeated W until it doesnt reduce, same with
    (is-some none|(some 1)).
    it makes it harder for bailout for e.g. noun surgery if you can't say "panic
    if we get a malformed map". technically if you virtualize with a ++mock-like
    function then you can define crash semantics like that - we could also add
    (S -> 0), (K -> 1) rules in W. like *(W S (x_0 ... x_n)) -> x_0, so that it
    doesn't require virtualization
    we need virtualization for crashes in general tho
    i have no clue how theyre supposed to be defined

    reduction is defined as reaching a fixpoint? should we have explicit *'s on the
    right hand rules and not use fixpoint, so that we can just do
    (E A(1) t f (K x K)) for quoting x?
```
