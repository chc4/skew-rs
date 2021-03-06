SKI calculus + CB + numbers + jets + reduction order + termination definition
probably gonna be called SKEIN or something? idk
```
    *(x y)                    -> *(*x *y) # ???
    *(x y z)                  -> *((*x *y) z) # ????
    *(K x y)                  -> x
    *(S x y z)                -> *(x z (y z))
    *(E A(n) f x_0 ... x_n) -> *(f *x_0 … *x_n)
    *(W A(i) (x_0 ... x_n))   -> *x_i
    *(W x y)                  -> ! # crash semantics (do we want this?)
    *(X A(n))                 -> A(n+1)
    *(Q A(n) A(m)) if n == m  -> A(0) # these can probably be synthesized but im too dumb
    *(Q x y)                  -> A(1)

    *(V K)                    -> A(0)
    *(V S)                    -> A(1)
    *(V E)                    -> A(2)
    *(V W)                    -> A(3)
    *(V X)                    -> A(4)
    *(V Q)                    -> A(5)
    *(V V)                    -> A(6)
    *(V A(n))                 -> (A(7) A(n)) # can we do any better? should the others be (A(x) A(0)) instead too?
    *x                        -> x # halt execution

    cmp x y = (Q x y)
    if c t f = (W (X (Q c A(0))) (K (t f) K))

    do we actually need jet hints?
    do we need to define "strictness" for the left side of rules? normal skew doesnt
    define reduction order very much, and passes around unevaluated thunks whereever
    it can due to data jets.
    kinda feel like i should be sprinkling *'s everywhere on the right side
```
