# Twist

This is a Rust implemention of [SKEW](https://github.com/urbit/urbit/blob/ac21a5d7c58cd95dfa2e1329292f8a4d819b3feb/pkg/hs/urbit-skew/skew.md), a purely functional combinator language.

v1.1: Add A(Int), so that it doesn't require peano arithmatic for numbers.
    - changes `E^n K ...` jet hints to `E N(n) K ...` arity hints
    - doesn't define K as being folded before application on the head
