#![allow(dead_code, unused_parens)]
use std::rc::Rc;
use std::fmt;
use std::fmt::Debug;
use std::mem;

extern crate ramp;
extern crate dyn_clone;
use ramp::Int;
use dyn_clone::DynClone;

use Skew::*;
use Twist::*;
mod jets;

#[derive(Clone, PartialEq, Debug)]
enum Skew {
    S,
    K,
    E,
    W,
    A(Int),
    // Lazy(Box<dyn Stand>) that we decompose into a concrete Skew?
    // how do we do this efficiently in the Skew::reduce function -
    // since we're always matching the header we just expand a Lazy
    // if we have one, and then expand them if we need conditional matching
    // for e.g. the Jet arity, but not hint?
    // (this is so we can have a jet return a native HashMap that can be used with
    // skew and a native get/set jet on the HashMap, with lazy reduction down to
    // actual skew if needed)
}

trait Jetted: DynClone {
    fn arity(&self) -> Int;
    fn call(&self, args: &[Twist]) -> Option<Twist>;
    fn name(&self) -> String;
}
dyn_clone::clone_trait_object!(Jetted);

// workaround for #39128
// this should probably be a (Rc<dyn Jetter>, TypeId) so that
// jets can match on it for arguments.
#[derive(Clone)]
struct Jet(Rc<dyn Jetted>);
impl PartialEq for Jet {
    fn eq(&self, other: &Self) -> bool {
        //Jet::eq(self.as_ref(), other)
        <Rc<dyn Jetted> as PartialEq>::eq(&self.0, &other.0)
    }
}

impl PartialEq for dyn Jetted {
    fn eq(&self, other: &Self) -> bool {
        // XXX: add Any downcasting impl?
        // i dont think we can actually use the derive partialeq, since
        // we *do* want jet == skew to be true if jet.normal() == skew
        false
    }
}

#[derive(Clone, PartialEq)]
enum Twist {
    Expr(Rc<Vec<Twist>>),
    N(Skew),
    J(Jet)
}

#[inline]
fn cons(mut exprs: Vec<Twist>) -> Twist {
    match &mut exprs.as_mut_slice() {
        // flatten ((x y) z) -> (x y z)
        [Expr(ref mut head), tail @ ..] if tail.len() > 0 => {
            //println!("flattening {:?} {:?}", head, tail);
            let l = Rc::make_mut(head);
            l.extend_from_slice(tail);
            //println!("-> {:?}", l);
            return Expr(Rc::new(l.to_vec()))
        },
        _ => Expr(Rc::new(exprs))
    }
}

#[macro_export]
macro_rules! skew {
    (S) => { N(S) };
    (K) => { N(K) };
    (E) => { N(E) };
    (W) => { N(W) };
    ( ($( $x:tt ),+)) => {
        {
            let mut temp_vec: Vec<Twist> = Vec::new();
            $(
                temp_vec.push(skew!($x));
            )+
            cons(temp_vec)
        }
    };
    ({A $x:expr}) => { Twist::atom($x) };
    ({$x:expr}) => { $x.clone() };
}

// skew is defined as left-associative: (x y z) is grouped as ((x y) z)
// this is a problem for pattern matching because you have to chase for the combinator
// tag multiple times, since it's not always at the top level.
// we represent ((x y) z) as vec![x,y,z] and (x (y z)) as vec![x,vec![y,z]] instead.

// how to make this faster:
// have exprs been a stack, and apply reductions at the *tail* so that
// we can do [... z y x S] -> [... (z y) (z x)] reductions in-place with make_mut
//
// we should also be using a stack vm for this, for E argument reduction
// without blowing the call stack.
// currently "evaluate arguments" for E means you have to call reduce() in a loop
// until it returns None, which means its recursive and will build deep call stacks.
//
// problem: "evaluating" is defined as running until fixpoint.
// this means that "data structures" are evaluated - (get my_map 'key) would need
// my_map in a way that doesn't reduce.
// we could just have it as `E A(1) K (map data)` with no arguments?
// then all get's impl has to match on `E A(1) _ J(Jet(map))` instead, but we keep
// semantics.
// can we just use (K map) instead? does that ruin codegen? idk how SKI compilers
// work.

impl Twist {
    /// Generate a new atom from a number
    fn atom(n: usize) -> Self {
        N(A(Int::from(n)))
    }
    /// Reduce until there's nothing left
    fn boil(&mut self) {
        let mut curr = N(K);
        mem::swap(&mut curr, self);
        loop {
            if let Some(next) = curr.reduce() {
                curr = next;
            } else {
                mem::swap(self, &mut curr);
                break;
            }
        }
    }
    /// Reduce a Twist one step
    #[inline]
    fn reduce(&self) -> Option<Self> {
        if let Expr(exprs) = self {
            let o: Option<Self> = match &exprs.as_slice() {
                [N(K), x, _y, z @ ..] => {
                    if z.len() > 0 {
                        let mut v = vec![x.clone()];
                        v.extend_from_slice(z);
                        Some(cons(v))
                    } else {
                        Some(x.clone())
                    }
                },
                [N(S), x, y, z, w @ ..] => {
                    let mut s = vec![];
                    let mut xz = vec![x.clone(), z.clone()];
                    s.append(&mut xz);
                    let yz = vec![y.clone(), z.clone()];
                    s.push(cons(yz));
                    if(w.len() != 0){
                        s.extend_from_slice(w);
                    }
                    Some(cons(s))
                },
                [N(E), N(A(n)), _t, f, x @ ..] if Int::from(x.len()) >= *n => {
                    let mut arity = n;
                    let mut jetted = None;
                    println!("jet arity {}", arity);
                    let s: usize = n.into();
                    let new_x = &mut x.to_owned();
                    // this leads to unnecessary allocations i think
                    for item in new_x[..s].iter_mut() {
                        item.boil();
                    }
                    println!("after reduction {:?}", new_x);
                    if let J(jet) = f {
                        if jet.0.arity() == *arity {
                            jetted = jet.0.call(&mut new_x[..s])
                        } else {
                            println!("jet arity doesnt match");
                        }
                    }
                    if let Some(jet_val) = jetted {
                        if(new_x.len() > s){
                            println!("function call with too many arguments");
                            let mut many = vec![jet_val];
                            many.extend_from_slice(&mut new_x[s..]);
                            return Some(cons(many));
                        } else {
                            return Some(jet_val);
                        }
                    } else {
                        // we didn't have a Jet as a function, but still have a hint
                        // search for it in the jet registry?
                        let mut unjetted = vec![f.clone()];
                        unjetted.extend_from_slice(new_x);
                        return Some(cons(unjetted));
                    }
                },
                [N(A(n)), f, x @ ..] => {
                    let mut r = vec![f.clone(), N(A(n+1))];
                    r.extend_from_slice(x.clone());
                    Some(cons(r))
                },
                // does this work? do i need to eval for e first?
                // do i add a `if e.len() >= n`?
                [N(W), N(A(n)), Expr(e), x @ ..] => {
                    let s: usize = n.into();
                    if x.len() > 0 {
                        let mut r = vec![e[s].clone()];
                        r.extend_from_slice(x.clone());
                        Some(cons(r))
                    } else {
                        Some(e[s].clone())
                    }
                },
                //// these rules force reduction of E arguments first
                //// it also ruins our cache coherency though :(
                //[x @ .., y @ _] if cons(x.into()).reduce().is_some() => {
                //    let mut xy: Vec<Twist> = vec![cons(x.into()).reduce().unwrap()];
                //    xy.push(y.clone());
                //    Some(cons(xy))
                //},
                //[x @ .., y @ _] if y.reduce().is_some() => {
                //    let mut xy: Vec<Twist> = x.into();
                //    xy.push(y.reduce().unwrap());
                //    Some(cons(xy))
                //},

                // probably need a J(jet) => jet.deoptimize() rule here
                _ => None,
            };
            //if let Some(x) = o.clone() {
            //    println!("reducing {:?}", self);
            //    println!("-> {:?}", x);
            //}
            o
        } else {
            None
        }
    }
}

impl fmt::Debug for Twist {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            N(c) => c.fmt(f),
            Expr(expr) => {
                write!(f, "(")?;
                let mut first = true;
                for e in expr.iter() {
                    if first {
                        first = false;
                    } else {
                        write!(f, " ")?;
                    }
                    std::fmt::Debug::fmt(e, f)?;
                }
                write!(f, ")")
            },
            J(j) => {
                write!(f, "{}", j.0.name())
            }
        }
    }
}

fn main() {
    let mut t = skew![(E, {Twist::atom(2)}, K, (S, K), (K, K, (K, K)), (S, K, K, K))];
    for i in 0..3 {
        println!("step {} {:?}", i, t);
        t = t.reduce().unwrap()
    }
    println!("final {:?}", t);
    assert_eq!(t, N(K));
}

mod test {
    use crate::Skew::*;
    use crate::Twist::*;
    use crate::Twist;
    use crate::Jet;
    use crate::cons;
    #[test]
    fn test_k() {
        let t = skew![(K, K, K)].reduce().unwrap();
        assert_eq!(t, N(K));
    }
    #[test]
    fn test_k_applies_first() {
        let t = skew![(K, K, (S, K, K))].reduce().unwrap();
        assert_eq!(t, N(K));
    }
    #[test]
    fn test_s() {
        let t1 = skew![(S, K, (S, K), (S, K, K))].reduce().unwrap();
        assert_eq!(t1, skew![(K, (S, K, K), (S, K, (S, K, K)))]);
        let t2 = t1.reduce().unwrap();
        assert_eq!(t2, skew![(S, K, K)]);
    }

    #[test]
    fn test_s2() {
        let t1 = skew![(S, K, K, K)].reduce().unwrap();
        assert_eq!(t1, skew![(K, K, (K, K))]);
    }
    #[test]
    pub fn test_e() {
        crate::main()
    }
    #[test]
    fn test_increment() {
        let t1 = cons(vec![Twist::atom(1), N(K), N(K)]).reduce().unwrap().reduce().unwrap();
        assert_eq!(t1, Twist::atom(2));
    }

    // TODO: add I and Swap jets, fast-path them in reduce() pattern matching?
    // maybe add C and B combiniator jets too
    #[test]
    fn test_i() {
        let i = skew![(S,K,K)];
        let mut apply_i = cons(vec![i.clone(), Twist::atom(1)]);
        apply_i.boil();
        assert_eq!(apply_i, Twist::atom(1));
    }

    #[test]
    fn test_swap() {
        let i = skew![(S,K,K)];
        let swap = skew![(S, (K, (S, {i})), K)];
        let mut apply_swap = skew![({swap}, {A 1}, K, K)];
        for i in 0..6 {
            println!("before reduce {:?}", apply_swap);
            apply_swap = apply_swap.reduce().unwrap();
            println!("reduce swap {:?}", apply_swap);
        }
        assert_eq!(apply_swap, cons(vec![N(K), Twist::atom(1), N(K)]));
    }

    #[test]
    fn test_pick() {
        let t1 = skew![(W, {A 1}, ({A 1}, {A 1}, {A 2}))];
        assert_eq!(t1.reduce().unwrap(), Twist::atom(1));
    }
}
