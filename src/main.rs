#![allow(dead_code, unused_parens)]
use std::rc::Rc;
use std::fmt;
use std::fmt::Debug;

extern crate ramp;
extern crate dyn_clone;
use ramp::Int;
use dyn_clone::DynClone;

use Skew::*;
use Twist::*;

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
#[derive(Clone)]
struct Jet(Box<dyn Jetted>);
impl PartialEq for Jet {
    fn eq(&self, other: &Self) -> bool {
        //Jet::eq(self.as_ref(), other)
        <Box<dyn Jetted> as PartialEq>::eq(&self.0, &other.0)
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
struct Add;
impl Jetted for Add {
    fn arity(&self) -> Int {
        2.into()
    }
    fn call(&self, args: &[Twist]) -> Option<Twist> {
        if let [N(A(n)), N(A(m))] = args {
            return Some(N(A(n + m)));
        } else {
            return None;
        }
    }
    fn name(&self) -> String {
        "Add".into()
    }
}

#[derive(Clone, PartialEq)]
enum Twist {
    Expr(Rc<Vec<Twist>>),
    N(Skew),
    J(Jet)
}

fn cons(mut exprs: Vec<Twist>) -> Twist {
    match &mut exprs.as_mut_slice() {
        // flatten ((x y) z) -> (x y z)
        [Expr(ref mut head), tail @ ..] => {
            //println!("flattening {:?} {:?}", head, tail);
            let l = Rc::make_mut(head);
            l.extend_from_slice(tail);
            //println!("-> {:?}", l);
            return Expr(Rc::new(l.to_vec()))
        },
        _ => Expr(Rc::new(exprs))
    }
}

// skew is defined as left-associative: (x y z) is grouped as ((x y) z)
// this is a problem for pattern matching because you have to chase for the combinator
// tag multiple times, since it's not always at the top level.
// we represent ((x y) z) as vec![x,y,z] and (x (y z)) as vec![x,vec![y,z]] instead.

// how to make this faster:
// have exprs been a stack, and apply reductions at the *tail* so that
// we can do [... z y x S] -> [... (z y) (z x)] reductions in-place with make_mut
//
// we should also be using a stack vm for this, so that S can be a state machine of
// S_0, S_1, S_2 for the (x z) and (y z) reduction, and E argument reduction
// without blowing the call stack.
// currently "evaluate arguments" for E means you have to call reduce() in a loop
// until it returns None, which means its recursive and will build deep call stacks.

impl Twist {
    fn atom(n: usize) -> Self {
        N(A(Int::from(n)))
    }
    fn reduce(&self) -> Option<Self> {
        if let Expr(exprs) = self {
            let o: Option<Self> = match &exprs.as_slice() {
                [N(K), x, _y @ ..] => Some(x.clone()),
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
                [N(S), x, y, z] => {
                    let mut s = vec![];
                    let mut xz = vec![x.clone(), z.clone()];
                    s.append(&mut xz);
                    let yz = vec![y.clone(), z.clone()];
                    s.push(cons(yz));
                    //s.extend_from_slice(w);
                    Some(cons(s))
                },
                // I think this is wrong, since something like ((curry |=([a=@ b=@] a+b) 1) 2)
                // might need a `rest`?
                [N(E), N(A(n)), _t, f, x @ ..] if Int::from(x.len()) == *n => {
                    let mut arity = n;
                    let mut jetted = None;
                    println!("jet arity {}", arity);
                    let mut eval_x: Vec<Twist> = x.clone().iter().map(|i| {
                        let mut curr = i.clone();
                        loop {
                            println!("curr ");
                            if let Some(r) = curr.reduce() {
                                curr = r;
                            } else {
                                break curr;
                            }
                        }}).collect();
                    if let J(jet) = f {
                        if jet.0.arity() == *arity {
                            jetted = jet.0.call(&eval_x);
                        } else {
                            println!("jet arity doesnt match");
                        }
                    }
                    // we didn't have a Jet as a function, but still have a hint
                    // search for it in the jet registry?
                    if jetted.is_none() {
                        let mut unjetted = vec![f.clone()];
                        unjetted.append(&mut eval_x);
                        return Some(cons(unjetted));
                    } else {
                        return jetted;
                    }
                },
                [N(A(n)), f, x @ ..] => {
                    let mut r = vec![f.clone(), N(A(n+1))];
                    r.extend_from_slice(x.clone());
                    Some(cons(r))
                },
                //[N(W), a, s, k, e, w, op @ _] => {
                //    match op {
                //        xy @ Expr(_) => {
                //            let mut e: Vec<Twist> = vec![a.clone()];
                //            e.push(xy.clone());
                //            Some(cons(e))
                //        },

                //        N(S) => Some(s.clone()),
                //        N(K) => Some(k.clone()),
                //        N(E) => Some(e.clone()),
                //        N(W) => Some(w.clone()),
                //        J(j) => unimplemented!(),
                //    }
                //},

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
    let mut t = cons(vec![N(E), Twist::atom(2), N(K), cons(vec![N(S), N(K)]), cons(vec![N(K), N(K), cons(vec![N(K), N(K)])]), cons(vec![N(S), N(K), N(K), N(K)])]);
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
        let t = cons(vec![N(K), N(K), N(K)]).reduce().unwrap();
        assert_eq!(t, N(K));
    }
    #[test]
    fn test_k_applies_first() {
        let t = cons(vec![N(K), N(K), cons(vec![N(S), N(K), N(K)])]).reduce().unwrap();
        assert_eq!(t, N(K));
    }
    #[test]
    fn test_s() {
        let t1 = cons(vec![N(S), N(K), cons(vec![N(S), N(K)]), cons(vec![N(S), N(K), N(K)])]).reduce().unwrap();
        assert_eq!(t1, cons(vec![N(K), cons(vec![N(S), N(K), N(K)]), cons(vec![N(S), N(K), cons(vec![N(S), N(K), N(K)])])]));
        let t2 = t1.reduce().unwrap();
        assert_eq!(t2, cons(vec![N(S), N(K), N(K)]));
    }

    #[test]
    fn test_s2() {
        let t1 = cons(vec![N(S), N(K), N(K), N(K)]).reduce().unwrap();
        assert_eq!(t1, cons(vec![N(K), N(K), cons(vec![N(K), N(K)])]));
    }
    #[test]
    pub fn test_e() {
        crate::main()
    }

    #[test]
    fn test_add() {
        use crate::Add;
        let t1 = cons(vec![N(E), Twist::atom(2), N(K), J(Jet(Box::new(Add))), Twist::atom(1), Twist::atom(2)]).reduce().unwrap();
        assert_eq!(t1, Twist::atom(3));
    }

    #[test]
    fn test_add_argument_eval() {
        use crate::Add;
        fn defer(t: Twist) -> Twist {
            cons(vec![N(K), t, N(K)])
        }
        let lazy_1 = defer(defer(Twist::atom(1)));
        let t1 = cons(vec![N(E), Twist::atom(2), N(K), J(Jet(Box::new(Add))), lazy_1, Twist::atom(2)]).reduce().unwrap();
        assert_eq!(t1, Twist::atom(3));

    }

    #[test]
    fn test_increment() {
        let t1 = cons(vec![Twist::atom(1), N(K), N(K)]).reduce().unwrap().reduce().unwrap();
        assert_eq!(t1, Twist::atom(2));
    }
}
