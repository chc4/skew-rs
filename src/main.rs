#![allow(dead_code, unused_parens)]
use std::rc::Rc;
use std::fmt;
use std::fmt::Debug;

#[derive(Clone, PartialEq, Debug)]
enum Skew {
    S,
    K,
    E,
    W
}
#[derive(Clone, PartialEq)]
enum Twist {
    Expr(Rc<Vec<Twist>>),
    N(Skew),
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

use Skew::*;
use Twist::*;

impl Twist {
    fn reduce(&self) -> Option<Self> {
        if let Expr(exprs) = self {
            let o: Option<Self> = match &exprs.as_slice() {
                // this K rule is slightly incorrect:
                // we're flattening combinators trees and matching left-to-right
                // to follow rule 2&3, but that means we aren't applying K right-to-left!
                // we probably need a rule looking for any Expr(N(K), _, _) in the slice
                // as well.
                [N(K), x, _y, ..] => Some(x.clone()),
                [N(S), x, y, z @ ..] => {
                    let mut s = vec![];
                    let mut xz = vec![x.clone()];
                    xz.extend_from_slice(z.clone());
                    s.push(cons(xz));
                    let mut yz = vec![y.clone()];
                    yz.extend_from_slice(z.clone());
                    s.push(cons(yz));
                    Some(cons(s))
                },
                e @ [N(E), ..] => {
                    let mut arity = 0;
                    for expr in e.iter() {
                        if let N(E) = expr {
                            arity = arity + 1;
                        } else {
                            break;
                        }
                    }
                    println!("jet arity {}", arity);
                    let o: Vec<Twist> = e.iter().skip(arity + 1).map(|x| x.clone()).collect();
                    // just ignore arity for now?
                    if(o.len() != arity + 1) {
                        return None
                    }
                    //assert_eq!(o.len(), arity);
                    Some(cons(o))
                },
                [N(W), a, s, k , e , w, op @ _] => {
                    match op {
                        xy @ Expr(_) => {
                            let mut e: Vec<Twist> = vec![a.clone()];
                            e.push(xy.clone());
                            Some(cons(e))
                        },
                        N(S) => Some(s.clone()),
                        N(K) => Some(k.clone()),
                        N(E) => Some(e.clone()),
                        N(W) => Some(w.clone()),
                    }
                },
                // I don't think we actually need these rules?
                [x @ .., y @ _] if cons(x.into()).reduce().is_some() => {
                    let mut xy: Vec<Twist> = vec![cons(x.into()).reduce().unwrap()];
                    xy.push(y.clone());
                    Some(cons(xy))
                },
                [x @ .., y @ _] if y.reduce().is_some() => {
                    let mut xy: Vec<Twist> = x.into();
                    xy.push(y.reduce().unwrap());
                    Some(cons(xy))
                },
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
            }
        }
    }
}

fn main() {
    test::test_e();
}

mod test {
    use crate::Skew::*;
    use crate::Twist::*;
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
    pub fn test_e() {
        let mut t = cons(vec![N(E), N(E), N(K), cons(vec![N(S), N(K)]), cons(vec![N(K), N(K), cons(vec![N(K), N(K)])]), cons(vec![N(S), N(K), N(K), N(K)])]);
        for i in 0..5 {
            println!("step {} {:?}", i, t);
            t = t.reduce().unwrap()
        }
        assert_eq!(t, N(K));
    }
}
