/// Lambda calculus -> SKEW rewritter
use std::rc::Rc;
use ramp::Int;
use Twist::*;
use Twist;
use Skew::*;
use Skew;
use Jetted;
use Jet;
use cons;
use skew;

#[derive(Clone, PartialEq)]
struct C;
impl Jetted for C {
    fn arity(&self) -> Int {
        3.into()
    }
    fn call(&self, args: &[Twist]) -> Option<Twist> {
        if let [f, g, x] = args {
            return Some(skew![({f}, {x}, {g})]);
        } else {
            return None;
        }
    }
    fn name(&self) -> String {
        "C".into()
    }
}

#[derive(Clone, PartialEq)]
struct B;
impl Jetted for B {
    fn arity(&self) -> Int {
        3.into()
    }
    fn call(&self, args: &[Twist]) -> Option<Twist> {
        if let [f, g, x] = args {
            return Some(skew![({f}, ({g}, {x}))]);
        } else {
            return None;
        }
    }
    fn name(&self) -> String {
        "B".into()
    }
}

// TARGET FUNCTION
// fact = Y(\f.\n.if(is0(n) 1 mul(n f(dec(n)))))

#[derive(Debug, Clone)]
pub enum LTerm<'a> {
    Twist(Twist),
    Var(&'a str),
}
#[derive(Debug, Clone)]
pub enum Lambda<'a> {
    Func(&'a str, Box<Lambda<'a>>),
    App(Box<Lambda<'a>>, Box<Lambda<'a>>),
    Term(LTerm<'a>),
}

impl<'a> Lambda<'a> {
    pub fn free(&self, var: &'a str) -> bool {
        match self {
            Lambda::Term(LTerm::Var(x)) => var == *x,
            Lambda::App(x, y) => { x.free(var) || y.free(var) },
            Lambda::Func(x, bod) => {
                if( *x == var) {
                    return false
                }
                bod.free(var)
            },
            _ => false
        }
    }

    pub fn transform(&mut self) -> Lambda<'a> {
        println!("reducing {:?}", self);
        match self.clone() {
            Lambda::App(mut m, mut n) => {
                println!("2");
                Lambda::App(
                    box m.transform(),
                    box n.transform()
                )
            },
            Lambda::Func(x, mut body) => match *body.clone() {
                mut y @ _ if !body.free(x) => {
                    println!("3");
                    Lambda::App(box Lambda::Term(LTerm::Twist(Twist::N(Skew::K))), box y.transform())
                },
                Lambda::Term(LTerm::Var(x2)) if x == x2 => {
                    println!("4");
                    Lambda::Term(LTerm::Twist(skew![(S, K, K)]))
                },
                Lambda::Func(y, body2) if body2.free(x) => {
                    println!("5");
                    Lambda::Func(x, box body.transform()).transform()
                }
                Lambda::App(mut e, box Lambda::Term(LTerm::Var(y))) if !e.free(x) => {
                    println!("fancy n");
                    e.transform()
                },
                Lambda::App(mut m, mut n) => {
                    let c = cons(vec![N(E), N(A(Rc::new(C.arity()))), N(K), J(Jet(Rc::new(C)))]);
                    let b = cons(vec![N(E), N(A(Rc::new(B.arity()))), N(K), J(Jet(Rc::new(B)))]);
                    match (m.free(x), n.free(x)) {
                        (true, true) => {
                            println!("6");
                            Lambda::App(
                                box Lambda::App(
                                    box Lambda::Term(LTerm::Twist(Twist::N(Skew::S))),
                                    box Lambda::Func(x, m).transform()
                                ),
                                box Lambda::Func(x, n).transform()
                            )
                        },
                        (true, false) => {
                            println!("C");
                            Lambda::App(
                                box Lambda::App(
                                    box Lambda::Term(LTerm::Twist(c)),
                                    box Lambda::Func(x, m).transform()
                                ),
                                box n.transform()
                            )
                        },
                        (false, true) => {
                            println!("B");
                            Lambda::App(
                                box Lambda::App(
                                    box Lambda::Term(LTerm::Twist(b)),
                                    box m.transform()
                                ),
                                box Lambda::Func(x, n).transform()
                            )
                        },
                        (false, false) => panic!("what goes here?"),
                    }
                },
                x @ _ => x
            },
            x @ _ => { println!("1"); x },
        }
    }

    pub fn open(&self) -> Twist {
        match self {
            Lambda::Term(LTerm::Twist(t)) => t.clone(),
            Lambda::App(m, n) => cons(vec![(*m).open(), (*n).open()]),
            _ => panic!(),
        }
    }
}
// wow this looks terrible
// adding B + C makes this more efficient, but not simpler
// i dont think you need fancy-n-reduction for B + C?
// where does the supercombinator stuff come in - probably lifting pure functions
// at the lambda layer
//
// skew literally just copies https://crypto.stanford.edu/~blynn/lambda/logski.html
// as "Oleg's Algorithm"
// do i want to try and make my own transform, based on the fact we have pick?
// im definitely not smart enough for this
//
// (f x y) where f = (E A(2) _ Add) is fine
// (f x y z) where f = (E A(2) _ Mul E A(2) _ Add) is fine? add a test
//fn transform(lam: Lambda) {
//    match lam {
//        Term(x) => x,
//        App(x, y) => App(transform(x), transform(y)),
//        Func(var, body) if *body == var => I.clone(),
//        Func(var, body) if not body.free(var) => {
//            App(Box::new(LTwist(N(K))), transform(x))
//        },
//        Func(x, out) if match out { Func(y, inner) if inner.free(x) => true, _ => false } => {
//            transform(Func(x, transform(Func(y, inner))))
//        }
//        Func(x, body) if match body { App(e1, e2) if e1.free(x) or e2.free(x) => true, _ => false } => {
//            App(App(N(S), transform(Func(x, e1))), transform(Func(x, e2)))
//        }
//    }
//}
