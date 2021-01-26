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

// TARGET FUNCTION
// fact = Y(\f.\n.if(is0(n) 1 mul(n f(dec(n)))))

#[derive(Debug, Clone)]
pub enum LTerm {
    Twist(Twist),
    Var(String),
}
#[derive(Debug, Clone)]
pub enum Lambda {
    Func(String, Box<Lambda>),
    App(Box<Lambda>, Box<Lambda>),
    Term(LTerm),
}

impl Lambda {
    pub fn free(&self, var: String) -> bool {
        match self {
            Lambda::Term(LTerm::Var(x)) => var == *x,
            Lambda::App(x, y) => { x.free(var.clone()) || y.free(var) },
            Lambda::Func(x, bod) => {
                if( *x == var) {
                    return false
                }
                bod.free(var)
            },
            _ => false
        }
    }

    pub fn transform(&self) -> Lambda {
        println!("reducing {:?}", self);
        match self.clone() {
            Lambda::App(m, n) => {
                println!("2");
                Lambda::App(
                    box m.transform(),
                    box n.transform()
                )
            },
            Lambda::Func(x, body) => match *body.clone() {
                y @ _ if !body.free(x.clone()) => {
                    println!("3");
                    Lambda::App(box Lambda::Term(LTerm::Twist(Twist::N(Skew::K))), box y.transform())
                },
                Lambda::Term(LTerm::Var(x2)) if x.clone() == x2 => {
                    println!("4");
                    Lambda::Term(LTerm::Twist(skew![(S, K, K)]))
                },
                Lambda::Func(y, body2) if body2.free(x.clone()) => {
                    println!("5");
                    Lambda::Func(x.clone(), box body.clone().transform()).transform()
                }
                Lambda::App(m, n) if m.free(x.clone()) || n.free(x.clone()) => {
                    println!("6");
                    Lambda::App(
                        box Lambda::App(
                            box Lambda::Term(LTerm::Twist(Twist::N(Skew::S))),
                            box Lambda::Func(x.clone(), m.clone()).transform()
                        ),
                        box Lambda::Func(x.clone(), n.clone()).transform()
                    )
                },
                x @ _ => x
                // this could instead be
                //match (m.free(var), n.free(var)) {
                //  (True, False) => B,
                //  (False, True) => C,
                //  (False, False) => S,
                //  (True, True) => free function
                //}
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
