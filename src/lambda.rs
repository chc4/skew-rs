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
use jets::call;
use jets::{Mul, If};
use jets::jet;
use turboprop;

#[derive(Clone, PartialEq)]
pub struct C;
impl Jetted for C {
    fn arity(&self) -> Int {
        3.into()
    }
    #[inline]
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
pub struct B;
impl Jetted for B {
    fn arity(&self) -> Int {
        3.into()
    }
    #[inline]
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
        //println!("reducing {:?}", self);
        match self.clone() {
            Lambda::App(mut m, mut n) => {
                //println!("2");
                Lambda::App(
                    box m.transform(),
                    box n.transform()
                )
            },
            Lambda::Func(x, mut body) => match *body.clone() {
                mut y @ _ if !body.free(x) => {
                    //println!("3");
                    Lambda::App(box Lambda::Term(LTerm::Twist(Twist::N(Skew::K))), box y.transform())
                },
                Lambda::Term(LTerm::Var(x2)) if x == x2 => {
                    //println!("4");
                    Lambda::Term(LTerm::Twist(skew![(S, K, K)]))
                },
                Lambda::Func(y, body2) if body2.free(x) => {
                    //println!("5");
                    Lambda::Func(x, box body.transform()).transform()
                }
                Lambda::App(mut e, box Lambda::Term(LTerm::Var(y))) if !e.free(x) => {
                    //println!("fancy n");
                    e.transform()
                },
                Lambda::App(mut m, mut n) => {
                    //let c = cons(vec![N(E), N(A(Rc::new(C.arity()))), N(K), J(Jet(Rc::new(C)))]);
                    //let b = cons(vec![N(E), N(A(Rc::new(B.arity()))), N(K), J(Jet(Rc::new(B)))]);
                    use turboprop;
                    match (m.free(x), n.free(x)) {
                        (true, true) => {
                            //println!("6");
                            Lambda::App(
                                box Lambda::App(
                                    box Lambda::Term(LTerm::Twist(Twist::N(Skew::S))),
                                    box Lambda::Func(x, m).transform()
                                ),
                                box Lambda::Func(x, n).transform()
                            )
                        },
                        (true, false) => {
                            //println!("C");
                            Lambda::App(
                                box Lambda::App(
                                    box Lambda::Term(LTerm::Twist(Twist::Turbo(turboprop::TURBO_C))),
                                    box Lambda::Func(x, m).transform()
                                ),
                                box n.transform()
                            )
                        },
                        (false, true) => {
                            //println!("B");
                            Lambda::App(
                                box Lambda::App(
                                    box Lambda::Term(LTerm::Twist(Twist::Turbo(turboprop::TURBO_B))),
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
            x @ _ => { /*println!("1");*/ x },
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

#[macro_export]
macro_rules! lambda {
    (fn#$x:ident.$($bod:tt) +) => {
        box Lambda::Func(stringify!($x), lambda!($($bod)+))
    };
    ({$($x:tt)+}) => {
        box Lambda::Term(LTerm::Twist($($x)+))
    };
    (($x:tt $($y:tt) +)) => {
        {
        let x: Box<Lambda> = lambda!($x);
        $(
            let x = box Lambda::App(x, lambda!($y));
        )*
        x}
    };
    ($x:ident) => {
        box Lambda::Term(LTerm::Var(stringify!($x)))
    };
}

#[test]
fn test_lambda(){
    let mut lam = lambda!(fn#x.x);
    assert_eq!(lam.transform().open(), skew![(S, K, K)]);

    let mut swap = lambda!(fn#x.fn#y.(y x));

    println!("before: {:?}", swap);
    let twist_swap = swap.transform().open();
    println!("after: {:?}", twist_swap);

    let mut test_swap = skew![({twist_swap}, {A 1}, {A 2})];
    println!("test swap: {:?}", test_swap);
    test_swap.boil();
    assert_eq!(test_swap, skew![({A 2}, {A 1})]);
}

#[test]
fn test_lambda_mul(){
    let mut mul = lambda!(fn#x.fn#y.(({jet(Mul)} x) y));
    let twist_mul = mul.transform().open();
    println!("twist_mul: {:?}", twist_mul);
    let mut c = skew![({twist_mul}, {A 6}, {A 7})];
    c.boil();
    assert_eq!(c, Twist::atom(42))
}

#[test]
fn test_lambda_if(){
    let mut eq_zero = lambda!(fn#n.({N(Q)} n {Twist::atom(0)}));
    let eq_zero = eq_zero.transform().open();
    let mut _if = lambda!(fn#c.fn#t.fn#f.({N(W)} ({eq_zero} c) (t f)));
    let mut twist_if = _if.transform().open();
    println!("twist_if: {:?}", twist_if);
    let mut c = skew![({twist_if}, {A 0}, {A 4}, {A 5})];
    c.boil();
    assert_eq!(c, Twist::atom(4));
    let mut c = skew![({twist_if}, {A 1}, {A 4}, {A 5})];
    c.boil();
    assert_eq!(c, Twist::atom(5));
}

#[test]
fn test_recurse() {
    let top = Twist::atom(400);
    let mut count = lambda!(fn#f.fn#n.({N(W)} ({N(Q)} n {top.clone()}) ({N(K)} (n (f f ({N(X)} n))) {N(K)})));
    let count = count.transform().open();
    let mut c = skew![({count}, {count}, {A 1})];
    c.boil();
    assert_eq!(c, top.clone());
}

pub fn factorial() -> Twist {
    fn rust_factorial(n: Int) -> Int {
        if n == 0 {
            1.into()
        } else {
            n.clone() * rust_factorial(n - 1)
        }
    }
    let mut factorial = lambda!(fn#f.fn#n.({N(W)} ({N(Q)} n {Twist::atom(0)}) ({N(K)} ({Twist::atom(1)} ({Turbo(turboprop::TURBO_MUL)} n (f f ({Turbo(turboprop::TURBO_DEC)} n)))) {N(K)})));
    let factorial = factorial.transform().open();
    skew![({factorial.clone()}, {factorial})]
}

#[test]
fn test_factorial(){
    fn rust_factorial(n: Int) -> Int {
        if n == 0 {
            1.into()
        } else {
            n.clone() * rust_factorial(n - 1)
        }
    }
    let _f = factorial();
    for i in 0..40 {
        println!("Calculating factorial({})", i);
        let mut c = skew![({_f.clone()}, {Twist::atom(i)})];
        c.boil();
        assert_eq!(c, N(A(Rc::new(rust_factorial(i.into())))));
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
