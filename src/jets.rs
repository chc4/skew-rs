use std::rc::Rc;
use ramp::Int;
use Twist::*;
use Twist;
use Skew::*;
use cons;
use skew;
use dyn_clone::DynClone;

pub trait Jetted: DynClone {
    fn arity(&self) -> Int;
    fn call(&self, args: &[Twist]) -> Option<Twist>;
    fn name(&self) -> String;
}
dyn_clone::clone_trait_object!(Jetted);

// workaround for #39128
// this should probably be a (Rc<dyn Jetter>, TypeId) so that
// jets can match on it for arguments.
#[derive(Clone)]
pub struct Jet(pub Rc<dyn Jetted>);
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

pub fn jet<T: Jetted + 'static>(j: T) -> Twist {
    let mut x = vec![N(E), N(A(Rc::new(j.arity()))), N(K), J(Jet(Rc::new(j)))];
    cons(x)
}

#[inline]
pub fn call<T: Jetted + 'static>(j: T, mut args: Twist) -> Twist {
    // just uses K for the jet hint for now
    let mut x = vec![N(E), N(A(Rc::new(j.arity()))), N(K), J(Jet(Rc::new(j)))];
    if let Expr(mut v) = args {
        x.append(&mut (*v).clone());
    } else {
        x.push(args);
    }
    cons(x)
}
fn delay(t: Twist) -> Twist {
    cons(vec![N(K), t, N(K)])
}
fn defer(t: Twist) -> Twist {
    cons(vec![N(K), t])
}

#[derive(Clone, PartialEq)]
pub struct Add;
impl Jetted for Add {
    fn arity(&self) -> Int {
        2.into()
    }
    #[inline]
    fn call(&self, args: &[Twist]) -> Option<Twist> {
        if let [N(A(n)), N(A(m))] = args {
            return Some(N(A(Rc::new((**n).clone() + (**m).clone()))));
        } else {
            return None;
        }
    }
    fn name(&self) -> String {
        "Add".into()
    }
}
#[test]
fn test_add() {
    let t1 = call(Add, skew![({A 1}, {A 2})]);
    println!("add {:?}", t1);
    assert_eq!(t1.reduce().unwrap(), Twist::atom(3));
}
#[test]
fn test_add_twice() {
    let t1 = call(Add, skew![({A 1}, {A 2})]);
    // the extra () around t1 is needed here so it groups correctly
    // (i think its actually just a problem with call()?)
    let t2 = call(Add, skew![(({t1}), {A 3})]).reduce().unwrap();
    println!("add {:?}", t2);
    assert_eq!(t2, Twist::atom(6));
}

#[test]
fn test_add_argument_eval() {
    let lazy_1 = delay(delay(Twist::atom(1)));
    let t1 = call(Add, skew![(({lazy_1}), {A 2})]).reduce().unwrap();
    assert_eq!(t1, Twist::atom(3));
}

#[derive(Clone, PartialEq)]
struct Add3;
impl Jetted for Add3 {
    fn arity(&self) -> Int {
        3.into()
    }
    fn call(&self, args: &[Twist]) -> Option<Twist> {
        if let [N(A(n)), N(A(m)), N(A(o))] = args {
            return Some(N(A(Rc::new((**n).clone() + (**m).clone() + (**o).clone()))));
        } else {
            return None;
        }
    }
    fn name(&self) -> String {
        "Add3".into()
    }
}

#[test]
fn test_add3(){
    let t1 = call(Add3, skew![({A 1}, {A 2}, {A 3})]);
    assert_eq!(t1.reduce().unwrap(), Twist::atom(6));
}

#[test]
fn test_add3_lazy(){
    let lazy_1 = delay(delay(Twist::atom(1)));
    let t1 = call(Add3, skew![(({lazy_1}), {A 2}, {A 3})]);
    assert_eq!(t1.reduce().unwrap(), Twist::atom(6));
}

#[derive(Clone, PartialEq)]
pub struct Mul;
impl Jetted for Mul {
    fn arity(&self) -> Int {
        2.into()
    }
    fn call(&self, args: &[Twist]) -> Option<Twist> {
        if let [N(A(n)), N(A(m))] = args {
            return Some(N(A(Rc::new((**n).clone() * (**m).clone()))));
        } else {
            return None;
        }
    }
    fn name(&self) -> String {
        "Mul".into()
    }
}

#[derive(Clone, PartialEq)]
struct If;
impl Jetted for If {
    fn arity(&self) -> Int {
        3.into()
    }
    fn call(&self, args: &[Twist]) -> Option<Twist> {
        if let [N(A(n)), t, f] = args {
            if **n == 0 {
                return Some(t.clone());
            } else {
                return Some(f.clone());
            }
        } else {
            return None;
        }
    }
    fn name(&self) -> String {
        "If".into()
    }
}

#[test]
fn test_if_true() {
    let t1 = call(If, skew![({A 0}, {A 1}, {A 2})]);
    assert_eq!(t1.reduce().unwrap(), Twist::atom(1));
}
#[test]
fn test_if_false() {
    let t1 = call(If, skew![({A 1}, {A 1}, {A 2})]);
    assert_eq!(t1.reduce().unwrap(), Twist::atom(2));
}
#[test]
fn test_if_lazy() {
    let t1 = call(If, skew![({A 0}, {defer(Twist::atom(1))}, {defer(Twist::atom(2))}, K)]);
    assert_eq!(t1.reduce().unwrap(), delay(Twist::atom(1)));
}
#[test]
fn test_if_lazy_deep() {
    //let t1 = cons(vec![call(If, vec![Twist::atom(0), defer(Twist::atom(1)), defer(Twist::atom(2))]), N(K)]);
    let t1 = skew![({call(If, skew![({A 0}, {defer(Twist::atom(1))}, {defer(Twist::atom(2))})])}, K)];
    assert_eq!(t1.reduce().unwrap(), delay(Twist::atom(1)));
}
