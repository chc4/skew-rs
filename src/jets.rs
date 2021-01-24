use ramp::Int;
use Twist::*;
use Twist;
use Skew::*;
use Jetted;
use Jet;
use cons;

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
struct If;
impl Jetted for If {
    fn arity(&self) -> Int {
        3.into()
    }
    fn call(&self, args: &[Twist]) -> Option<Twist> {
        if let [N(A(n)), t, f] = args {
            if *n == 0 {
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

fn call<T: Jetted + 'static>(j: T, mut args: Vec<Twist>) -> Twist {
    // just uses K for the jet hint for now
    let mut x = vec![N(E), N(A(j.arity())), N(K), J(Jet(Box::new(j)))];
    x.append(&mut args);
    cons(x)
}
fn delay(t: Twist) -> Twist {
    cons(vec![N(K), t, N(K)])
}
fn defer(t: Twist) -> Twist {
    cons(vec![N(K), t])
}

#[test]
fn test_add() {
    //let t1 = cons(vec![N(E), Twist::atom(2), N(K), J(Jet(Box::new(Add))), Twist::atom(1), Twist::atom(2)]).reduce().unwrap();
    let t1 = call(Add, vec![Twist::atom(1), Twist::atom(2)]);
    println!("add {:?}", t1);
    assert_eq!(t1.reduce().unwrap(), Twist::atom(3));
}
#[test]
fn test_add_argument_eval() {
    let lazy_1 = delay(delay(Twist::atom(1)));
    let t1 = cons(vec![N(E), Twist::atom(2), N(K), J(Jet(Box::new(Add))), lazy_1, Twist::atom(2)]).reduce().unwrap();
    assert_eq!(t1, Twist::atom(3));
}

#[test]
fn test_if_true() {
    let t1 = call(If, vec![Twist::atom(0), Twist::atom(1), Twist::atom(2)]);
    assert_eq!(t1.reduce().unwrap(), Twist::atom(1));
}
#[test]
fn test_if_false() {
    let t1 = call(If, vec![Twist::atom(1), Twist::atom(1), Twist::atom(2)]);
    assert_eq!(t1.reduce().unwrap(), Twist::atom(2));
}
#[test]
fn test_if_lazy() {
    let t1 = cons(vec![call(If, vec![Twist::atom(0), defer(Twist::atom(1)), defer(Twist::atom(2))]), N(K)]);
    assert_eq!(t1.reduce().unwrap(), delay(Twist::atom(1)));
}
