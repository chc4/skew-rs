use Twist;
use jets;
use jets::{Jet, Jetted};
use lambda;
use cons;

use linkme::distributed_slice;
use ramp::Int;
use std::any::TypeId;

// Turboprops are jets that we register with the VM at compile-time. We then special
// case them, so that reduce() matches on a single Twist::Turbo variant and calls
// the correct static function instead of an expr containing
// (E, A(arity), _, dyn Jet) and having to do a trait object dispatch.
#[derive(Clone, PartialEq)]
pub struct Turboprop(Int,TypeId);
#[distributed_slice]
pub static TURBOPROPS: [&'static (dyn Jetted + Sync)] = [..];

#[macro_export]
macro_rules! turboprop {
    ( $(($arity:literal, $instance:ident, $turbo:ident, $ty:ty, $val:expr)),+ ) => {
        $(
        pub static $instance: $ty = $val;
        #[distributed_slice(TURBOPROPS)]
        pub static $turbo: &'static (dyn Jetted + Sync) = &$instance;
        )+
        pub fn turboprop(twist: &[Twist]) -> Option<Twist> {
            match twist {
                $(
                [Twist::Turbo(prop), args @ ..] if *prop as *const dyn Jetted == ($turbo as &'static dyn Jetted) && args.len() >= $arity => {
                    // XX: extract this + main's jet handling to a helper
                    let mut new_args = args.to_owned();
                    for item in new_args[..$arity].iter_mut() {
                        *item = item.clone().cook();
                    }
                    // XX: this actually has to call $instance.deopt() if its None
                    let r = $instance.call(&new_args[..$arity]);
                    if let Some(mut r) = r {
                        if(new_args.len() > $arity) {
                            let mut v = vec![r];
                            v.extend_from_slice(&new_args[$arity..]);
                            let mut v = cons(v);
                            // XX: this shouldn't be boil, but instead a cook()
                            // requires changing B and C to call intermediates
                            v.boil();
                            Some(v)
                        } else {
                            r.boil();
                            Some(r)
                        }
                    } else {
                        None
                    }
                },
                )+
                //[Twist::Turbo(unknown), args @ ..] =>  panic!("unknown turboprop"),
                unjetted @ _ => {
                    //println!("unknown turboprop {:?}", unjetted);
                    None
                } // XX: we actually need to call the deopt reduce
            }
        }

    }
}
turboprop!(
    (1, STATIC_DEC, TURBO_DEC, jets::Dec, jets::Dec),
    (2, STATIC_ADD, TURBO_ADD, jets::Add, jets::Add),
    (2, STATIC_MUL, TURBO_MUL, jets::Mul, jets::Mul),
    (3, STATIC_C, TURBO_C, lambda::C, lambda::C),
    (3, STATIC_B, TURBO_B, lambda::B, lambda::B)
);


