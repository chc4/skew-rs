use Twist;
use jets;
use jets::{Jet, Jetted};
use lambda;

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
                        item.boil();
                    }
                    // XX: this actually has to call $instance.deopt() if its None
                    $instance.call(&new_args[..$arity])
                },
                )+
                [Twist::Turbo(unknown), args @ ..] =>  panic!("unknown turboprop"),
                unjetted @ _ => { panic!("unknown turboprop {:?}", unjetted); } // XX: we actually need to call the deopt reduce
            }
        }

    }
}
turboprop!(
    (2, STATIC_ADD, TURBO_ADD, jets::Add, jets::Add),
    (3, STATIC_C, TURBO_C, lambda::C, lambda::C),
    (3, STATIC_B, TURBO_B, lambda::B, lambda::B)
);


