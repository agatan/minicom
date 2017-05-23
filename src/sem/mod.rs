pub mod ir;
pub mod typing;

use self::ir::{Type, TypeVariable};

error_chain! {
    types {
        Error, ErrorKind, ResultExt, Result;
    }

    errors {
        InvalidTypeUnification(t1: Type, t2: Type) {
            description("invalid type unification")
            display("cannot unify types: {:?} and {:?}", t1, t2)
        }

        NonDeterministicTypeVariable(x: TypeVariable) {
            description("non deterministic type variable")
            display("cannot determine a type variable {:?}", x)
        }
    }
}
