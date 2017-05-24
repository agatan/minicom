pub mod ir;
pub mod typing2;

use self::ir::Type;
use ast::NodeId;

error_chain! {
    types {
        Error, ErrorKind, ResultExt, Result;
    }

    errors {
        InvalidTypeUnification(t1: Type, t2: Type) {
            description("invalid type unification")
            display("cannot unify types: {:?} and {:?}", t1, t2)
        }

        CannotInfer(x: NodeId) {
            description("cannot infer type")
            display("cannot infer type: {:?}", x)
        }
    }
}
