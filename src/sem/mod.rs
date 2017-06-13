use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut, Drop};

pub mod ir;
mod typing;
mod alpha;
mod tyenv;
pub mod infer;

use basis::pos::{Span, Spanned};
use basis::errors::Error as BasisError;
use syntax::ast::{self, Toplevel, ToplevelKind, NodeId, Node as AstNode, NodeKind as AstNodeKind,
                  Operator};

use self::ir::*;
pub use self::typing::TypeMap;
use self::tyenv::TypeEnv;
use self::alpha::Alpha;
use self::infer::Infer;

mod errors {
    error_chain! {
        errors { }
    }
}

pub use self::errors::{Error, ErrorKind};

pub type Result<T> = ::std::result::Result<T, BasisError<Error>>;

macro_rules! bail_with {
    ($span:expr, $kind:expr) => {
        return Err(BasisError::span($span, $kind.into()));
    };
    ($span:expr, $fmt:expr, $($args:tt)+) => {
        return Err(BasisError::span($span, ErrorKind::Msg(format!($fmt, $($args)+))));
    };
}

#[derive(Debug)]
pub struct Context {
    inferer: Infer,
    program: Program,
}

impl Context {
    pub fn new() -> Self {
        Context {
            inferer: Infer::new(),
            program: Program::new(),
        }
    }

    fn process_node(&mut self, node: Spanned<AstNode>) -> Result<Node> {
        unimplemented!()
    }

    fn process_global_def(&mut self, id: NodeId, def: Spanned<ast::Def>) -> Result<()> {
        let Spanned { value: def, span } = def;
        let (arg_types, ret) = self.inferer.get_toplevel_function(id);
        let body = self.process_node(def.body)?;
        let args = def.args
            .into_iter()
            .map(|(name, _)| name)
            .zip(arg_types)
            .collect();
        let function = Function {
            name: def.name.clone(),
            args: args,
            ret_typ: ret,
            body: Box::new(body),
        };
        self.program.define_function(def.name, function);
        Ok(())
    }

    fn process_global_let(&mut self, let_: Spanned<ast::Let>) -> Result<()> {
        let Spanned { value: let_, span } = let_;
        let irnode = self.process_node(let_.value)?;
        let typ = self.inferer.gettyp(let_.value.value.id);
        self.program.define_global(let_.name.clone(), typ.clone());
        let var = Var {
            name: let_.name,
            typ: typ,
        };
        let assignment = Node::new(NodeKind::AssignGlobal(var, Box::new(irnode)), Type::Unit);
        self.program.append_toplevel(assignment);
        Ok(())
    }

    fn process_toplevel(&mut self, toplevel: Spanned<Toplevel>) -> Result<()> {
        match toplevel.value.kind {
            ToplevelKind::Def(def) => {
                self.process_global_def(toplevel.value.id, Spanned::span(toplevel.span, *def))
            }
            ToplevelKind::Let(let_) => self.process_global_let(Spanned::span(toplevel.span, *let_)),
            ToplevelKind::Expr(expr) => {
                let node = self.process_node(*expr)?;
                self.program.append_toplevel(node);
                Ok(())
            }
        }
    }

    /// `check_and_transform` handle type checking and inference by `Infer`, and transform AST
    /// nodes to semantical IR (`Program`).
    /// All transformation functions in `Context` does not do any type checks because `Infer`
    /// already checks them.
    pub fn check_and_transform(&mut self, program: Vec<Spanned<Toplevel>>) -> Result<Program> {
        self.inferer.infer_program(&program)?;
        let mut alp = Alpha::new();
        let program = program
            .into_iter()
            .map(|n| n.map(|n| alp.apply_toplevel(n)))
            .collect::<Vec<_>>();
        for toplevel in program {
            self.process_toplevel(toplevel)?;
        }
        Ok(::std::mem::replace(&mut self.program, Program::new()))
    }
}
