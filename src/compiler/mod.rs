use std::rc::Rc;
use std::sync::{Once, ONCE_INIT};
use std::collections::HashMap;

use llvm::{self, Message, Context, Module, Builder, Value};
use sem::ir::*;

pub struct Compiler {
    ctx: Rc<Context>,
    module: Module,
    builder: Builder,
    globals: HashMap<String, Value>,
}

static LLVM_INIT: Once = ONCE_INIT;

impl Compiler {
    pub fn new() -> Self {
        LLVM_INIT.call_once(|| llvm::init());
        let ctx = Context::new();
        let module = Module::new(ctx.clone(), "main");
        let builder = Builder::new(ctx.clone());
        Compiler {
            ctx: ctx,
            module: module,
            builder: builder,
            globals: HashMap::new(),
        }
    }

    pub fn test_run(&mut self) -> Result<(), Message> {
        let int_ty = self.ctx.int32_type();
        let fun_ty = self.ctx.function_type(int_ty, &[], false);
        let mut fun = self.module.add_function("main", fun_ty);
        let bb = fun.append_basic_block("entry");
        self.builder.position_at_end(&bb);
        let a = self.builder.alloca(int_ty, "a");
        let b = self.builder.alloca(int_ty, "b");
        let x: i32 = -10000;
        self.builder.store(int_ty.const_int(x as u64), a);
        self.builder.store(int_ty.const_int(11), b);
        let a = self.builder.load(a, "a");
        let b = self.builder.load(b, "b");
        let ret = self.builder.add(a, b, "addtmp");
        self.builder.ret(ret);
        self.module.verify()
    }

    pub fn dump_module(&self) {
        self.module.dump();
    }

    fn compile_type(&mut self, ty: &Type) -> llvm::Type {
        match *ty {
            Type::Int => self.ctx.int32_type(),
            Type::Float => self.ctx.float_type(),
            Type::Bool => self.ctx.bool_type(),
            Type::Unit => self.ctx.unit_type(),
        }
    }

    fn compile_function(&mut self, f: &Function) -> llvm::Function {
        let args = f.args
            .iter()
            .map(|ty| self.compile_type(ty))
            .collect::<Vec<_>>();
        let ret = self.compile_type(&f.ret_typ);
        let fun_ty = self.ctx.function_type(ret, &args, false);
        let mut fun = self.module.add_function(&f.name, fun_ty);
        let bb = fun.append_basic_block("entry");
        self.builder.position_at_end(&bb);
        let mut ret = None;
        for node in f.body.iter() {
            ret = Some(self.compile_node(node));
        }
        let ret = ret.unwrap_or_else(|| self.unit());
        self.builder.ret(ret);
        fun
    }

    pub fn compile_node(&mut self, node: &Node) -> Value {
        match node.kind {
            NodeKind::Unit => self.unit(),
            NodeKind::Int(n) => self.int(n),
            NodeKind::Float(f) => self.float(f),
            NodeKind::Bool(b) => self.bool(b),
            NodeKind::GlobalIdent(ref var) => {
                let ptr = self.getvar(&var.name);
                self.builder.load(ptr, "loadtmp")
            }
            NodeKind::AddInt(ref l, ref r) => {
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                self.builder.add(l, r, "addtmp")
            }
            NodeKind::SubInt(ref l, ref r) => {
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                self.builder.sub(l, r, "subtmp")
            }
            NodeKind::MulInt(ref l, ref r) => {
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                self.builder.mul(l, r, "multmp")
            }
            NodeKind::DivInt(ref l, ref r) => {
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                self.builder.div(l, r, "divtmp")
            }
            NodeKind::AddFloat(ref l, ref r) => {
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                self.builder.fadd(l, r, "addtmp")
            }
            NodeKind::SubFloat(ref l, ref r) => {
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                self.builder.fsub(l, r, "subtmp")
            }
            NodeKind::MulFloat(ref l, ref r) => {
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                self.builder.fmul(l, r, "multmp")
            }
            NodeKind::DivFloat(ref l, ref r) => {
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                self.builder.fdiv(l, r, "divtmp")
            }
            NodeKind::Not(ref e) => {
                let e = self.compile_node(e);
                self.builder.not(e, "nottmp")
            }
            NodeKind::Eq(ref l, ref r) => {
                let typ = l.typ.clone();
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                if typ == Type::Int {
                    self.builder.icmp(llvm::LLVMIntEQ, l, r, "eqtmp")
                } else {
                    self.builder.fcmp(llvm::LLVMRealOEQ, l, r, "eqtmp")
                }
            }
            NodeKind::Let(ref let_) => {
                let ptr = self.getvar(&let_.name);
                let value = self.compile_node(&let_.value);
                self.builder.store(value, ptr)
            }
            _ => unimplemented!(),
        }
    }

    pub fn compile_program(&mut self, program: &Program) -> Result<&Module, Message> {
        for entry in program.entries.values() {
            match *entry {
                Entry::Var(ref var) => {
                    let ty = self.compile_type(&var.typ);
                    let v = self.module.add_global(&var.name, ty);
                    v.set_initializer(self.zerovalue(&var.typ));
                    self.globals.insert(var.name.clone(), v);
                }
                Entry::Function(ref f) => {
                    self.compile_function(f);
                }
            }
        }
        let fun_ty = self.ctx.function_type(self.ctx.unit_type(), &[], false);
        let mut fun = self.module.add_function("main", fun_ty);
        let bb = fun.append_basic_block("entry");
        self.builder.position_at_end(&bb);
        for node in program.toplevels.iter() {
            self.compile_node(node);
        }
        let ret = self.unit();
        self.builder.ret(ret);
        self.module.verify()?;
        Ok(&self.module)
    }

    fn getvar(&self, name: &str) -> Value {
        *self.globals
             .get(name)
             .expect(&format!("undefined variable: {}", name))
    }

    // helpers
    fn unit(&self) -> Value {
        self.ctx.unit_type().const_int(0)
    }

    fn bool(&self, b: bool) -> Value {
        self.ctx.bool_type().const_int(b as ::libc::c_ulonglong)
    }

    fn int(&self, n: i32) -> Value {
        self.ctx.int32_type().const_int(n as ::libc::c_ulonglong)
    }

    fn float(&self, f: f64) -> Value {
        self.ctx.float_type().const_float(f)
    }

    fn zerovalue(&self, ty: &Type) -> Value {
        match *ty {
            Type::Unit => self.unit(),
            Type::Int => self.int(0),
            Type::Bool => self.bool(false),
            Type::Float => self.float(0.0),
        }
    }
}
