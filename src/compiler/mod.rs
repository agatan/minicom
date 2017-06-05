use std::rc::Rc;
use std::sync::{Once, ONCE_INIT};
use std::collections::HashMap;

use llvm::{self, Message, Context, Module, Builder, Value, BasicBlock};
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
            .map(|&(_, ref ty)| self.compile_type(ty))
            .collect::<Vec<_>>();
        let ret = self.compile_type(&f.ret_typ);
        let fun_ty = self.ctx.function_type(ret, &args, false);
        let mut fun = self.module.add_function(&f.name, fun_ty);
        let alloc = fun.append_basic_block("entry");
        let start = fun.append_basic_block("start");
        self.builder.position_at_end(&alloc);
        self.builder.br(&start);
        self.builder.position_at_end(&start);
        let last_value = {
            let mut fbuilder = FunBuilder::new(&alloc, self);
            for (name, i) in f.args.iter().map(|x| &x.0).zip(0..) {
                fbuilder.add_local(name, fun.param(i));
            }
            let mut ret = None;
            for node in f.body.iter() {
                ret = Some(fbuilder.compile_node(node));
            }
            ret
        };
        let ret = last_value.unwrap_or_else(|| self.unit());
        self.builder.ret(ret);
        fun
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
        let entry = fun.append_basic_block("entry");
        let start = fun.append_basic_block("start");
        self.builder.position_at_end(&entry);
        self.builder.br(&start);
        self.builder.position_at_end(&start);
        {
            let mut fbuilder = FunBuilder::new(&entry, self);
            for node in program.toplevels.iter() {
                fbuilder.compile_node(node);
            }
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

pub struct FunBuilder<'a, 's> {
    alloc_block: &'a BasicBlock,
    compiler: &'a mut Compiler,
    local_vars: HashMap<&'s str, Value>,
}

impl<'a, 's> FunBuilder<'a, 's> {
    pub fn new(alloc_block: &'a BasicBlock, compiler: &'a mut Compiler) -> Self {
        FunBuilder {
            alloc_block: alloc_block,
            compiler: compiler,
            local_vars: HashMap::new(),
        }
    }

    pub fn add_local(&mut self, name: &'s str, value: Value) {
        self.local_vars.insert(name, value);
    }

    pub fn getvar(&self, name: &'s str) -> Value {
        self.local_vars
            .get(name)
            .cloned()
            .unwrap_or_else(|| self.compiler.getvar(name))
    }

    pub fn compile_node(&mut self, node: &Node) -> Value {
        match node.kind {
            NodeKind::Unit => self.compiler.unit(),
            NodeKind::Int(n) => self.compiler.int(n),
            NodeKind::Float(f) => self.compiler.float(f),
            NodeKind::Bool(b) => self.compiler.bool(b),
            NodeKind::GlobalIdent(ref var) => {
                let ptr = self.compiler.getvar(&var.name);
                self.compiler.builder.load(ptr, "loadtmp")
            }
            NodeKind::AddInt(ref l, ref r) => {
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                self.compiler.builder.add(l, r, "addtmp")
            }
            NodeKind::SubInt(ref l, ref r) => {
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                self.compiler.builder.sub(l, r, "subtmp")
            }
            NodeKind::MulInt(ref l, ref r) => {
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                self.compiler.builder.mul(l, r, "multmp")
            }
            NodeKind::DivInt(ref l, ref r) => {
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                self.compiler.builder.div(l, r, "divtmp")
            }
            NodeKind::AddFloat(ref l, ref r) => {
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                self.compiler.builder.fadd(l, r, "addtmp")
            }
            NodeKind::SubFloat(ref l, ref r) => {
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                self.compiler.builder.fsub(l, r, "subtmp")
            }
            NodeKind::MulFloat(ref l, ref r) => {
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                self.compiler.builder.fmul(l, r, "multmp")
            }
            NodeKind::DivFloat(ref l, ref r) => {
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                self.compiler.builder.fdiv(l, r, "divtmp")
            }
            NodeKind::Not(ref e) => {
                let e = self.compile_node(e);
                self.compiler.builder.not(e, "nottmp")
            }
            NodeKind::Eq(ref l, ref r) => {
                let typ = l.typ.clone();
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                if typ == Type::Int {
                    self.compiler.builder.icmp(llvm::LLVMIntEQ, l, r, "eqtmp")
                } else {
                    self.compiler.builder.fcmp(llvm::LLVMRealOEQ, l, r, "eqtmp")
                }
            }
            NodeKind::LE(ref l, ref r) => {
                let typ = l.typ.clone();
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                if typ == Type::Int {
                    self.compiler.builder.icmp(llvm::LLVMIntSLE, l, r, "letmp")
                } else {
                    self.compiler.builder.fcmp(llvm::LLVMRealOLE, l, r, "letmp")
                }
            }
            NodeKind::LT(ref l, ref r) => {
                let typ = l.typ.clone();
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                if typ == Type::Int {
                    self.compiler.builder.icmp(llvm::LLVMIntSLT, l, r, "lttmp")
                } else {
                    self.compiler.builder.fcmp(llvm::LLVMRealOLT, l, r, "lttmp")
                }
            }
            NodeKind::Let(ref let_) => {
                let ptr = self.getvar(&let_.name);
                let value = self.compile_node(&let_.value);
                self.compiler.builder.store(value, ptr)
            }
            _ => unimplemented!(),
        }
    }
}
