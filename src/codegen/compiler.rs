use std::rc::Rc;
use std::sync::{Once, ONCE_INIT};
use std::collections::HashMap;

use llvm::{self, Context, Module, Builder, Value, BasicBlock};
use llvm::target;
use sem::ir::*;

use super::{Error, ResultExt};

pub struct Compiler {
    ctx: Rc<Context>,
    pub module: Module,
    builder: Builder,
    globals: HashMap<String, Value>,

    pub machine: target::TargetMachine,
    target_data: target::TargetData,
}

static LLVM_INIT: Once = ONCE_INIT;

impl Compiler {
    pub fn new() -> Result<Self, Error> {
        LLVM_INIT.call_once(|| llvm::init());
        let ctx = Context::new();
        let module = Module::new(ctx.clone(), "main");
        let builder = Builder::new(ctx.clone());
        let triple = target::get_default_target_triple();
        let t = target::Target::from_triple(triple)
            .map_err(|err| Error::from(err.to_string()))
            .chain_err(|| "failed to initialize llvm target")?;
        let tm = target::TargetMachine::new(triple, t);
        let data = tm.data_layout();
        Ok(Compiler {
               ctx: ctx,
               module: module,
               builder: builder,
               globals: HashMap::new(),
               machine: tm,
               target_data: data,
           })
    }

    fn compile_type(&self, ty: &Type) -> llvm::Type {
        match *ty {
            Type::Int => self.ctx.int32_type(),
            Type::Float => self.ctx.float_type(),
            Type::Bool => self.ctx.bool_type(),
            Type::Unit => self.ctx.unit_type(),
            Type::Ref(ref inner) => {
                let inner = self.compile_type(inner);
                inner.pointer_type()
            }
            Type::Var(_) => unreachable!(),
        }
    }

    fn declare_function(&mut self, f: &Function) -> llvm::Function {
        let args = f.args
            .iter()
            .map(|&(_, ref ty)| self.compile_type(ty))
            .collect::<Vec<_>>();
        let ret = self.compile_type(&f.ret_typ);
        let fun_ty = self.ctx.function_type(ret, &args, false);
        self.module.add_function(&f.name, fun_ty)
    }

    fn compile_function(&mut self, f: &Function) -> llvm::Function {
        let mut fun = self.module
            .get_function(&f.name)
            .expect("undeclared function");
        let alloc = fun.append_basic_block("entry");
        let start = fun.append_basic_block("start");
        self.builder.position_at_end(&start);
        let ret = {
            let mut fbuilder = FunBuilder::new(&alloc, self);
            for (name, i) in f.args.iter().map(|x| &x.0).zip(0..) {
                fbuilder.define_arg(name, fun.param(i));
            }
            fbuilder.compile_node(&f.body)
        };
        self.builder.ret(ret);
        self.builder.position_at_end(&alloc);
        self.builder.br(&start);
        fun
    }

    fn compile_main_function(&mut self,
                             f: Option<&Function>,
                             init_fun: llvm::Function)
                             -> llvm::Function {
        let fun_ty = self.ctx.function_type(self.ctx.int32_type(), &[], false);
        let mut fun = self.module.add_function("main", fun_ty);
        let alloc = fun.append_basic_block("entry");
        let start = fun.append_basic_block("start");
        self.builder.position_at_end(&start);
        match f {
            Some(f) => {
                self.builder.call(init_fun, &[], "calltmp");
                let mut fbuilder = FunBuilder::new(&alloc, self);
                for (name, i) in f.args.iter().map(|x| &x.0).zip(0..) {
                    fbuilder.define_arg(name, fun.param(i));
                }
                fbuilder.compile_node(&f.body);
            }
            None => {
                self.builder.call(init_fun, &[], "calltmp");
            }
        }
        let ret = self.ctx.int32_type().const_int(0);
        self.builder.ret(ret);
        self.builder.position_at_end(&alloc);
        self.builder.br(&start);
        fun
    }

    pub fn compile_program(&mut self, program: &Program) -> Result<&Module, Error> {
        // FIXME(agatan): language items
        {
            // print_unit
            let fun_ty = self.ctx
                .function_type(self.ctx.unit_type(), &[self.ctx.unit_type()], false);
            self.module.add_function("print_unit", fun_ty);
        }
        {
            // print_int
            let fun_ty = self.ctx
                .function_type(self.ctx.int32_type(), &[self.ctx.int32_type()], false);
            self.module.add_function("print_int", fun_ty);
        }
        {
            // print_bool
            let fun_ty = self.ctx
                .function_type(self.ctx.bool_type(), &[self.ctx.bool_type()], false);
            self.module.add_function("print_bool", fun_ty);
        }
        {
            // print_float
            let fun_ty = self.ctx
                .function_type(self.ctx.float_type(), &[self.ctx.float_type()], false);
            self.module.add_function("print_float", fun_ty);
        }
        {
            // gc_malloc
            let fun_ty = self.ctx
                .function_type(self.ctx.void_ptr_type(),
                               &[self.target_data.int_ptr_typ()],
                               false);
            self.module.add_function("gc_malloc", fun_ty);
        }
        let gc_init_fun = {
            // gc_init
            let fun_ty = self.ctx.function_type(self.ctx.void_type(), &[], false);
            self.module.add_function("gc_init", fun_ty)
        };
        // declare global variables and functions
        for entry in program.entries.values() {
            match *entry {
                Entry::Var(ref var) => {
                    let ty = self.compile_type(&var.typ);
                    let v = self.module.add_global(&var.name, ty);
                    v.set_initializer(self.zerovalue(&var.typ));
                    self.globals.insert(var.name.clone(), v);
                }
                Entry::Function(ref f) => {
                    if !f.is_main() {
                        self.declare_function(f);
                    }
                }
            }
        }
        // compile functions
        let mut main_function = None;
        for entry in program.entries.values() {
            if let Entry::Function(ref f) = *entry {
                if f.is_main() {
                    main_function = Some(f);
                } else {
                    self.compile_function(f);
                }
            }
        }
        let init_fun_ty = self.ctx.function_type(self.ctx.int32_type(), &[], false);
        let mut init_fun = self.module.add_function("minivm.init", init_fun_ty);
        let entry = init_fun.append_basic_block("entry");
        let start = init_fun.append_basic_block("start");
        self.builder.position_at_end(&start);
        {
            let mut fbuilder = FunBuilder::new(&entry, self);
            // call gc_init
            fbuilder.compiler.builder.call(gc_init_fun, &[], "");
            for node in program.inits.iter() {
                fbuilder.compile_node(node);
            }
        }
        let ret = self.int(0);
        self.builder.ret(ret);
        self.builder.position_at_end(&entry);
        self.builder.br(&start);

        // compile main function
        self.compile_main_function(main_function, init_fun);

        self.module
            .verify()
            .map_err(|err| Error::from(format!("{}", err)))
            .chain_err(|| {
                           format!("failed to generate valid LLVM IR: {}",
                                   self.module.to_string())
                       })?;
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
            Type::Ref(ref inner) => self.compile_type(inner).pointer_type().const_null(),
            Type::Var(_) => unreachable!(),
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

    pub fn define_arg(&mut self, name: &'s str, value: Value) {
        let saved = self.compiler.builder.get_insert_block();
        self.compiler.builder.position_at_end(self.alloc_block);
        let alloca = self.compiler.builder.alloca(value.get_type(), name);
        self.compiler.builder.store(value, alloca);
        self.add_local(name, alloca);
        self.compiler.builder.position_at_end(&saved);
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

    pub fn getvar_opt(&self, name: &'s str) -> Option<Value> {
        self.local_vars.get(name).cloned()
    }

    pub fn alloca(&mut self, name: &'s str, typ: llvm::Type) -> Value {
        let saved = self.compiler.builder.get_insert_block();
        self.compiler.builder.position_at_end(self.alloc_block);
        let alloca = self.compiler.builder.alloca(typ, name);
        self.compiler.builder.position_at_end(&saved);
        alloca
    }

    pub fn malloc(&mut self, typ: llvm::Type) -> Value {
        let size = self.compiler.target_data.store_size_of_type(&typ);
        let size_value = self.compiler.target_data.int_ptr_typ().const_int(size);
        let f = self.compiler
            .module
            .get_function("gc_malloc")
            .expect("runtime function `gc_malloc` is undefined");
        let void_ptr = self.compiler.builder.call(f, &[size_value], "malloctmp");
        self.compiler
            .builder
            .bitcast(void_ptr, typ.pointer_type(), "mallocptr")
    }

    pub fn compile_node(&mut self, node: &'s Node) -> Value {
        match node.kind {
            NodeKind::Unit => self.compiler.unit(),
            NodeKind::Int(n) => self.compiler.int(n),
            NodeKind::Float(f) => self.compiler.float(f),
            NodeKind::Bool(b) => self.compiler.bool(b),
            NodeKind::Ident(ref var) => {
                let ptr = self.getvar(&var.name);
                self.compiler.builder.load(ptr, "loadtmp")
            }
            NodeKind::Call(ref f, ref args) => {
                let f = self.compiler
                    .module
                    .get_function(f)
                    .expect("function should be defined");
                let args = args.iter()
                    .map(|arg| self.compile_node(arg))
                    .collect::<Vec<_>>();
                self.compiler.builder.call(f, &args, "calltmp")
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
            NodeKind::Print(ref e) => {
                let typ = &e.typ;
                let e = self.compile_node(e);
                let fname = match *typ {
                    Type::Unit => "print_unit",
                    Type::Int => "print_int",
                    Type::Bool => "print_bool",
                    Type::Float => "print_float",
                    _ => unreachable!(),
                };
                let f = self.compiler
                    .module
                    .get_function(fname)
                    .expect("predefined builtin functions");
                self.compiler.builder.call(f, &[e], "calltmp")
            }
            NodeKind::Block(ref es) => {
                match es.split_last() {
                    None => self.compiler.unit(),
                    Some((last, init)) => {
                        for n in init {
                            self.compile_node(n);
                        }
                        self.compile_node(last)
                    }
                }
            }
            NodeKind::If(ref cond, ref then, ref els) => {
                let cond = self.compile_node(cond);
                let mut fun = self.compiler.builder.get_insert_block().parent();
                let mut then_bb = fun.append_basic_block("then");
                let mut els_bb = fun.append_basic_block("else");
                let merge_bb = fun.append_basic_block("ifcont");
                self.compiler.builder.cond_br(cond, &then_bb, &els_bb);
                self.compiler.builder.position_at_end(&then_bb);
                let then_value = self.compile_node(then);
                self.compiler.builder.br(&merge_bb);
                // insertion block can be changed while compiling then block.
                then_bb = self.compiler.builder.get_insert_block();
                self.compiler.builder.position_at_end(&els_bb);
                let els_value = match *els {
                    Some(ref els) => self.compile_node(els),
                    None => self.compiler.unit(),
                };
                self.compiler.builder.br(&merge_bb);
                els_bb = self.compiler.builder.get_insert_block();
                self.compiler.builder.position_at_end(&merge_bb);
                let phi_type = self.compiler.compile_type(&node.typ);
                let mut phi = self.compiler.builder.phi(phi_type, "iftmp");
                phi.add_incoming(&[(then_bb, then_value), (els_bb, els_value)]);
                phi
            }
            NodeKind::While(ref cond, ref body) => {
                let mut fun = self.compiler.builder.get_insert_block().parent();
                let cond_bb = fun.append_basic_block("cond");
                self.compiler.builder.br(&cond_bb);
                let body_bb = fun.append_basic_block("body");
                let exit_bb = fun.append_basic_block("exit");
                self.compiler.builder.position_at_end(&cond_bb);
                let cond_value = self.compile_node(cond);
                self.compiler
                    .builder
                    .cond_br(cond_value, &body_bb, &exit_bb);
                self.compiler.builder.position_at_end(&body_bb);
                self.compile_node(body);
                self.compiler.builder.br(&cond_bb);
                self.compiler.builder.position_at_end(&exit_bb);
                self.compiler.unit()
            }
            NodeKind::Let(ref let_) => {
                let ptr = match self.getvar_opt(&let_.name) {
                    Some(ptr) => ptr,
                    None => {
                        let typ = self.compiler.compile_type(&let_.typ);
                        let ptr = self.alloca(&let_.name, typ);
                        self.add_local(&let_.name, ptr);
                        ptr
                    }
                };
                let value = self.compile_node(&let_.value);
                self.compiler.builder.store(value, ptr)
            }
            NodeKind::AssignGlobal(ref var, ref value) => {
                let ptr = self.compiler.getvar(&var.name);
                let value = self.compile_node(value);
                self.compiler.builder.store(value, ptr)
            }
            NodeKind::Ref(ref e) => {
                let value = self.compile_node(e);
                let typ = value.get_type();
                let ptr = self.malloc(typ);
                self.compiler.builder.store(value, ptr);
                ptr
            }
            NodeKind::Deref(ref e) => {
                let value = self.compile_node(e);
                self.compiler.builder.load(value, "loadtmp")
            }
            NodeKind::Assign(ref to, ref value) => {
                let ptr = self.compile_node(to);
                let value = self.compile_node(value);
                self.compiler.builder.store(value, ptr)
            }
        }
    }
}
