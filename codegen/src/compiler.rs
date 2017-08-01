use std::rc::Rc;
use std::sync::{Once, ONCE_INIT};
use std::collections::HashMap;

use llvm::{self, Context, Module, Builder, Value, BasicBlock};
use llvm::target;
use mir::{Program, Node, NodeKind, Type, Decl, Def};

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
        }
    }

    fn declare_function(&mut self, f: &Def) -> llvm::Function {
        let params = f.params
            .iter()
            .map(|param| self.compile_type(&param.typ))
            .collect::<Vec<_>>();
        let ret = self.compile_type(&f.ret);
        let fun_ty = self.ctx.function_type(ret, &params, false);
        self.module.add_function(&f.name, fun_ty)
    }

    fn compile_function(&mut self, f: &Def) -> llvm::Function {
        let mut fun = self.module.get_function(&f.name).expect(
            "undeclared function",
        );
        let alloc = fun.append_basic_block("entry");
        let start = fun.append_basic_block("start");
        self.builder.position_at_end(&start);
        let ret = {
            let mut fbuilder = FunBuilder::new(&alloc, self);
            for (name, i) in f.params.iter().map(|x| &x.name).zip(0..) {
                fbuilder.define_arg(name, fun.param(i));
            }
            fbuilder.compile_node(&f.body)
        };
        self.builder.ret(ret);
        self.builder.position_at_end(&alloc);
        self.builder.br(&start);
        fun
    }

    fn compile_main_function(
        &mut self,
        main_body: &Option<Node>,
        init_fun: llvm::Function,
    ) -> llvm::Function {
        let fun_ty = self.ctx.function_type(self.ctx.int32_type(), &[], false);
        let mut fun = self.module.add_function("main", fun_ty);
        let alloc = fun.append_basic_block("entry");
        let start = fun.append_basic_block("start");
        self.builder.position_at_end(&start);
        match *main_body {
            Some(ref body) => {
                self.builder.call(init_fun, &[], "calltmp");
                let mut fbuilder = FunBuilder::new(&alloc, self);
                fbuilder.compile_node(body);
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
            let fun_ty = self.ctx.function_type(
                self.ctx.unit_type(),
                &[self.ctx.unit_type()],
                false,
            );
            self.module.add_function("print_unit", fun_ty);
        }
        {
            // print_int
            let fun_ty = self.ctx.function_type(
                self.ctx.int32_type(),
                &[self.ctx.int32_type()],
                false,
            );
            self.module.add_function("print_int", fun_ty);
        }
        {
            // print_bool
            let fun_ty = self.ctx.function_type(
                self.ctx.bool_type(),
                &[self.ctx.bool_type()],
                false,
            );
            self.module.add_function("print_bool", fun_ty);
        }
        {
            // print_float
            let fun_ty = self.ctx.function_type(
                self.ctx.float_type(),
                &[self.ctx.float_type()],
                false,
            );
            self.module.add_function("print_float", fun_ty);
        }
        {
            // gc_malloc
            let fun_ty = self.ctx.function_type(
                self.ctx.void_ptr_type(),
                &[self.target_data.int_ptr_typ()],
                false,
            );
            self.module.add_function("gc_malloc", fun_ty);
        }
        let gc_init_fun = {
            // gc_init
            let fun_ty = self.ctx.function_type(self.ctx.void_type(), &[], false);
            self.module.add_function("gc_init", fun_ty)
        };
        // declare global variables and functions
        for decl in program.decls.values() {
            match *decl {
                Decl::Let(ref let_) => {
                    let ty = self.compile_type(&let_.value.typ);
                    let v = self.module.add_global(&let_.name, ty);
                    v.set_initializer(self.zerovalue(&let_.value.typ));
                    self.globals.insert(let_.name.clone(), v);
                }
                Decl::Def(ref f) => {
                    self.declare_function(f);
                }
            }
        }
        // compile functions
        for entry in program.decls.values() {
            if let Decl::Def(ref f) = *entry {
                self.compile_function(f);
            }
        }
        let init_fun_ty = self.ctx.function_type(self.ctx.int32_type(), &[], false);
        let mut init_fun = self.module.add_function("minicom.init", init_fun_ty);
        let entry = init_fun.append_basic_block("entry");
        let start = init_fun.append_basic_block("start");
        self.builder.position_at_end(&start);
        {
            let mut fbuilder = FunBuilder::new(&entry, self);
            // call gc_init
            fbuilder.compiler.builder.call(gc_init_fun, &[], "");
            // initialize global variables
            for node in program.decls.values() {
                if let Decl::Let(ref let_) = *node {
                    let to = fbuilder.compiler.module.get_global(&let_.name);
                    let v = fbuilder.compile_node(&let_.value);
                    fbuilder.compiler.builder.store(v, to);
                }
            }
        }
        let ret = self.int(0);
        self.builder.ret(ret);
        self.builder.position_at_end(&entry);
        self.builder.br(&start);

        // compile main function
        self.compile_main_function(&program.main, init_fun);

        self.module
            .verify()
            .map_err(|err| Error::from(format!("{}", err)))
            .chain_err(|| {
                format!(
                    "failed to generate valid LLVM IR: {}",
                    self.module.to_string()
                )
            })?;
        Ok(&self.module)
    }

    fn getvar(&self, name: &str) -> Value {
        *self.globals.get(name).expect(&format!(
            "undefined variable: {}",
            name
        ))
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
        self.local_vars.get(name).cloned().unwrap_or_else(|| {
            self.compiler.getvar(name)
        })
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
        let f = self.compiler.module.get_function("gc_malloc").expect(
            "runtime function `gc_malloc` is undefined",
        );
        let void_ptr = self.compiler.builder.call(f, &[size_value], "malloctmp");
        self.compiler.builder.bitcast(
            void_ptr,
            typ.pointer_type(),
            "mallocptr",
        )
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
                let f = self.compiler.module.get_function(f).expect(
                    "function should be defined",
                );
                let args = args.iter()
                    .map(|arg| self.compile_node(arg))
                    .collect::<Vec<_>>();
                self.compiler.builder.call(f, &args, "calltmp")
            }
            NodeKind::Add(ref l, ref r) => {
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                if node.typ == Type::Int {
                    self.compiler.builder.add(l, r, "addtmp")
                } else {
                    self.compiler.builder.fadd(l, r, "addtmp")
                }
            }
            NodeKind::Sub(ref l, ref r) => {
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                if node.typ == Type::Int {
                    self.compiler.builder.sub(l, r, "subtmp")
                } else {
                    self.compiler.builder.fsub(l, r, "subtmp")
                }
            }
            NodeKind::Mul(ref l, ref r) => {
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                if node.typ == Type::Int {
                    self.compiler.builder.mul(l, r, "multmp")
                } else {
                    self.compiler.builder.fmul(l, r, "multmp")
                }
            }
            NodeKind::Div(ref l, ref r) => {
                let l = self.compile_node(l);
                let r = self.compile_node(r);
                if node.typ == Type::Int {
                    self.compiler.builder.div(l, r, "divtmp")
                } else {
                    self.compiler.builder.fdiv(l, r, "divtmp")
                }
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
                self.compiler.builder.cond_br(
                    cond_value,
                    &body_bb,
                    &exit_bb,
                );
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
                        let typ = self.compiler.compile_type(&let_.value.typ);
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
