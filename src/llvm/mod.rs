use std::ffi::CString;
use std::rc::Rc;
use std::ops::Drop;
use std::fmt;

use llvm_sys::prelude::*;
use llvm_sys::core;
use llvm_sys::target;
use llvm_sys::execution_engine;
use llvm_sys::analysis;

pub mod engine;

pub fn init() {
    unsafe {
        target::LLVMInitializeX86TargetInfo();
        target::LLVMInitializeX86Target();
        target::LLVMInitializeX86TargetMC();
        target::LLVMInitializeX86AsmPrinter();
        target::LLVMInitializeX86AsmParser();
        target::LLVMInitializeX86Disassembler();
        execution_engine::LLVMLinkInMCJIT();
    }
}

#[derive(Debug)]
pub struct Message(*mut ::libc::c_char);

impl Message {
    pub fn get_mut_ptr(&mut self) -> *mut *mut ::libc::c_char {
        &mut self.0 as *mut *mut _
    }
}

impl fmt::Display for Message {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unsafe { CString::from_raw(self.0) }
            .to_string_lossy()
            .fmt(f)
    }
}

impl Drop for Message {
    fn drop(&mut self) {
        unsafe { core::LLVMDisposeMessage(self.0) };
    }
}

pub struct Context(LLVMContextRef);

impl Context {
    pub fn new() -> Rc<Context> {
        let llctx = unsafe { core::LLVMContextCreate() };
        Rc::new(Context(llctx))
    }

    pub fn get(&self) -> LLVMContextRef {
        self.0
    }

    pub fn unit_type(&self) -> Type {
        Type(unsafe { core::LLVMInt1TypeInContext(self.get()) })
    }

    pub fn int32_type(&self) -> Type {
        Type(unsafe { core::LLVMInt32TypeInContext(self.get()) })
    }

    pub fn float_type(&self) -> Type {
        Type(unsafe { core::LLVMFloatTypeInContext(self.get()) })
    }

    pub fn function_type(&self, ret: Type, params: &[Type], is_var_arg: bool) -> Type {
        let params = params.iter().map(|ty| ty.get()).collect::<Vec<_>>();
        let param_types = params.as_ptr() as *mut LLVMTypeRef;
        let param_count = params.len() as ::libc::c_uint;
        unsafe {
            let ty = core::LLVMFunctionType(ret.get(),
                                            param_types,
                                            param_count,
                                            if is_var_arg { 1 } else { 0 });
            Type(ty)
        }
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe {
            core::LLVMContextDispose(self.0);
        }
    }
}

pub struct Builder {
    _ctx: Rc<Context>,
    llbuilder: LLVMBuilderRef,
}

impl Builder {
    pub fn new(ctx: Rc<Context>) -> Builder {
        let llbuilder = unsafe { core::LLVMCreateBuilderInContext(ctx.get()) };
        Builder {
            _ctx: ctx,
            llbuilder: llbuilder,
        }
    }

    fn get(&self) -> LLVMBuilderRef {
        self.llbuilder
    }

    pub fn position_at_end(&mut self, bb: &BasicBlock) {
        unsafe { core::LLVMPositionBuilderAtEnd(self.get(), bb.get()) }
    }

    pub fn ret(&mut self, v: Value) -> Value {
        unsafe { Value(core::LLVMBuildRet(self.get(), v.to_value())) }
    }

    pub fn alloca(&mut self, ty: Type, name: &str) -> Value {
        let name = CString::new(name).unwrap();
        unsafe {
            let value = core::LLVMBuildAlloca(self.get(), ty.get(), name.as_ptr());
            Value(value)
        }
    }

    pub fn store(&mut self, v: Value, ptr: Value) -> Value {
        unsafe { Value(core::LLVMBuildStore(self.get(), v.to_value(), ptr.to_value())) }
    }

    pub fn load(&mut self, ptr: Value, name: &str) -> Value {
        let name = CString::new(name).unwrap();
        unsafe { Value(core::LLVMBuildLoad(self.get(), ptr.to_value(), name.as_ptr())) }
    }

    pub fn add(&mut self, a: Value, b: Value, name: &str) -> Value {
        let name = CString::new(name).unwrap();
        unsafe { Value(core::LLVMBuildAdd(self.get(), a.to_value(), b.to_value(), name.as_ptr())) }
    }
}

impl Drop for Builder {
    fn drop(&mut self) {
        unsafe {
            core::LLVMDisposeBuilder(self.llbuilder);
        }
    }
}

pub struct Module {
    _ctx: Rc<Context>,
    llmodule: LLVMModuleRef,
}

impl Module {
    pub fn new(ctx: Rc<Context>, name: &str) -> Module {
        let name = CString::new(name).unwrap();
        Module {
            llmodule: unsafe { core::LLVMModuleCreateWithNameInContext(name.as_ptr(), ctx.get()) },
            _ctx: ctx,
        }
    }

    pub fn get(&self) -> LLVMModuleRef {
        self.llmodule
    }

    pub fn add_function(&mut self, name: &str, fun_type: Type) -> Function {
        let name = CString::new(name).unwrap();
        let value = unsafe { core::LLVMAddFunction(self.get(), name.as_ptr(), fun_type.get()) };
        Function(value)
    }

    pub fn dump(&self) {
        unsafe { core::LLVMDumpModule(self.get()) }
    }

    pub fn verify(&self) -> Result<(), Message> {
        unsafe {
            let mut msg: Message = ::std::mem::uninitialized();
            let ret = analysis::LLVMVerifyModule(
                self.get(),
                analysis::LLVMVerifierFailureAction::LLVMReturnStatusAction,
                    msg.get_mut_ptr());
            if ret == 1 { Err(msg) } else { Ok(()) }
        }
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        unsafe {
            core::LLVMDisposeModule(self.llmodule);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Type(LLVMTypeRef);

impl Type {
    fn get(&self) -> LLVMTypeRef {
        self.0
    }

    pub fn const_int(&self, n: u64) -> Value {
        unsafe { Value(core::LLVMConstInt(self.get(), n, 0)) }
    }

    pub fn const_float(&self, f: f64) -> Value {
        Value(unsafe { core::LLVMConstReal(self.get(), f) })
    }
}

trait IsValue {
    fn to_value(&self) -> LLVMValueRef;
    fn dump(&self) {
        unsafe { core::LLVMDumpValue(self.to_value()) }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Value(LLVMValueRef);

impl IsValue for Value {
    fn to_value(&self) -> LLVMValueRef {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Function(LLVMValueRef);

impl IsValue for Function {
    fn to_value(&self) -> LLVMValueRef {
        self.0
    }
}

impl Function {
    pub fn get(&self) -> LLVMValueRef {
        self.0
    }

    pub fn append_basic_block(&mut self, name: &str) -> BasicBlock {
        unsafe {
            let module = core::LLVMGetGlobalParent(self.to_value());
            let ctx = core::LLVMGetModuleContext(module);
            let llbb = core::LLVMAppendBasicBlockInContext(ctx,
                                                           self.to_value(),
                                                           CString::new(name).unwrap().as_ptr());
            BasicBlock(llbb)
        }
    }
}

pub struct BasicBlock(LLVMBasicBlockRef);

impl BasicBlock {
    pub fn get(&self) -> LLVMBasicBlockRef {
        self.0
    }
}

pub fn run() {
    let context = Context::new();
    let mut module = Module::new(context.clone(), "main");
    let int_ty = context.int32_type();
    let fun_ty = context.function_type(int_ty, &[], false);
    let mut fun = module.add_function("main", fun_ty);
    let bb = fun.append_basic_block("entry");
    let mut builder = Builder::new(context.clone());
    builder.position_at_end(&bb);
    let a = builder.alloca(int_ty, "a");
    let b = builder.alloca(int_ty, "b");
    let x: i32 = -10000;
    let iv = context.int32_type().const_int(x as u64);
    builder.store(iv, a);
    builder.store(context.int32_type().const_int(11), b);
    let new_a = builder.load(a, "new_a");
    let new_b = builder.load(b, "new_b");
    let ret = builder.add(new_a, new_b, "add");
    builder.ret(ret);

    if let Err(msg) = module.verify() {
        panic!("{}", msg);
    }
    module.dump();

    init();
    let ee = engine::ExecutionEngine::create_jit_compiler(&module, 0).unwrap();
    println!("call main: {}", ee.run(fun).to_int());
}
