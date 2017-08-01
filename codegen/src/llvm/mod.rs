use std::ffi::{CStr, CString};
use std::rc::Rc;
use std::ops::Drop;
use std::fmt;

use llvm_sys::prelude::*;
use llvm_sys::core;
use llvm_sys::target as lltarget;
use llvm_sys::target_machine::{self, LLVMCodeGenFileType};
use llvm_sys::analysis;

pub use llvm_sys::LLVMIntPredicate;
pub use llvm_sys::LLVMIntPredicate::*;
pub use llvm_sys::LLVMRealPredicate;
pub use llvm_sys::LLVMRealPredicate::*;

pub mod target;

pub fn init() {
    unsafe {
        lltarget::LLVMInitializeX86TargetInfo();
        lltarget::LLVMInitializeX86Target();
        lltarget::LLVMInitializeX86TargetMC();
        lltarget::LLVMInitializeX86AsmPrinter();
        lltarget::LLVMInitializeX86AsmParser();
        lltarget::LLVMInitializeX86Disassembler();
    }
}

pub struct Message(*mut ::libc::c_char);

impl Message {
    pub fn with_null() -> Self {
        Message(::std::ptr::null_mut())
    }

    pub fn get_mut_ptr(&mut self) -> *mut *mut ::libc::c_char {
        &mut self.0 as *mut *mut _
    }
}

impl fmt::Debug for Message {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.0.is_null() {
            f.write_str("(null)")
        } else {
            write!(f, "Message({:?})", unsafe { CStr::from_ptr(self.0) })
        }
    }
}

impl fmt::Display for Message {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.0.is_null() {
            return f.write_str("(null)");
        }
        unsafe { CStr::from_ptr(self.0) }.to_string_lossy().fmt(f)
    }
}

impl ::std::error::Error for Message {
    fn description(&self) -> &str {
        "llvm error"
    }
}

impl Drop for Message {
    fn drop(&mut self) {
        if !self.0.is_null() {
            unsafe { core::LLVMDisposeMessage(self.0) };
        }
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

    pub fn void_type(&self) -> Type {
        Type(unsafe { core::LLVMVoidTypeInContext(self.get()) })
    }

    pub fn unit_type(&self) -> Type {
        Type(unsafe { core::LLVMInt1TypeInContext(self.get()) })
    }

    pub fn void_ptr_type(&self) -> Type {
        Type(unsafe {
            core::LLVMPointerType(core::LLVMInt8TypeInContext(self.get()), 0)
        })
    }

    pub fn bool_type(&self) -> Type {
        Type(unsafe { core::LLVMInt1TypeInContext(self.get()) })
    }

    pub fn int32_type(&self) -> Type {
        Type(unsafe { core::LLVMInt32TypeInContext(self.get()) })
    }

    pub fn float_type(&self) -> Type {
        Type(unsafe { core::LLVMDoubleTypeInContext(self.get()) })
    }

    pub fn function_type(&self, ret: Type, params: &[Type], is_var_arg: bool) -> Type {
        let params = params.iter().map(|ty| ty.get()).collect::<Vec<_>>();
        let param_types = params.as_ptr() as *mut LLVMTypeRef;
        let param_count = params.len() as ::libc::c_uint;
        unsafe {
            let ty = core::LLVMFunctionType(
                ret.get(),
                param_types,
                param_count,
                if is_var_arg { 1 } else { 0 },
            );
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

    pub fn get_insert_block(&self) -> BasicBlock {
        unsafe { BasicBlock(core::LLVMGetInsertBlock(self.get())) }
    }

    pub fn position_at_end(&mut self, bb: &BasicBlock) {
        unsafe { core::LLVMPositionBuilderAtEnd(self.get(), bb.get()) }
    }

    pub fn br(&mut self, bb: &BasicBlock) -> Value {
        unsafe { Value(core::LLVMBuildBr(self.get(), bb.get())) }
    }

    pub fn cond_br(&mut self, cond: Value, then: &BasicBlock, els: &BasicBlock) -> Value {
        unsafe {
            Value(core::LLVMBuildCondBr(
                self.get(),
                cond.get(),
                then.get(),
                els.get(),
            ))
        }
    }

    pub fn phi(&mut self, typ: Type, name: &str) -> Value {
        let name = CString::new(name).unwrap();
        unsafe { Value(core::LLVMBuildPhi(self.get(), typ.get(), name.as_ptr())) }
    }

    pub fn ret(&mut self, v: Value) -> Value {
        unsafe { Value(core::LLVMBuildRet(self.get(), v.to_value())) }
    }

    pub fn call(&mut self, f: Function, args: &[Value], name: &str) -> Value {
        let name = CString::new(name).unwrap();
        let num_args = args.len() as ::libc::c_uint;
        let args = args.as_ptr() as *mut _;
        unsafe {
            Value(core::LLVMBuildCall(
                self.get(),
                f.get(),
                args,
                num_args,
                name.as_ptr(),
            ))
        }
    }

    pub fn alloca(&mut self, ty: Type, name: &str) -> Value {
        let name = CString::new(name).unwrap();
        unsafe {
            let value = core::LLVMBuildAlloca(self.get(), ty.get(), name.as_ptr());
            Value(value)
        }
    }

    pub fn store(&mut self, v: Value, ptr: Value) -> Value {
        unsafe {
            Value(core::LLVMBuildStore(
                self.get(),
                v.to_value(),
                ptr.to_value(),
            ))
        }
    }

    pub fn load(&mut self, ptr: Value, name: &str) -> Value {
        let name = CString::new(name).unwrap();
        unsafe {
            Value(core::LLVMBuildLoad(
                self.get(),
                ptr.to_value(),
                name.as_ptr(),
            ))
        }
    }

    pub fn bitcast(&mut self, ptr: Value, typ: Type, name: &str) -> Value {
        let name = CString::new(name).unwrap();
        unsafe {
            Value(core::LLVMBuildBitCast(
                self.get(),
                ptr.get(),
                typ.get(),
                name.as_ptr(),
            ))
        }
    }

    pub fn add(&mut self, a: Value, b: Value, name: &str) -> Value {
        let name = CString::new(name).unwrap();
        unsafe {
            Value(core::LLVMBuildAdd(
                self.get(),
                a.to_value(),
                b.to_value(),
                name.as_ptr(),
            ))
        }
    }

    pub fn sub(&mut self, a: Value, b: Value, name: &str) -> Value {
        let name = CString::new(name).unwrap();
        unsafe {
            Value(core::LLVMBuildSub(
                self.get(),
                a.to_value(),
                b.to_value(),
                name.as_ptr(),
            ))
        }
    }

    pub fn mul(&mut self, a: Value, b: Value, name: &str) -> Value {
        let name = CString::new(name).unwrap();
        unsafe {
            Value(core::LLVMBuildMul(
                self.get(),
                a.to_value(),
                b.to_value(),
                name.as_ptr(),
            ))
        }
    }

    pub fn div(&mut self, a: Value, b: Value, name: &str) -> Value {
        let name = CString::new(name).unwrap();
        unsafe {
            Value(core::LLVMBuildSDiv(
                self.get(),
                a.to_value(),
                b.to_value(),
                name.as_ptr(),
            ))
        }
    }

    pub fn fadd(&mut self, a: Value, b: Value, name: &str) -> Value {
        let name = CString::new(name).unwrap();
        unsafe {
            Value(core::LLVMBuildFAdd(
                self.get(),
                a.to_value(),
                b.to_value(),
                name.as_ptr(),
            ))
        }
    }

    pub fn fsub(&mut self, a: Value, b: Value, name: &str) -> Value {
        let name = CString::new(name).unwrap();
        unsafe {
            Value(core::LLVMBuildFSub(
                self.get(),
                a.to_value(),
                b.to_value(),
                name.as_ptr(),
            ))
        }
    }

    pub fn fmul(&mut self, a: Value, b: Value, name: &str) -> Value {
        let name = CString::new(name).unwrap();
        unsafe {
            Value(core::LLVMBuildFMul(
                self.get(),
                a.to_value(),
                b.to_value(),
                name.as_ptr(),
            ))
        }
    }

    pub fn fdiv(&mut self, a: Value, b: Value, name: &str) -> Value {
        let name = CString::new(name).unwrap();
        unsafe {
            Value(core::LLVMBuildFDiv(
                self.get(),
                a.to_value(),
                b.to_value(),
                name.as_ptr(),
            ))
        }
    }

    pub fn not(&mut self, a: Value, name: &str) -> Value {
        let name = CString::new(name).unwrap();
        unsafe { Value(core::LLVMBuildNot(self.get(), a.to_value(), name.as_ptr())) }
    }

    pub fn icmp(&mut self, op: LLVMIntPredicate, a: Value, b: Value, name: &str) -> Value {
        let name = CString::new(name).unwrap();
        unsafe {
            Value(core::LLVMBuildICmp(
                self.get(),
                op,
                a.to_value(),
                b.to_value(),
                name.as_ptr(),
            ))
        }
    }

    pub fn fcmp(&mut self, op: LLVMRealPredicate, a: Value, b: Value, name: &str) -> Value {
        let name = CString::new(name).unwrap();
        unsafe {
            Value(core::LLVMBuildFCmp(
                self.get(),
                op,
                a.to_value(),
                b.to_value(),
                name.as_ptr(),
            ))
        }
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

    pub fn get_function(&self, name: &str) -> Option<Function> {
        let name = CString::new(name).unwrap();
        let f = unsafe { core::LLVMGetNamedFunction(self.get(), name.as_ptr()) };
        if f.is_null() { None } else { Some(Function(f)) }
    }

    pub fn add_global(&mut self, name: &str, ty: Type) -> Value {
        let name = CString::new(name).unwrap();
        let value = unsafe { core::LLVMAddGlobal(self.get(), ty.get(), name.as_ptr()) };
        Value(value)
    }

    pub fn get_global(&mut self, name: &str) -> Value {
        let name = CString::new(name).unwrap();
        let value = unsafe { core::LLVMGetNamedGlobal(self.get(), name.as_ptr()) };
        Value(value)
    }

    pub fn verify(&self) -> Result<(), String> {
        unsafe {
            let mut msg: Message = Message::with_null();
            let ret = analysis::LLVMVerifyModule(
                self.get(),
                analysis::LLVMVerifierFailureAction::LLVMReturnStatusAction,
                msg.get_mut_ptr(),
            );
            if ret == 1 {
                Err(msg.to_string())
            } else {
                Ok(())
            }
        }
    }

    pub fn emit_object(&self, machine: target::TargetMachine) -> Result<Vec<u8>, Message> {
        let mut err: Message = Message::with_null();
        let mut membuf: LLVMMemoryBufferRef = unsafe { ::std::mem::uninitialized() };
        let failed = unsafe {
            target_machine::LLVMTargetMachineEmitToMemoryBuffer(
                machine.get(),
                self.get(),
                LLVMCodeGenFileType::LLVMObjectFile,
                err.get_mut_ptr(),
                &mut membuf as *mut _,
            )
        };
        if failed == 1 {
            return Err(err);
        }
        let result = unsafe {
            let start = core::LLVMGetBufferStart(membuf) as *const u8;
            let size = core::LLVMGetBufferSize(membuf);
            ::std::slice::from_raw_parts(start, size).to_vec()
        };
        Ok(result)
    }
}

impl ::std::string::ToString for Module {
    fn to_string(&self) -> String {
        unsafe {
            let s = core::LLVMPrintModuleToString(self.get());
            if s.is_null() {
                panic!("failed to stringize llvm module")
            }
            CStr::from_ptr(s).to_string_lossy().to_string()
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

    pub fn const_null(&self) -> Value {
        Value(unsafe { core::LLVMConstNull(self.get()) })
    }

    pub fn pointer_type(&self) -> Type {
        Type(unsafe { core::LLVMPointerType(self.get(), 0) })
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

impl Value {
    pub fn get(&self) -> LLVMValueRef {
        self.0
    }

    pub fn set_initializer(&self, init: Value) {
        unsafe { core::LLVMSetInitializer(self.get(), init.get()) }
    }

    pub fn get_type(&self) -> Type {
        unsafe { Type(core::LLVMTypeOf(self.get())) }
    }

    pub fn add_incoming(&mut self, incomings: &[(BasicBlock, Value)]) {
        let blocks = incomings.iter().map(|x| x.0.get()).collect::<Vec<_>>();
        let values = incomings.iter().map(|x| x.1.get()).collect::<Vec<_>>();
        let count = incomings.len() as ::libc::c_uint;
        unsafe {
            core::LLVMAddIncoming(
                self.get(),
                values.as_ptr() as *mut _,
                blocks.as_ptr() as *mut _,
                count,
            )
        }
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

    pub fn param(&self, i: i32) -> Value {
        Value(unsafe {
            core::LLVMGetParam(self.get(), i as ::libc::c_uint)
        })
    }

    pub fn append_basic_block(&mut self, name: &str) -> BasicBlock {
        unsafe {
            let module = core::LLVMGetGlobalParent(self.to_value());
            let ctx = core::LLVMGetModuleContext(module);
            let llbb = core::LLVMAppendBasicBlockInContext(
                ctx,
                self.to_value(),
                CString::new(name).unwrap().as_ptr(),
            );
            BasicBlock(llbb)
        }
    }
}

pub struct BasicBlock(LLVMBasicBlockRef);

impl BasicBlock {
    pub fn get(&self) -> LLVMBasicBlockRef {
        self.0
    }

    pub fn parent(&self) -> Function {
        Function(unsafe { core::LLVMGetBasicBlockParent(self.get()) })
    }
}
