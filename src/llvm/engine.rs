use std::ops::Drop;

use llvm_sys::execution_engine as ee;

use super::{Module, Function, Message};

pub struct ExecutionEngine(ee::LLVMExecutionEngineRef);

impl ExecutionEngine {
    pub fn create_jit_compiler(module: &Module,
                               opt_level: ::libc::c_uint)
                               -> Result<Self, Message> {
        unsafe {
            let mut msg: Message = ::std::mem::uninitialized();
            let mut engine: ee::LLVMExecutionEngineRef = ::std::mem::uninitialized();
            let ret = ee::LLVMCreateJITCompilerForModule(&mut engine as
                                                         *mut ee::LLVMExecutionEngineRef,
                                                         module.get(),
                                                         opt_level,
                                                         msg.get_mut_ptr());
            if ret != 0 {
                return Err(msg);
            }
            Ok(ExecutionEngine(engine))
        }
    }

    pub fn run(&self, f: Function) -> GenericValue {
        let ret = unsafe { ee::LLVMRunFunction(self.get(), f.get(), 0, ::std::ptr::null_mut()) };
        GenericValue(ret)
    }

    pub fn get(&self) -> ee::LLVMExecutionEngineRef {
        self.0
    }
}

pub struct GenericValue(ee::LLVMGenericValueRef);

impl GenericValue {
    pub fn to_int(&self) -> ::libc::c_ulonglong {
        unsafe { ee::LLVMGenericValueToInt(self.0, 0) }
    }
}

impl Drop for GenericValue {
    fn drop(&mut self) {
        unsafe { ee::LLVMDisposeGenericValue(self.0) }
    }
}
