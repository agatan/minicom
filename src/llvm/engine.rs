use std::ffi::CString;
use std::ops::Drop;

use llvm_sys::execution_engine as ee;
use llvm_sys::core;

use super::{Module, Function};

pub struct ExecutionEngine(ee::LLVMExecutionEngineRef);

impl ExecutionEngine {
    pub fn create_jit_compiler(module: &Module, opt_level: ::libc::c_uint) -> Result<Self, String> {
        unsafe {
            let mut error: *mut ::libc::c_char = ::std::ptr::null_mut();
            let mut engine: ee::LLVMExecutionEngineRef = ::std::mem::uninitialized();
            let ret = ee::LLVMCreateJITCompilerForModule(&mut engine as
                                                         *mut ee::LLVMExecutionEngineRef,
                                                         module.get(),
                                                         opt_level,
                                                         &mut error as *mut *mut ::libc::c_char);
            if ret != 0 {
                let msg = CString::from_raw(error);
                core::LLVMDisposeMessage(error);
                return Err(msg.into_string().unwrap());
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
