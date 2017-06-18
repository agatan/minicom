use llvm_sys::target_machine::{self, LLVMTargetRef, LLVMCodeGenOptLevel, LLVMRelocMode,
                               LLVMCodeModel};
use llvm_sys::target;

use super::Message;

#[derive(Clone, Copy)]
pub struct Target(LLVMTargetRef);

impl Target {
    pub fn new() -> Result<Self, Message> {
        let triple = get_default_target_triple();
        get_target_from_triple(triple).map(|t| Target(t))
    }

    pub fn from_triple(triple: *mut ::libc::c_char) -> Result<Self, Message> {
        get_target_from_triple(triple).map(|t| Target(t))
    }

    pub fn get(&self) -> LLVMTargetRef {
        self.0
    }
}

#[derive(Clone, Copy)]
pub struct TargetMachine(target_machine::LLVMTargetMachineRef);

impl TargetMachine {
    pub fn new(triple: *mut ::libc::c_char, target: Target) -> TargetMachine {
        TargetMachine(unsafe {
            target_machine::LLVMCreateTargetMachine(target.get(),
                                                    triple,
                                                    "".as_ptr() as *const _,
                                                    "".as_ptr() as *const _,
                                                    LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
                                                    LLVMRelocMode::LLVMRelocDefault,
                                                    LLVMCodeModel::LLVMCodeModelDefault)
        })
    }

    pub fn get(&self) -> target_machine::LLVMTargetMachineRef {
        self.0
    }
}

#[derive(Clone, Copy)]
pub struct TargetData(target::LLVMTargetDataRef);

pub fn get_default_target_triple() -> *mut ::libc::c_char {
    unsafe { target_machine::LLVMGetDefaultTargetTriple() }
}

pub fn get_target_from_triple(triple: *mut ::libc::c_char) -> Result<LLVMTargetRef, Message> {
    unsafe {
        let mut target: LLVMTargetRef = ::std::mem::uninitialized();
        let mut err: Message = Message::with_null();
        let failed = target_machine::LLVMGetTargetFromTriple(triple,
                                                             &mut target as *mut _,
                                                             err.get_mut_ptr());
        if failed == 0 { Ok(target) } else { Err(err) }
    }
}
