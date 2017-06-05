use llvm_sys::target_machine::{self, LLVMTargetRef};

use super::Message;

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
