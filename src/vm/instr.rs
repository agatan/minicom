#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Instruction {
    PushUnit,
    Pop,

    PushInt(i64),
    AddInt,
    SubInt,
    MulInt,
    DivInt,

    PushFloat(f64),
    AddFloat,
    SubFloat,
    MulFloat,
    DivFloat,

    Not,
    Eq,
    LE,
    LT,

    Jump(i32),
    JumpIfZero(i32),

    Call { id: u32, n_args: u32 },
    Ret,

    Print,

    SetLocal(u32),
    GetLocal(u32),

    SetGlobal(u32),
    GetGlobal(u32),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub instrs: Vec<Instruction>,
    pub n_locals: u32,
}

impl Function {
    pub fn start_pc(&self) -> ProgramCounter {
        ProgramCounter::new(self.instrs.as_ptr())
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct ProgramCounter(*const Instruction);

impl ProgramCounter {
    pub fn null() -> Self {
        ProgramCounter::new(0 as *const Instruction)
    }

    pub fn new(ptr: *const Instruction) -> Self {
        ProgramCounter(ptr)
    }

    pub fn fetch(&self) -> Instruction {
        unsafe { *self.0 }
    }

    pub fn next(&mut self) {
        self.0 = unsafe { self.0.offset(1) }
    }

    pub fn jump(&mut self, offset: i32) {
        self.0 = unsafe { self.0.offset(offset as isize) }
    }
}
