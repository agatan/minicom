use std::fmt;

use vm::instr::Instruction;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Unit,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Int(n) => n.fmt(f),
            Value::Float(n) => n.fmt(f),
            Value::Unit => f.write_str("()"),
        }
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
}
