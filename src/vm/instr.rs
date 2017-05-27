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

    Call { id: u32, n_args: u32 },
    Ret,

    Print,

    SetLocal { id: u32, level: u32 },
    GetLocal { id: u32, level: u32 },

    SetGlobal(u32),
    GetGlobal(u32),
}
