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

    Print,

    SetLocal { id: u32, level: u32 },
    GetLocal { id: u32, level: u32 },
}
