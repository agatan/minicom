#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Instruction {
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

    SetLocal(u32),
    GetLocal(u32),
}
