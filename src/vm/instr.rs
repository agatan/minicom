#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Instruction {
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
}
