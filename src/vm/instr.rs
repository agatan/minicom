#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Instruction {
    PushInt(i64),
    AddInt,
    SubInt,
}
