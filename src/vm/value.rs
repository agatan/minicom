use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Value {
    Int(i32),
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
