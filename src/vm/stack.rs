use vm::value::Value;

#[derive(Debug)]
pub struct Stack {
    values: Vec<Value>,
}

impl Stack {
    pub fn new() -> Self {
        Stack { values: Vec::new() }
    }

    pub fn push(&mut self, value: Value) {
        self.values.push(value)
    }

    pub fn pop(&mut self) -> Value {
        self.values.pop().expect("pop on empty stack")
    }

    pub fn pop_optional(&mut self) -> Option<Value> {
        self.values.pop()
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }
}
