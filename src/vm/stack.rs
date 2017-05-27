use std::ops::{Index, IndexMut};

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

impl Index<usize> for Stack {
    type Output = Value;

    fn index(&self, index: usize) -> &Value {
        self.values.index(index)
    }
}

impl IndexMut<usize> for Stack {
    fn index_mut(&mut self, index: usize) -> &mut Value {
        self.values.index_mut(index)
    }
}
