pub mod instr;
pub mod value;
pub mod stack;
pub use self::value::Value;
use self::instr::Instruction;
use self::instr::Instruction::*;

pub struct Machine {
    pc: usize,
    stack: stack::Stack,
    vars: Vec<Value>,
    global_vars: Vec<Value>,
}

impl Machine {
    pub fn new() -> Self {
        Machine {
            pc: 0,
            stack: stack::Stack::new(),
            vars: Vec::new(),
            global_vars: Vec::new(),
        }
    }

    fn fetch_instr(&mut self, instrs: &[Instruction]) -> Instruction {
        let ins = instrs[self.pc];
        debug!("instruction: {:?}", ins);
        self.pc += 1;
        ins
    }

    fn eval_instr(&mut self, ins: Instruction) {
        match ins {
            PushUnit => self.stack.push(Value::Unit),
            Pop => {
                self.stack.pop();
            }
            PushInt(n) => {
                let v = Value::Int(n);
                self.stack.push(v);
            }
            AddInt => {
                let v1 = self.stack.pop();
                let v2 = self.stack.pop();
                match (v1, v2) {
                    (Value::Int(v1), Value::Int(v2)) => {
                        let v = Value::Int(v1 + v2);
                        self.stack.push(v);
                    }
                    _ => panic!("typed expression should be always valid"),
                }
            }
            SubInt => {
                let v1 = self.stack.pop();
                let v2 = self.stack.pop();
                match (v1, v2) {
                    (Value::Int(v1), Value::Int(v2)) => {
                        let v = Value::Int(v1 - v2);
                        self.stack.push(v);
                    }
                    _ => panic!("typed expression should be always valid"),
                }
            }
            MulInt => {
                let v1 = self.stack.pop();
                let v2 = self.stack.pop();
                match (v1, v2) {
                    (Value::Int(v1), Value::Int(v2)) => {
                        let v = Value::Int(v1 * v2);
                        self.stack.push(v);
                    }
                    _ => panic!("typed expression should be always valid"),
                }
            }
            DivInt => {
                let v1 = self.stack.pop();
                let v2 = self.stack.pop();
                match (v1, v2) {
                    (Value::Int(v1), Value::Int(v2)) => {
                        let v = Value::Int(v1 / v2);
                        self.stack.push(v);
                    }
                    _ => panic!("typed expression should be always valid"),
                }
            }
            PushFloat(n) => {
                let v = Value::Float(n);
                self.stack.push(v);
            }
            AddFloat => {
                let v1 = self.stack.pop();
                let v2 = self.stack.pop();
                match (v1, v2) {
                    (Value::Float(v1), Value::Float(v2)) => {
                        let v = Value::Float(v1 + v2);
                        self.stack.push(v);
                    }
                    _ => panic!("typed expression should be always valid"),
                }
            }
            SubFloat => {
                let v1 = self.stack.pop();
                let v2 = self.stack.pop();
                match (v1, v2) {
                    (Value::Float(v1), Value::Float(v2)) => {
                        let v = Value::Float(v1 - v2);
                        self.stack.push(v);
                    }
                    _ => panic!("typed expression should be always valid"),
                }
            }
            MulFloat => {
                let v1 = self.stack.pop();
                let v2 = self.stack.pop();
                match (v1, v2) {
                    (Value::Float(v1), Value::Float(v2)) => {
                        let v = Value::Float(v1 * v2);
                        self.stack.push(v);
                    }
                    _ => panic!("typed expression should be always valid"),
                }
            }
            DivFloat => {
                let v1 = self.stack.pop();
                let v2 = self.stack.pop();
                match (v1, v2) {
                    (Value::Float(v1), Value::Float(v2)) => {
                        let v = Value::Float(v1 / v2);
                        self.stack.push(v);
                    }
                    _ => panic!("typed expression should be always valid"),
                }
            }
            Call { id, level, n_args } => unimplemented!(),
            Print => {
                let v = self.stack.pop();
                println!("{}", v);
                self.stack.push(Value::Unit);
            }
            SetLocal { id, level } => {
                let v = self.stack.pop();
                debug_assert!(id as usize <= self.vars.len(),
                              "id: {}, vars: {:?}",
                              id,
                              self.vars);
                debug_assert_eq!(level, 0);
                if self.vars.len() == id as usize {
                    self.vars.push(v);
                } else {
                    self.vars[id as usize] = v;
                }
                self.stack.push(Value::Unit);
            }
            GetLocal { id, level } => {
                debug_assert_eq!(level, 0);
                let v = self.vars[id as usize];
                self.stack.push(v);
            }

            SetGlobal(n) => {
                let v = self.stack.pop();
                if self.global_vars.len() == n as usize {
                    self.global_vars.push(v);
                } else {
                    self.global_vars[n as usize] = v;
                }
                self.stack.push(Value::Unit)
            }
            GetGlobal(n) => {
                let v = self.global_vars[n as usize];
                self.stack.push(v);
            }
        }
    }

    fn is_finished(&self, instrs: &[Instruction]) -> bool {
        self.pc == instrs.len()
    }

    pub fn run(&mut self, instrs: &[Instruction]) -> Value {
        self.pc = 0;
        while !self.is_finished(instrs) {
            let ins = self.fetch_instr(instrs);
            self.eval_instr(ins);
        }
        self.stack.pop_optional().unwrap_or(Value::Unit)
    }
}
