pub mod instr;
mod value;
mod stack;

pub use self::value::Value;
use self::instr::Instruction;
use self::instr::Instruction::*;
use self::value::ProgramCounter;

pub struct Machine {
    pc: ProgramCounter,
    end: ProgramCounter,
    stack: stack::Stack,
    vars: Vec<Value>,
    global_vars: Vec<Value>,
    frames: Vec<Frame>,
}

impl Machine {
    pub fn new() -> Self {
        Machine {
            pc: ProgramCounter::null(),
            end: ProgramCounter::null(),
            stack: stack::Stack::new(),
            vars: Vec::new(),
            global_vars: Vec::new(),
            frames: Vec::new(),
        }
    }

    fn eval_instr(&mut self) {
        let ins = self.pc.fetch();
        match ins {
            PushUnit => {
                self.stack.push(Value::Unit);
                self.pc.next();
            }
            Pop => {
                self.stack.pop();
                self.pc.next();
            }
            PushInt(n) => {
                let v = Value::Int(n);
                self.stack.push(v);
                self.pc.next();
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
                self.pc.next();
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
                self.pc.next();
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
                self.pc.next();
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
                self.pc.next();
            }
            PushFloat(n) => {
                let v = Value::Float(n);
                self.stack.push(v);
                self.pc.next();
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
                self.pc.next();
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
                self.pc.next();
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
                self.pc.next();
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
                self.pc.next();
            }
            Call { id, n_args } => unimplemented!(),
            Ret => unimplemented!(),
            Print => {
                let v = self.stack.pop();
                println!("{}", v);
                self.stack.push(Value::Unit);
                self.pc.next();
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
                self.pc.next();
            }
            GetLocal { id, level } => {
                debug_assert_eq!(level, 0);
                let v = self.vars[id as usize];
                self.stack.push(v);
                self.pc.next();
            }

            SetGlobal(n) => {
                let v = self.stack.pop();
                if self.global_vars.len() == n as usize {
                    self.global_vars.push(v);
                } else {
                    self.global_vars[n as usize] = v;
                }
                self.stack.push(Value::Unit);
                self.pc.next();
            }
            GetGlobal(n) => {
                let v = self.global_vars[n as usize];
                self.stack.push(v);
                self.pc.next();
            }
        }
    }

    fn is_finished(&self) -> bool {
        self.pc == self.end
    }

    pub fn run(&mut self, instrs: &[Instruction]) -> Value {
        self.pc = ProgramCounter::new(instrs.as_ptr());
        self.end = unsafe { ProgramCounter::new(instrs.as_ptr().offset(instrs.len() as isize)) };
        self.frames.push(Frame {
            pc: self.pc,
            sp: self.stack.len(),
        });
        while !self.is_finished() {
            self.eval_instr();
        }
        self.stack.pop_optional().unwrap_or(Value::Unit)
    }
}

#[derive(Debug)]
struct Frame {
    pc: ProgramCounter,
    sp: usize,
}
