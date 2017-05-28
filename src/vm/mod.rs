use std::rc::Rc;

pub mod instr;
mod value;
mod stack;

pub use self::value::Value;
use self::instr::{Instruction, Function, ProgramCounter};
use self::instr::Instruction::*;

pub struct Machine {
    pc: ProgramCounter,
    end: ProgramCounter,
    stack: stack::Stack,
    global_vars: Vec<Value>,
    frames: Vec<Frame>,
    funcs: Vec<Rc<Function>>,
}

impl Machine {
    pub fn new() -> Self {
        Machine {
            pc: ProgramCounter::null(),
            end: ProgramCounter::null(),
            stack: stack::Stack::new(),
            global_vars: Vec::new(),
            frames: Vec::new(),
            funcs: Vec::new(),
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
            Not => {
                let v = self.stack.pop();
                if v == Value::Int(0) {
                    self.stack.push(Value::Int(1))
                } else {
                    self.stack.push(Value::Int(0))
                }
                self.pc.next();
            }
            Eq => {
                let v1 = self.stack.pop();
                let v2 = self.stack.pop();
                if v1 == v2 {
                    self.stack.push(Value::Int(1))
                } else {
                    self.stack.push(Value::Int(0))
                }
                self.pc.next();
            }
            LE => {
                let v1 = self.stack.pop();
                let v2 = self.stack.pop();
                if v1 <= v2 {
                    self.stack.push(Value::Int(1))
                } else {
                    self.stack.push(Value::Int(0))
                }
                self.pc.next();
            }
            LT => {
                let v1 = self.stack.pop();
                let v2 = self.stack.pop();
                if v1 < v2 {
                    self.stack.push(Value::Int(1))
                } else {
                    self.stack.push(Value::Int(0))
                }
                self.pc.next();
            }
            Jump(offset) => self.pc.jump(offset),
            JumpIfZero(offset) => {
                let v = self.stack.pop();
                if v == Value::Int(0) {
                    self.pc.jump(offset)
                } else {
                    self.pc.next()
                }
            }
            Call { id, n_args } => {
                let f = &self.funcs[id as usize];
                let fp = self.stack.len() - n_args as usize;
                let frame = Frame {
                    pc: self.pc,
                    fp: fp,
                };
                self.frames.push(frame);
                for _ in 0..(f.n_locals - n_args) {
                    self.stack.push(Value::Unit);
                }
                self.pc = f.start_pc();
            }
            Ret => {
                let v = self.stack.pop();
                let last_frame = self.frames.pop().expect("frames is not empty");
                self.pc = last_frame.pc;
                self.stack.set_len(last_frame.fp);
                self.stack.push(v);
                self.pc.next();
            }
            Print => {
                let v = self.stack.pop();
                println!("{}", v);
                self.stack.push(Value::Unit);
                self.pc.next();
            }
            SetLocal { id, level } => {
                let v = self.stack.pop();
                debug_assert_eq!(level, 0);
                let fp = self.current_frame().fp;
                self.stack[fp + id as usize] = v;
                self.stack.push(Value::Unit);
                self.pc.next();
            }
            GetLocal { id, level } => {
                debug_assert_eq!(level, 0);
                let fp = self.current_frame().fp;
                let v = self.stack[fp + id as usize];
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

    fn current_frame(&self) -> &Frame {
        self.frames.last().expect("frame is not empty")
    }

    fn is_finished(&self) -> bool {
        self.pc == self.end
    }

    pub fn run(&mut self, funcs: Vec<Rc<Function>>, instrs: &[Instruction]) -> Value {
        self.pc = ProgramCounter::new(instrs.as_ptr());
        self.end = unsafe { ProgramCounter::new(instrs.as_ptr().offset(instrs.len() as isize)) };
        self.frames.push(Frame {
            pc: self.pc,
            fp: self.stack.len(),
        });
        self.funcs = funcs;
        while !self.is_finished() {
            self.eval_instr();
        }
        self.stack.pop_optional().unwrap_or(Value::Unit)
    }
}

#[derive(Debug)]
struct Frame {
    pc: ProgramCounter,
    fp: usize,
}
