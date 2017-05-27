use sem::ir::*;
use vm::instr::Instruction;

#[derive(Debug)]
struct Compiler {
    instrs: Vec<Instruction>,
}

impl Compiler {
    fn new() -> Compiler {
        Compiler { instrs: Vec::new() }
    }

    fn push(&mut self, instr: Instruction) {
        self.instrs.push(instr)
    }

    fn compile_node(&mut self, node: &Node) {
        match node.kind {
            NodeKind::Unit => self.push(Instruction::PushUnit),
            NodeKind::Int(n) => self.push(Instruction::PushInt(n)),
            NodeKind::Float(n) => self.push(Instruction::PushFloat(n)),
            NodeKind::Ident(ref var, level) => {
                let instr = match *var {
                    Var::Arg(n, _) => Instruction::GetLocalArg(n),
                    Var::Local(id, _) => {
                        Instruction::GetLocal {
                            id: id.to_u32(),
                            level: level,
                        }
                    }
                };
                self.push(instr)
            }
            NodeKind::Call(id, ref args) => {
                for arg in args {
                    self.compile_node(arg);
                }
                self.push(Instruction::Call {
                    id: id.to_u32(),
                    n_args: args.len() as u32,
                })
            }
            NodeKind::AddInt(ref l, ref r) => {
                self.compile_node(r);
                self.compile_node(l);
                self.push(Instruction::AddInt);
            }
            NodeKind::SubInt(ref l, ref r) => {
                self.compile_node(r);
                self.compile_node(l);
                self.push(Instruction::SubInt);
            }
            NodeKind::MulInt(ref l, ref r) => {
                self.compile_node(r);
                self.compile_node(l);
                self.push(Instruction::MulInt);
            }
            NodeKind::DivInt(ref l, ref r) => {
                self.compile_node(r);
                self.compile_node(l);
                self.push(Instruction::DivInt);
            }
            NodeKind::AddFloat(ref l, ref r) => {
                self.compile_node(r);
                self.compile_node(l);
                self.push(Instruction::AddFloat);
            }
            NodeKind::SubFloat(ref l, ref r) => {
                self.compile_node(r);
                self.compile_node(l);
                self.push(Instruction::SubFloat);
            }
            NodeKind::MulFloat(ref l, ref r) => {
                self.compile_node(r);
                self.compile_node(l);
                self.push(Instruction::MulFloat);
            }
            NodeKind::DivFloat(ref l, ref r) => {
                self.compile_node(r);
                self.compile_node(l);
                self.push(Instruction::DivFloat);
            }
            NodeKind::Print(ref e) => {
                self.compile_node(e);
                self.push(Instruction::Print);
            }
            NodeKind::Let(ref let_) => {
                self.compile_node(&let_.value);
                self.push(Instruction::SetLocal {
                    id: let_.id.to_u32(),
                    level: 0,
                });
            }
            NodeKind::Assign(ref var, level, ref value) => {
                self.compile_node(value);
                let instr = match *var {
                    Var::Arg(n, _) => Instruction::SetLocalArg(n),
                    Var::Local(id, _) => {
                        Instruction::SetLocal {
                            id: id.to_u32(),
                            level: level,
                        }
                    }
                };
                self.push(instr)
            }
        }
    }

    fn compile(&mut self, nodes: &[Node]) {
        if let Some((last, init)) = nodes.split_last() {
            for node in init {
                self.compile_node(node);
                self.push(Instruction::Pop);
            }
            self.compile_node(last);
        }
    }
}

pub fn compile(nodes: &[Node]) -> Vec<Instruction> {
    let mut c = Compiler::new();
    c.compile(nodes);
    c.instrs
}
