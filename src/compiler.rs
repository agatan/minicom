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
            NodeKind::Ident(ref lv_var) => {
                self.push(Instruction::GetLocal {
                    id: lv_var.value.index,
                    level: lv_var.level,
                })
            }
            NodeKind::Call(ref lv_var, ref args) => {
                for arg in args {
                    self.compile_node(arg);
                }
                self.push(Instruction::Call {
                    id: lv_var.value,
                    level: lv_var.level,
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
                    id: let_.id,
                    level: 0,
                });
            }
            NodeKind::Assign(ref lv_var, ref value) => {
                self.compile_node(value);
                self.push(Instruction::SetLocal {
                    id: lv_var.value.index,
                    level: lv_var.level,
                })
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
