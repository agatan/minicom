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

    fn compile_node(&mut self, node: &Node, is_root: bool) {
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
            NodeKind::GlobalIdent(ref var) => self.push(Instruction::GetGlobal(var.index)),
            NodeKind::Call(index, ref args) => {
                for arg in args {
                    self.compile_node(arg, is_root);
                }
                self.push(Instruction::Call {
                    id: index,
                    n_args: args.len() as u32,
                })
            }
            NodeKind::AddInt(ref l, ref r) => {
                self.compile_node(r, is_root);
                self.compile_node(l, is_root);
                self.push(Instruction::AddInt);
            }
            NodeKind::SubInt(ref l, ref r) => {
                self.compile_node(r, is_root);
                self.compile_node(l, is_root);
                self.push(Instruction::SubInt);
            }
            NodeKind::MulInt(ref l, ref r) => {
                self.compile_node(r, is_root);
                self.compile_node(l, is_root);
                self.push(Instruction::MulInt);
            }
            NodeKind::DivInt(ref l, ref r) => {
                self.compile_node(r, is_root);
                self.compile_node(l, is_root);
                self.push(Instruction::DivInt);
            }
            NodeKind::AddFloat(ref l, ref r) => {
                self.compile_node(r, is_root);
                self.compile_node(l, is_root);
                self.push(Instruction::AddFloat);
            }
            NodeKind::SubFloat(ref l, ref r) => {
                self.compile_node(r, is_root);
                self.compile_node(l, is_root);
                self.push(Instruction::SubFloat);
            }
            NodeKind::MulFloat(ref l, ref r) => {
                self.compile_node(r, is_root);
                self.compile_node(l, is_root);
                self.push(Instruction::MulFloat);
            }
            NodeKind::DivFloat(ref l, ref r) => {
                self.compile_node(r, is_root);
                self.compile_node(l, is_root);
                self.push(Instruction::DivFloat);
            }
            NodeKind::Print(ref e) => {
                self.compile_node(e, is_root);
                self.push(Instruction::Print);
            }
            NodeKind::Let(ref let_) => {
                self.compile_node(&let_.value, is_root);
                if is_root {
                    self.push(Instruction::SetGlobal(let_.id))
                } else {
                    self.push(Instruction::SetLocal {
                        id: let_.id,
                        level: 0,
                    })
                }
            }
            NodeKind::Assign(ref lv_var, ref value) => {
                self.compile_node(value, is_root);
                self.push(Instruction::SetLocal {
                    id: lv_var.value.index,
                    level: lv_var.level,
                })
            }
            NodeKind::AssignGlobal(ref var, ref value) => {
                self.compile_node(value, is_root);
                self.push(Instruction::SetGlobal(var.index))
            }
        }
    }

    fn compile(&mut self, nodes: &[Node]) {
        if let Some((last, init)) = nodes.split_last() {
            for node in init {
                self.compile_node(node, true);
                self.push(Instruction::Pop);
            }
            self.compile_node(last, true);
        }
    }
}

pub fn compile(nodes: &[Node]) -> Vec<Instruction> {
    let mut c = Compiler::new();
    c.compile(nodes);
    c.instrs
}
