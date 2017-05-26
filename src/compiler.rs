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
            NodeKind::Int(n) => self.push(Instruction::PushInt(n)),
            NodeKind::Float(n) => self.push(Instruction::PushFloat(n)),
            NodeKind::Ident(id) => self.push(Instruction::GetLocal(id.to_u32())),
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
                self.push(Instruction::SetLocal(let_.id.to_u32()));
            }
            NodeKind::Assign(id, ref value) => {
                self.compile_node(value);
                self.push(Instruction::SetLocal(id.to_u32()));
            }
        }
    }

    fn compile(&mut self, nodes: &[Node]) {
        for node in nodes {
            self.compile_node(node);
            self.push(Instruction::Pop);
        }
    }
}

pub fn compile(nodes: &[Node]) -> Vec<Instruction> {
    let mut c = Compiler::new();
    c.compile(nodes);
    c.instrs
}
