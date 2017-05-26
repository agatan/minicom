use sem::ir::*;
use vm::instr::Instruction;

#[derive(Debug)]
pub struct Compiler {
    instrs: Vec<Instruction>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler { instrs: Vec::new() }
    }

    fn push(&mut self, instr: Instruction) {
        self.instrs.push(instr)
    }

    pub fn compile_node(&mut self, node: &Node) {
        match node.kind {
            NodeKind::Int(n) => self.push(Instruction::PushInt(n)),
            NodeKind::Float(n) => self.push(Instruction::PushFloat(n)),
            NodeKind::Print(ref e) => {
                self.compile_node(e);
                self.push(Instruction::Print);
            }
            _ => unimplemented!(),
        }
    }

    pub fn compile(&mut self, nodes: &[Node]) {
        for node in nodes {
            self.compile_node(node);
            self.push(Instruction::Pop);
        }
    }
}
