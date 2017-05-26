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

    pub fn compile_node(&mut self, node: &Node) {}

    pub fn compile(&mut self, nodes: &[Node]) {}
}
