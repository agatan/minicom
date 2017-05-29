use std::collections::HashMap;
use std::rc::Rc;
use sem::ir::*;
use vm::instr::{self, Instruction};

#[derive(Debug)]
pub struct Compiler {
    functions: HashMap<u32, Rc<instr::Function>>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler { functions: HashMap::new() }
    }

    pub fn compile(&mut self, rootenv: &LocalEnv, nodes: &[Node]) -> Vec<Instruction> {
        self.compile_root(rootenv);
        self.compile_nodes(nodes, true)
    }

    pub fn funcs(&self) -> Vec<Rc<instr::Function>> {
        let mut vec = Vec::new();
        for (&k, f) in self.functions.iter() {
            if vec.len() < 1 + k as usize {
                vec.resize(k as usize + 1, f.clone());
            }
            vec[k as usize] = f.clone();
        }
        vec
    }

    fn compile_node(&mut self, instrs: &mut Vec<Instruction>, node: &Node, is_root: bool) {
        match node.kind {
            NodeKind::Unit => instrs.push(Instruction::PushUnit),
            NodeKind::Int(n) => instrs.push(Instruction::PushInt(n)),
            NodeKind::Float(n) => instrs.push(Instruction::PushFloat(n)),
            NodeKind::Bool(b) => instrs.push(Instruction::PushInt(if b { 1 } else { 0 })),
            NodeKind::Ident(ref lv_var) => instrs.push(Instruction::GetLocal(lv_var.index)),
            NodeKind::GlobalIdent(ref var) => instrs.push(Instruction::GetGlobal(var.index)),
            NodeKind::Call(index, ref args) => {
                for arg in args {
                    self.compile_node(instrs, arg, is_root);
                }
                instrs.push(Instruction::Call {
                                id: index,
                                n_args: args.len() as u32,
                            })
            }
            NodeKind::AddInt(ref l, ref r) => {
                self.compile_node(instrs, r, is_root);
                self.compile_node(instrs, l, is_root);
                instrs.push(Instruction::AddInt);
            }
            NodeKind::SubInt(ref l, ref r) => {
                self.compile_node(instrs, r, is_root);
                self.compile_node(instrs, l, is_root);
                instrs.push(Instruction::SubInt);
            }
            NodeKind::MulInt(ref l, ref r) => {
                self.compile_node(instrs, r, is_root);
                self.compile_node(instrs, l, is_root);
                instrs.push(Instruction::MulInt);
            }
            NodeKind::DivInt(ref l, ref r) => {
                self.compile_node(instrs, r, is_root);
                self.compile_node(instrs, l, is_root);
                instrs.push(Instruction::DivInt);
            }
            NodeKind::AddFloat(ref l, ref r) => {
                self.compile_node(instrs, r, is_root);
                self.compile_node(instrs, l, is_root);
                instrs.push(Instruction::AddFloat);
            }
            NodeKind::SubFloat(ref l, ref r) => {
                self.compile_node(instrs, r, is_root);
                self.compile_node(instrs, l, is_root);
                instrs.push(Instruction::SubFloat);
            }
            NodeKind::MulFloat(ref l, ref r) => {
                self.compile_node(instrs, r, is_root);
                self.compile_node(instrs, l, is_root);
                instrs.push(Instruction::MulFloat);
            }
            NodeKind::DivFloat(ref l, ref r) => {
                self.compile_node(instrs, r, is_root);
                self.compile_node(instrs, l, is_root);
                instrs.push(Instruction::DivFloat);
            }
            NodeKind::Not(ref e) => {
                self.compile_node(instrs, e, is_root);
                instrs.push(Instruction::Not);
            }
            NodeKind::Eq(ref l, ref r) => {
                self.compile_node(instrs, r, is_root);
                self.compile_node(instrs, l, is_root);
                instrs.push(Instruction::Eq);
            }
            NodeKind::LE(ref l, ref r) => {
                self.compile_node(instrs, r, is_root);
                self.compile_node(instrs, l, is_root);
                instrs.push(Instruction::LE);
            }
            NodeKind::LT(ref l, ref r) => {
                self.compile_node(instrs, r, is_root);
                self.compile_node(instrs, l, is_root);
                instrs.push(Instruction::LT);
            }
            NodeKind::Print(ref e) => {
                self.compile_node(instrs, e, is_root);
                instrs.push(Instruction::Print);
            }
            NodeKind::Block(ref nodes) => {
                match nodes.split_last() {
                    None => instrs.push(Instruction::PushUnit),
                    Some((last, nodes)) => {
                        for node in nodes {
                            self.compile_node(instrs, node, is_root);
                            instrs.push(Instruction::Pop);
                        }
                        self.compile_node(instrs, last, is_root);
                    }
                }
            }
            NodeKind::If(ref cond, ref then, ref els) => {
                self.compile_node(instrs, cond, is_root);
                let mut then_instrs = Vec::new();
                self.compile_node(&mut then_instrs, then, is_root);
                let els_instrs = match *els {
                    None => vec![Instruction::PushUnit],
                    Some(ref n) => {
                        let mut els_instrs = Vec::new();
                        self.compile_node(&mut els_instrs, n, is_root);
                        els_instrs
                    }
                };
                instrs.push(Instruction::JumpIfZero(then_instrs.len() as i32 + 2));
                then_instrs.push(Instruction::Jump(els_instrs.len() as i32 + 1));
                instrs.extend(then_instrs);
                instrs.extend(els_instrs);
            }
            NodeKind::While(ref cond, ref body) => {
                let cond_instrs = {
                    let mut instrs = Vec::new();
                    self.compile_node(&mut instrs, cond, is_root);
                    instrs
                };
                let mut body_instrs = {
                    let mut instrs = Vec::new();
                    self.compile_node(&mut instrs, body, is_root);
                    instrs
                };
                body_instrs.push(Instruction::Pop);
                let jumpback_size = (cond_instrs.len() + 1 + body_instrs.len()) as isize;
                body_instrs.push(Instruction::Jump(-jumpback_size as i32));
                let break_size = body_instrs.len() as i32 + 1;
                instrs.extend(cond_instrs);
                instrs.push(Instruction::JumpIfZero(break_size));
                instrs.extend(body_instrs);
                instrs.push(Instruction::PushUnit)
            }
            NodeKind::Let(ref let_) => {
                self.compile_node(instrs, &let_.value, is_root);
                if is_root {
                    instrs.push(Instruction::SetGlobal(let_.id))
                } else {
                    instrs.push(Instruction::SetLocal(let_.id))
                }
            }
            NodeKind::Assign(ref lv_var, ref value) => {
                self.compile_node(instrs, value, is_root);
                instrs.push(Instruction::SetLocal(lv_var.index))
            }
            NodeKind::AssignGlobal(ref var, ref value) => {
                self.compile_node(instrs, value, is_root);
                instrs.push(Instruction::SetGlobal(var.index))
            }
        }
    }

    fn compile_function(&mut self, id: u32, f: &Function) {
        if self.functions.contains_key(&id) {
            return;
        }
        for (id, f) in f.env.functions() {
            self.compile_function(id, f);
        }
        let mut instrs = self.compile_nodes(&f.body, false);
        instrs.push(Instruction::Ret);
        let n_locals = f.env.n_locals();
        self.functions
            .insert(id,
                    Rc::new(instr::Function {
                                instrs: instrs,
                                n_locals: n_locals,
                            }));
    }

    fn compile_root(&mut self, env: &LocalEnv) {
        for (id, f) in env.functions() {
            self.compile_function(id, f);
        }
    }

    fn compile_nodes(&mut self, nodes: &[Node], is_root: bool) -> Vec<Instruction> {
        let mut instrs = Vec::new();
        if let Some((last, init)) = nodes.split_last() {
            for node in init {
                self.compile_node(&mut instrs, node, is_root);
                instrs.push(Instruction::Pop);
            }
            self.compile_node(&mut instrs, last, is_root);
        } else {
            instrs.push(Instruction::PushUnit);
        }
        instrs
    }
}
