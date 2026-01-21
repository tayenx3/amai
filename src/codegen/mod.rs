use std::collections::HashMap;
use crate::common::{Operator, Span};
use crate::semantic_checker::types::Type;
use crate::vm::inst::*;
use crate::parser::ast::*;

pub mod value;
use value::ValueBuilder;

pub struct ASTCompiler {
    scope: Vec<(HashMap<String, u8>, u8, usize)>,
    pub constants: Vec<ValueBuilder>,
    current_scope_id: usize,
    taken_scope_ids: Vec<usize>,
}

impl ASTCompiler {
    pub fn new() -> Self {
        Self {
            scope: vec![(HashMap::new(), 3, 0)],
            constants: Vec::new(),
            current_scope_id: 0,
            taken_scope_ids: vec![0],
        }
    }

    fn add_constant(&mut self, c: ValueBuilder) -> u16 {
        if !self.constants.contains(&c) {
            self.constants.push(c.clone());
        }
        self.constants.iter().position(|s| *s == c).unwrap() as u16
    }

    pub fn compile(&mut self, ast: &ASTModule) -> Vec<(u32, Span)> {
        let mut func = Vec::new();
        for node in &ast.nodes {
            self.compile_node(node, &mut func, 0);
        }
        func
    }

    pub fn get_var(&mut self, n: &str) -> u8 {
        let s = self.scope
            .iter()
            .rev()
            .find(
                |(s, _, sc)|
                s.contains_key(&format!("{n}_{sc}"))
            ).unwrap();
        s.0[&format!("{n}_{}", s.2)]
    }

    fn compile_node(&mut self, node: &ASTNode, output_buf: &mut Vec<(u32, Span)>, dest: u8) {
        match &node.ty {
            ASTNodeType::IntLit(n) => {
                let const_id = self.add_constant(ValueBuilder::Int(*n));
                output_buf.push((LOAD as u32 | ((dest as u32) << 8) | ((const_id as u32) << 16), node.span));
            },
            ASTNodeType::FloatLit(n) => {
                let const_id = self.add_constant(ValueBuilder::Float(*n));
                output_buf.push((LOAD as u32 | ((dest as u32) << 8) | ((const_id as u32) << 16), node.span));
            },
            ASTNodeType::StringLit(n) => {
                let chars = n.as_bytes();
                let const_id = self.add_constant(ValueBuilder::String(chars.to_vec()));
                output_buf.push((LOAD as u32 | ((dest as u32) << 8) | ((const_id as u32) << 16), node.span));
            }
            ASTNodeType::Boolean(n) => {
                let const_id = self.add_constant(ValueBuilder::Bool(*n));
                output_buf.push((LOAD as u32 | ((dest as u32) << 8) | ((const_id as u32) << 16), node.span));
            },
            ASTNodeType::Identifier(n) => output_buf.push((MOVE as u32 | ((dest as u32) << 8) | ((self.get_var(n) as u32) << 16), node.span)),
            ASTNodeType::Semi(stmt) => self.compile_node(stmt, output_buf, 0),
            ASTNodeType::Unit => {
                let const_id = self.add_constant(ValueBuilder::Unit);
                output_buf.push((LOAD as u32 | ((dest as u32) << 8) | ((const_id as u32) << 16), node.span));
            },
            ASTNodeType::Block(stmts) => {
                if stmts.len() == 0 {
                    let const_id = self.add_constant(ValueBuilder::Unit);
                    output_buf.push((LOAD as u32 | ((dest as u32) << 8) | ((const_id as u32) << 16), node.span));
                    return;
                }

                let previous_scope_id = self.current_scope_id;
                let new_scope_id = *self.taken_scope_ids.iter().max().unwrap() + 1;
                self.current_scope_id = new_scope_id;
                self.taken_scope_ids.push(new_scope_id);
                self.scope.push((HashMap::new(), 3, self.current_scope_id));

                if stmts.len() == 1 {
                    self.compile_node(&stmts[0], output_buf, dest);
                } else {
                    for node in &stmts[0..stmts.len() - 1] {
                        self.compile_node(node, output_buf, 0);
                    }
                    self.compile_node(&stmts[stmts.len() - 1], output_buf, dest);
                }

                self.scope.pop();
                self.current_scope_id = previous_scope_id;
            },
            ASTNodeType::BinaryOp { op, lhs, rhs, op_tys } => {
                if *op == Operator::Assign {
                    if let ASTNodeType::Identifier(s) = &lhs.ty {
                        let reg = self.get_var(s);
                        self.compile_node(rhs, output_buf, 0);
                        output_buf.push((MOVE as u32 | ((reg as u32) << 8), node.span));

                        let const_id = self.add_constant(ValueBuilder::Unit);
                        output_buf.push((LOAD as u32 | ((dest as u32) << 8) | ((const_id as u32) << 16), node.span));
                        return;
                    }
                } else if *op == Operator::PlusAssign {
                    if let ASTNodeType::Identifier(s) = &lhs.ty {
                        let reg = self.get_var(s);
                        self.compile_node(rhs, output_buf, 1);
                        match op_tys.as_ref().unwrap().0 {
                            Type::Int => output_buf.push((IADD as u32 | ((reg as u32) << 8) | ((reg as u32) << 16) | 0x1000000, node.span)),
                            Type::Float => output_buf.push((FADD as u32 | ((reg as u32) << 8) | ((reg as u32) << 16) | 0x1000000, node.span)),
                            _ => unreachable!(),
                        }

                        let const_id = self.add_constant(ValueBuilder::Unit);
                        output_buf.push((LOAD as u32 | ((dest as u32) << 8) | ((const_id as u32) << 16), node.span));
                        return;
                    }
                } else if *op == Operator::MinusAssign {
                    if let ASTNodeType::Identifier(s) = &lhs.ty {
                        let reg = self.get_var(s);
                        self.compile_node(rhs, output_buf, 1);
                        match op_tys.as_ref().unwrap().0 {
                            Type::Int => output_buf.push((ISUB as u32 | ((reg as u32) << 8) | ((reg as u32) << 16) | 0x1000000, node.span)),
                            Type::Float => output_buf.push((FSUB as u32 | ((reg as u32) << 8) | ((reg as u32) << 16) | 0x1000000, node.span)),
                            _ => unreachable!(),
                        }

                        let const_id = self.add_constant(ValueBuilder::Unit);
                        output_buf.push((LOAD as u32 | ((dest as u32) << 8) | ((const_id as u32) << 16), node.span));
                        return;
                    }
                } else if *op == Operator::StarAssign {
                    if let ASTNodeType::Identifier(s) = &lhs.ty {
                        let reg = self.get_var(s);
                        self.compile_node(rhs, output_buf, 1);
                        match op_tys.as_ref().unwrap().0 {
                            Type::Int => output_buf.push((IMUL as u32 | ((reg as u32) << 8) | ((reg as u32) << 16) | 0x1000000, node.span)),
                            Type::Float => output_buf.push((FMUL as u32 | ((reg as u32) << 8) | ((reg as u32) << 16) | 0x1000000, node.span)),
                            _ => unreachable!(),
                        }

                        let const_id = self.add_constant(ValueBuilder::Unit);
                        output_buf.push((LOAD as u32 | ((dest as u32) << 8) | ((const_id as u32) << 16), node.span));
                        return;
                    }
                } else if *op == Operator::SlashAssign {
                    if let ASTNodeType::Identifier(s) = &lhs.ty {
                        let reg = self.get_var(s);
                        self.compile_node(rhs, output_buf, 1);
                        match op_tys.as_ref().unwrap().0 {
                            Type::Int => output_buf.push((IDIV as u32 | ((reg as u32) << 8) | ((reg as u32) << 16) | 0x1000000, node.span)),
                            Type::Float => output_buf.push((FDIV as u32 | ((reg as u32) << 8) | ((reg as u32) << 16) | 0x1000000, node.span)),
                            _ => unreachable!(),
                        }

                        let const_id = self.add_constant(ValueBuilder::Unit);
                        output_buf.push((LOAD as u32 | ((dest as u32) << 8) | ((const_id as u32) << 16), node.span));
                        return;
                    }
                } else if *op == Operator::ModuloAssign {
                    if let ASTNodeType::Identifier(s) = &lhs.ty {
                        let reg = self.get_var(s);
                        self.compile_node(rhs, output_buf, 1);
                        match op_tys.as_ref().unwrap().0 {
                            Type::Int => output_buf.push((IREM as u32 | ((reg as u32) << 8) | ((reg as u32) << 16) | 0x1000000, node.span)),
                            Type::Float => output_buf.push((FREM as u32 | ((reg as u32) << 8) | ((reg as u32) << 16) | 0x1000000, node.span)),
                            _ => unreachable!(),
                        }

                        let const_id = self.add_constant(ValueBuilder::Unit);
                        output_buf.push((LOAD as u32 | ((dest as u32) << 8) | ((const_id as u32) << 16), node.span));
                        return;
                    }
                }

                self.compile_node(lhs, output_buf, 1);
                self.compile_node(rhs, output_buf, 2);

                match op {
                    Operator::Plus => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output_buf.push((IADD as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                        Type::Float => output_buf.push((FADD as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                        _ => unreachable!(),
                    },
                    Operator::Minus => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output_buf.push((ISUB as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                        Type::Float => output_buf.push((FSUB as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                        _ => unreachable!(),
                    },
                    Operator::Star => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output_buf.push((IMUL as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                        Type::Float => output_buf.push((FMUL as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                        _ => unreachable!(),
                    },
                    Operator::Slash => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output_buf.push((IDIV as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                        Type::Float => output_buf.push((FDIV as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                        _ => unreachable!(),
                    },
                    Operator::Modulo => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output_buf.push((IREM as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                        Type::Float => output_buf.push((FREM as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                        _ => unreachable!(),
                    },
                    Operator::Eq => output_buf.push((CMEQ as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                    Operator::Ne => output_buf.push((CMNE as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                    Operator::Gt => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output_buf.push((ICGT as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                        Type::Float => output_buf.push((FCGT as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                        _ => unreachable!(),
                    },
                    Operator::Lt => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output_buf.push((ICLT as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                        Type::Float => output_buf.push((FCLT as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                        _ => unreachable!(),
                    },
                    Operator::Ge => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output_buf.push((ICGE as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                        Type::Float => output_buf.push((FCGE as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                        _ => unreachable!(),
                    },
                    Operator::Le => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output_buf.push((ICLE as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                        Type::Float => output_buf.push((FCLE as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                        _ => unreachable!(),
                    },
                    Operator::Concat => output_buf.push((SCON as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                    Operator::LogOr => output_buf.push((LOR as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                    Operator::LogAnd => output_buf.push((LAND as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                    Operator::Pipe => output_buf.push((BOR as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                    Operator::Ampersand => output_buf.push((BAND as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                    Operator::Caret => output_buf.push((BXOR as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                    Operator::Lsh => output_buf.push((LSHF as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                    Operator::Rsh => output_buf.push((RSHF as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                    _ => todo!("binary op: {op:?}"),
                }
            },
            ASTNodeType::UnaryOp { op, operand, op_ty } => {
                self.compile_node(operand, output_buf, 1);

                match *op {
                    Operator::Plus => output_buf.push((MOVE as u32 | ((dest as u32) << 8) | 0x10000, node.span)),
                    Operator::Minus => match op_ty.as_ref().unwrap() {
                        Type::Int => output_buf.push((INEG as u32 | ((dest as u32) << 8) | 0x10000, node.span)),
                        Type::Float => output_buf.push((FNEG as u32 | ((dest as u32) << 8) | 0x10000, node.span)),
                        _ => unreachable!(),
                    },
                    Operator::Tilde => output_buf.push((BNOT as u32 | ((dest as u32) << 8) | 0x10000, node.span)),
                    Operator::Bang => output_buf.push((LNOT as u32 | ((dest as u32) << 8) | 0x10000, node.span)),
                    _ => unreachable!()
                }
            },
            ASTNodeType::LetDecl { name, ty: _, init }
            | ASTNodeType::VarDecl { name, ty: _, init } => {
                let (scope, next_available, sc_id) = self.scope.last_mut().unwrap();
                let reg = *next_available;
                scope.insert(format!("{name}_{sc_id}"), reg);
                *next_available += 1;
                if let Some(i) = init {
                    self.compile_node(i, output_buf, 0);
                    output_buf.push((MOVE as u32 | ((reg as u32) << 8), node.span))
                }
            },
            ASTNodeType::If { condition, then_body, else_body } => {
                self.compile_node(condition, output_buf, 0);

                let previous_scope_id = self.current_scope_id;
                let new_scope_id = *self.taken_scope_ids.iter().max().unwrap() + 1;
                self.current_scope_id = new_scope_id;
                self.taken_scope_ids.push(new_scope_id);
                let mut compiled_then = Vec::new();
                self.compile_node(then_body, &mut compiled_then, dest);
                self.current_scope_id = previous_scope_id;

                let then_len = compiled_then.len();
                output_buf.push((JIFL as u32 | ((then_len as u32 + 1) << 8), node.span));
                output_buf.extend(compiled_then);
                if let Some(else_body) = else_body {
                    let previous_scope_id = self.current_scope_id;
                    let new_scope_id = *self.taken_scope_ids.iter().max().unwrap() + 1;
                    self.current_scope_id = new_scope_id;
                    self.taken_scope_ids.push(new_scope_id);
                    let mut compiled_else = Vec::new();
                    self.compile_node(else_body, &mut compiled_else, dest);
                    output_buf.extend(compiled_else);
                    self.current_scope_id = previous_scope_id;
                }
            },
            ASTNodeType::While { condition, body } => {
                let previous_scope_id = self.current_scope_id;
                let new_scope_id = *self.taken_scope_ids.iter().max().unwrap() + 1;
                self.current_scope_id = new_scope_id;
                self.taken_scope_ids.push(new_scope_id);
                let mut compiled_body = Vec::new();
                self.compile_node(body, &mut compiled_body, 0);
                self.current_scope_id = previous_scope_id;

                let mut compiled_condition = Vec::new();
                self.compile_node(condition, &mut compiled_condition, 0);
                output_buf.extend(&compiled_condition);
                let jump_to_end = compiled_body.len() as i16 + 2;
                output_buf.push((JIFL as u32 | ((jump_to_end as u32) << 8), node.span));

                output_buf.extend(&compiled_body);

                let jump_to_cond_eval = -(compiled_body.len() as i16 + compiled_condition.len() as i16 + 1);
                output_buf.push((JUMP as u32 | ((jump_to_cond_eval as u32) << 8), node.span));
            },
            ASTNodeType::FunDef { name, args, return_ty, body } => {},
        }
    }
}