pub mod function;
pub mod value;

use std::collections::HashMap;
use function::FunctionBuilder;
use value::ValueBuilder;
use crate::parser::ast::{ASTModule, ASTNode, ASTNodeType};
use crate::common::{Operator, Span};
use crate::semantic_checker::types::Type;
use crate::vm::inst::*;

pub struct ASTCompiler {
    pub functions: Vec<FunctionBuilder>,
    pub constants: Vec<ValueBuilder>,
    pub current_function: Option<usize>,
}

impl ASTCompiler {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            constants: vec![ValueBuilder::Unit],
            current_function: Some(0)
        }
    }

    fn add_constant(&mut self, value: ValueBuilder) -> u16 {
        if self.constants.contains(&value) {
            self.constants.push(value.clone());
        }

        self.constants.iter()
            .position(|x| *x == value)
            .unwrap() as u16
    }

    fn enter_new_scope(&mut self) {
        let id = self.current_function.expect("Not in function body");
        let func = self.functions
            .get_mut(id)
            .expect("Unknown function ID `{id}`");
        func.variables.push((HashMap::new(), func.next_scope_id));
        func.next_scope_id += 1;
    }

    fn exit_scope(&mut self) {
        let id = self.current_function.expect("Not in function body");
        self.functions
            .get_mut(id)
            .expect("Unknown function ID `{id}`")
            .variables.pop();
    }

    fn get_var(&mut self, name: &str) -> Option<u8> {
        let id = self.current_function.expect("Not in function body");
        self.functions
            .get_mut(id)
            .expect("Unknown function ID `{id}`")
            .get_var(name)
    }

    fn add_function(&mut self, name: &str, scope: usize) -> usize {
        let id = self.functions.len();
        self.functions.push(FunctionBuilder::new(
            format!("${name}_s{scope}"),
            scope
        ));
        id
    }

    fn get_func(&mut self, name: &str) -> Option<&mut FunctionBuilder> {
        for f in &mut self.functions {
            if f.name == format!("${name}_s{}", f.func_scope_id) {
                return Some(f);
            }
        }
        None
    }

    fn get_func_as_reg(&mut self, name: &str) -> Option<u8> {
        self.get_func(name)?.get_var(name)
    }

    fn collect_function(&mut self, node: &ASTNode) {
        match &node.ty {
            ASTNodeType::FunDef { name, params, body, .. } => {
                let scope = match self.current_function {
                    Some(id) => self.functions.get(id)
                        .expect("Unknown function ID `{id}`")
                        .variables.last()
                        .unwrap().1,
                    None => 0,
                };
                let func_id = self.add_function(name, scope);
                let previous_function = self.current_function;
                self.current_function = Some(func_id);

                {
                    let func = self.functions.get_mut(func_id).unwrap();

                    let f = func.define_var(name);

                    for (param_idx, (param, _, s)) in params.iter().enumerate() {
                        let p = func.define_var(param);
                        func.bytecode.push((
                            (CARG as u32) | ((p as u32) << 8) | ((param_idx as u32) << 16),
                            *s
                        ));
                    }

                    func.registers[f as usize] = ValueBuilder::FuncPointer(func_id);
                }

                let mut output = Vec::new();
                self.compile_node(body, 0, &mut output);

                let func = self.get_func(name).unwrap();
                func.bytecode = output;

                self.current_function = previous_function;
            },
            ASTNodeType::Semi(stmt) => self.collect_function(stmt),
            _ => return,
        }
    }

    pub fn compile(&mut self, ast: &ASTModule) {
        for node in &ast.nodes {
            self.collect_function(node);
        }
    }

    fn compile_node(&mut self, node: &ASTNode, dest: u8, output: &mut Vec<(u32, Span)>) {
        match &node.ty {
            ASTNodeType::IntLit(n) => {
                let id = self.add_constant(ValueBuilder::Int(*n));
                output.push((
                    (LOAD as u32) | ((dest as u32) << 8) | ((id as u32) << 16),
                    node.span,
                ));
            },
            ASTNodeType::FloatLit(n) => {
                let id = self.add_constant(ValueBuilder::Float(*n));
                output.push((
                    (LOAD as u32) | ((dest as u32) << 8) | ((id as u32) << 16),
                    node.span,
                ));
            },
            ASTNodeType::StringLit(n) => {
                let id = self.add_constant(ValueBuilder::String(n.clone()));
                output.push((
                    (LOAD as u32) | ((dest as u32) << 8) | ((id as u32) << 16),
                    node.span,
                ));
            },
            ASTNodeType::Boolean(n) => {
                let id = self.add_constant(ValueBuilder::Boolean(*n));
                output.push((
                    (LOAD as u32) | ((dest as u32) << 8) | ((id as u32) << 16),
                    node.span,
                ));
            },
            ASTNodeType::Identifier(n) => {
                let v = self.get_var(n).expect("Unknown variable `{n}`");
                if v != dest {
                    output.push((
                        (MOVE as u32) | ((dest as u32) << 8) | ((v as u32) << 16),
                        node.span,
                    ));
                }
            },
            ASTNodeType::Semi(stmt) => {
                self.compile_node(stmt, 0, output);
                output.push((
                    (LOAD as u32) | ((dest as u32) << 8),
                    node.span,
                ));
            },
            ASTNodeType::Block(stmts) => {
                if stmts.len() == 0 {
                    output.push((
                        (LOAD as u32) | ((dest as u32) << 8),
                        node.span,
                    ));
                    return;
                }

                self.enter_new_scope();
                for stmt in stmts {
                    self.compile_node(stmt, dest, output);
                }
                self.exit_scope();
            },
            ASTNodeType::Unit => output.push((
                (LOAD as u32) | ((dest as u32) << 8),
                node.span,
            )),
            ASTNodeType::BinaryOp { op, lhs, rhs, op_tys } => {
                self.compile_node(rhs, 1, output);

                if *op == Operator::Assign {
                    if let ASTNodeType::Identifier(s) = &lhs.ty {
                        let v = self.get_var(s).expect("Unknown variable");
                        output.push((
                            (MOVE as u32) | ((v as u32) << 8) | (1 << 16),
                             node.span,
                        ));
                        output.push((
                            (LOAD as u32) | ((dest as u32) << 8),
                            node.span,
                        ));
                    }   
                }

                self.compile_node(lhs, 0, output);

                match op {
                    Operator::Plus => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output.push((
                            (IADD as u32) | ((dest as u32) << 8) | (1 << 24),
                            node.span
                        )),
                        Type::Float => output.push((
                            (FADD as u32) | ((dest as u32) << 8) | (1 << 24),
                            node.span
                        )),
                        _ => unreachable!("Invalid type")
                    },
                    Operator::Minus => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output.push((
                            (ISUB as u32) | ((dest as u32) << 8) | (1 << 24),
                            node.span
                        )),
                        Type::Float => output.push((
                            (FSUB as u32) | ((dest as u32) << 8) | (1 << 24),
                            node.span
                        )),
                        _ => unreachable!("Invalid type")
                    },
                    Operator::Star => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output.push((
                            (IMUL as u32) | ((dest as u32) << 8) | (1 << 24),
                            node.span
                        )),
                        Type::Float => output.push((
                            (FMUL as u32) | ((dest as u32) << 8) | (1 << 24),
                            node.span
                        )),
                        _ => unreachable!("Invalid type")
                    },
                    Operator::Slash => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output.push((
                            (IDIV as u32) | ((dest as u32) << 8) | (1 << 24),
                            node.span
                        )),
                        Type::Float => output.push((
                            (FDIV as u32) | ((dest as u32) << 8) | (1 << 24),
                            node.span
                        )),
                        _ => unreachable!("Invalid type")
                    },
                    Operator::Modulo => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output.push((
                            (IREM as u32) | ((dest as u32) << 8) | (1 << 24),
                            node.span
                        )),
                        Type::Float => output.push((
                            (FREM as u32) | ((dest as u32) << 8) | (1 << 24),
                            node.span
                        )),
                        _ => unreachable!("Invalid type")
                    },
                    Operator::Eq => match op_tys.as_ref().unwrap().0 {
                        Type::String => output.push((
                            (SCEQ as u32) | ((dest as u32) << 8) | (1 << 24),
                            node.span
                        )),
                        _ => unreachable!("Invalid type")
                    },
                    Operator::Ne => match op_tys.as_ref().unwrap().0 {
                        Type::String => output.push((
                            (SCNE as u32) | ((dest as u32) << 8) | (1 << 24),
                            node.span
                        )),
                        _ => unreachable!("Invalid type")
                    },
                    Operator::Gt => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output.push((
                            (ICGT as u32) | ((dest as u32) << 8) | (1 << 24),
                            node.span
                        )),
                        _ => unreachable!("Invalid type")
                    },
                    Operator::Lt => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output.push((
                            (ICLT as u32) | ((dest as u32) << 8) | (1 << 24),
                            node.span
                        )),
                        _ => unreachable!("Invalid type")
                    },
                    Operator::Ge => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output.push((
                            (ICGE as u32) | ((dest as u32) << 8) | (1 << 24),
                            node.span
                        )),
                        _ => unreachable!("Invalid type")
                    },
                    Operator::Le => match op_tys.as_ref().unwrap().0 {
                        Type::Int => output.push((
                            (ICLE as u32) | ((dest as u32) << 8) | (1 << 24),
                            node.span
                        )),
                        _ => unreachable!("Invalid type")
                    },
                    Operator::Concat => output.push((
                        (SCON as u32) | ((dest as u32) << 8) | (1 << 24),
                        node.span
                    )),
                    Operator::LogOr => output.push((
                        (LOR as u32) | ((dest as u32) << 8) | (1 << 24),
                        node.span
                    )),
                    Operator::LogAnd => output.push((
                        (LAND as u32) | ((dest as u32) << 8) | (1 << 24),
                        node.span
                    )),
                    Operator::Pipe => output.push((
                        (BOR as u32) | ((dest as u32) << 8) | (1 << 24),
                        node.span
                    )),
                    Operator::Ampersand => output.push((
                        (BAND as u32) | ((dest as u32) << 8) | (1 << 24),
                        node.span
                    )),
                    Operator::Caret => output.push((
                        (BXOR as u32) | ((dest as u32) << 8) | (1 << 24),
                        node.span
                    )),
                    Operator::Lsh => output.push((
                        (LSHF as u32) | ((dest as u32) << 8) | (1 << 24),
                        node.span
                    )),
                    Operator::Rsh => output.push((
                        (RSHF as u32) | ((dest as u32) << 8) | (1 << 24),
                        node.span
                    )),
                    Operator::Tilde | Operator::Bang => unreachable!("Unary op in binary op"),
                    Operator::Assign => unreachable!(),
                }
            },
            _ => todo!("{node:#?}"),
        }
    }
}