use std::collections::HashMap;
use crate::common::{Operator, Span};
use crate::semantic_checker::types::Type;
use crate::vm::inst::*;
use crate::parser::ast::*;

pub mod value;
pub mod function_builder;
use value::ValueBuilder;
use function_builder::FunctionBuilder;

pub struct ASTCompiler {
    functions: Vec<FunctionBuilder>,
    pub constants: Vec<ValueBuilder>,
    current_function: usize,
}

impl ASTCompiler {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            constants: Vec::new(),
            current_function: 0,
        }
    }

    fn add_constant(&mut self, c: ValueBuilder) -> u16 {
        if !self.constants.contains(&c) {
            self.constants.push(c.clone());
        }
        self.constants.iter().position(|s| *s == c).unwrap() as u16
    }

    pub fn compile(&mut self, ast: &ASTModule) -> (usize, Vec<FunctionBuilder>) {
        for node in &ast.nodes {
            self.collect_functions_sig(node);
        }
        for node in &ast.nodes {
            self.compile_node(node, &mut Vec::new(), 0);
        }
        let main_id = self.get_func("@main_0");
        (main_id, self.functions.clone())
    }

    fn collect_functions_body(&mut self, node: &ASTNode) {
        match &node.ty {
            ASTNodeType::FunDef { name, params, body, .. } => {
                let scope = self.functions.last_mut()
                    .map(|func| func.current_scope_id)
                    .unwrap_or(0);
                self.current_function = self.get_func(&format!("@{name}_{scope}"));
                self.functions
                    .iter_mut()
                    .find(|f| f.name == format!("@{name}_{scope}"))
                    .unwrap()
                    .scope.last_mut()
                    .unwrap().0
                    .insert(format!("@{name}_{scope}"), 3);
                let mut output_buf = Vec::new();
                for (i, (name, _, _)) in params.iter().enumerate() {
                    let func = self.functions.get_mut(self.current_function).unwrap();
                    let (scope, next_available, sc_id) = func.scope.last_mut().unwrap();
                    let reg = *next_available;
                    scope.insert(format!("{name}_{sc_id}"), reg);
                    *next_available += 1;
                    output_buf.push((CARG as u32 | ((reg as u32) << 8) | ((i as u32) << 16), Span { start: 0, end: 0 })); // CARG does not create runtime errors (that im aware of)
                }
                self.compile_node(body, &mut output_buf, 0);
                output_buf.push((PARG as u32, Span { start: 0, end: 0 })); // PARG does not create runtime errors (that im aware of)
                output_buf.push((RETN as u32, Span { start: 0, end: 0 }));
                self.functions
                    .iter_mut()
                    .find(|f| f.name == format!("@{name}_{scope}"))
                    .unwrap().body = output_buf;
            },
            _ => {},
        }
    }

    fn get_func(&self, callee: &str) -> usize {
        self.functions.iter().position(|f| f.name == callee).unwrap()
    }

    fn collect_functions_sig(&mut self, node: &ASTNode) {
        match &node.ty {
            ASTNodeType::FunDef { name, .. } => {
                let mut registers: [ValueBuilder; 64] = std::array::from_fn(|_| ValueBuilder::Unit);
                registers[3] = ValueBuilder::Function(self.current_function);
                let scope = self.functions.last_mut()
                    .map(|func| func.current_scope_id)
                    .unwrap_or(0);
                self.functions.push(FunctionBuilder::new(&format!("@{name}_{scope}"), registers));
                self.functions
                    .last_mut()
                    .unwrap()
                    .scope.last_mut()
                    .unwrap().0
                    .insert(format!("@{name}_{scope}"), 3);
            },
            ASTNodeType::Semi(s) => self.collect_functions_sig(s),
            _ => {},
        }
    }

    fn compile_node(&mut self, node: &ASTNode, output_buf: &mut Vec<(u32, Span)>, dest: u8) {
        self.collect_functions_body(node);
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
            ASTNodeType::Identifier(n) => {
                let func = self.functions.get_mut(self.current_function).unwrap();
                output_buf.push((MOVE as u32 | ((dest as u32) << 8) | ((func.get_var(n) as u32) << 16), node.span))
            },
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

                let previous_scope_id: usize;
                {
                    let func = self.functions.get_mut(self.current_function).unwrap();
                    previous_scope_id = func.current_scope_id;
                    let new_scope_id = *func.taken_scope_ids.iter().max().unwrap() + 1;
                    func.current_scope_id = new_scope_id;
                    func.taken_scope_ids.push(new_scope_id);
                    func.scope.push((HashMap::new(), 3, func.current_scope_id));
                }

                if stmts.len() == 1 {
                    self.compile_node(&stmts[0], output_buf, dest);
                } else {
                    for node in &stmts[0..stmts.len() - 1] {
                        self.compile_node(node, output_buf, 0);
                    }
                    self.compile_node(&stmts[stmts.len() - 1], output_buf, dest);
                }

                
                let func = self.functions.get_mut(self.current_function).unwrap();
                func.scope.pop();
                func.current_scope_id = previous_scope_id;
            },
            ASTNodeType::BinaryOp { op, lhs, rhs, op_tys } => {
                if *op == Operator::Assign {
                    if let ASTNodeType::Identifier(s) = &lhs.ty {
                        let func = self.functions.get_mut(self.current_function).unwrap();
                        let reg = func.get_var(s);
                        self.compile_node(rhs, output_buf, 0);
                        output_buf.push((MOVE as u32 | ((reg as u32) << 8), node.span));

                        let const_id = self.add_constant(ValueBuilder::Unit);
                        output_buf.push((LOAD as u32 | ((dest as u32) << 8) | ((const_id as u32) << 16), node.span));
                        return;
                    }
                } else if *op == Operator::PlusAssign {
                    if let ASTNodeType::Identifier(s) = &lhs.ty {
                        let func = self.functions.get_mut(self.current_function).unwrap();
                        let reg = func.get_var(s);
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
                        let func = self.functions.get_mut(self.current_function).unwrap();
                        let reg = func.get_var(s);
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
                        let func = self.functions.get_mut(self.current_function).unwrap();
                        let reg = func.get_var(s);
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
                        let func = self.functions.get_mut(self.current_function).unwrap();
                        let reg = func.get_var(s);
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
                        let func = self.functions.get_mut(self.current_function).unwrap();
                        let reg = func.get_var(s);
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
                    Operator::Eq => match op_tys.as_ref().unwrap().0 {
                        Type::String => output_buf.push((SCEQ as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                        _ => output_buf.push((CMEQ as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                    },
                    Operator::Ne => match op_tys.as_ref().unwrap().0 {
                        Type::String => output_buf.push((SCNE as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                        _ => output_buf.push((CMNE as u32 | ((dest as u32) << 8) | 0x10000 | 0x2000000, node.span)),
                    },
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
                let func = self.functions.get_mut(self.current_function).unwrap();
                let (scope, next_available, sc_id) = func.scope.last_mut().unwrap();
                let reg = *next_available;
                scope.insert(format!("{name}_{sc_id}"), reg);
                *next_available += 1;
                if let Some(i) = init {
                    self.compile_node(i, output_buf, 0);
                    output_buf.push((MOVE as u32 | ((reg as u32) << 8), node.span))
                }

                let const_id = self.add_constant(ValueBuilder::Unit);
                output_buf.push((LOAD as u32 | ((dest as u32) << 8) | ((const_id as u32) << 16), node.span));
            },
            ASTNodeType::If { condition, then_body, else_body } => {
                self.compile_node(condition, output_buf, 0);

                let previous_scope_id: usize;
                {
                    let func = self.functions.get_mut(self.current_function).unwrap();

                    previous_scope_id = func.current_scope_id;
                    let new_scope_id = *func.taken_scope_ids.iter().max().unwrap() + 1;
                    func.current_scope_id = new_scope_id;
                    func.taken_scope_ids.push(new_scope_id);
                }
                let mut compiled_then = Vec::new();
                self.compile_node(then_body, &mut compiled_then, dest);

                let mut compile = None;
                {
                    let func = self.functions.get_mut(self.current_function).unwrap();
                    func.current_scope_id = previous_scope_id;

                    let then_len = compiled_then.len();
                    output_buf.push((JIFL as u32 | ((then_len as u32 + 1) << 8), node.span));
                    output_buf.extend(compiled_then);
                    if else_body.is_some() {
                        let previous_scope_id = func.current_scope_id;
                        let new_scope_id = *func.taken_scope_ids.iter().max().unwrap() + 1;
                        func.current_scope_id = new_scope_id;
                        func.taken_scope_ids.push(new_scope_id);
                        compile = else_body.clone().map(|x| (x, previous_scope_id));
                    }
                }
                if let Some((else_body, previous_scope_id)) = compile {
                    let mut compiled_else = Vec::new();
                    self.compile_node(&else_body, &mut compiled_else, dest);
                    output_buf.extend(compiled_else);
                    let func = self.functions.get_mut(self.current_function).unwrap();
                    func.current_scope_id = previous_scope_id;
                }
            },
            ASTNodeType::While { condition, body } => {
                let previous_scope_id: usize;
                let mut compiled_body: Vec<(u32, Span)>; 
                {
                    let func = self.functions.get_mut(self.current_function).unwrap();

                    previous_scope_id = func.current_scope_id;
                    let new_scope_id = *func.taken_scope_ids.iter().max().unwrap() + 1;
                    func.current_scope_id = new_scope_id;
                    func.taken_scope_ids.push(new_scope_id);
                    compiled_body = Vec::new();
                    self.compile_node(body, &mut compiled_body, 0);
                }
                let func = self.functions.get_mut(self.current_function).unwrap();
                func.current_scope_id = previous_scope_id;

                let mut compiled_condition = Vec::new();
                self.compile_node(condition, &mut compiled_condition, 0);
                output_buf.extend(&compiled_condition);
                let jump_to_end = compiled_body.len() as i16 + 2;
                output_buf.push((JIFL as u32 | ((jump_to_end as u32) << 8), node.span));

                output_buf.extend(&compiled_body);

                let jump_to_cond_eval = -(compiled_body.len() as i16 + compiled_condition.len() as i16 + 1);
                output_buf.push((JUMP as u32 | ((jump_to_cond_eval as u32) << 8), node.span));
            },
            ASTNodeType::FunDef { .. } => {},
            ASTNodeType::FunCall { callee, args } => {
                for arg in args {
                    self.compile_node(arg, output_buf, 0);
                    output_buf.push((PARG as u32, arg.span));
                }

                let func = self.functions.get_mut(self.current_function).unwrap();
                let fptr = func.get_var_safe(callee).unwrap_or_else(
                    || {
                        let fptr = self.get_func(&format!("@{callee}_0"));
                        let const_id = self.add_constant(ValueBuilder::Function(fptr));
                        output_buf.push((LOAD as u32 | ((const_id as u32) << 16), node.span));
                        0
                    }
                );
                output_buf.push((MOVE as u32 | ((fptr as u32) << 8), node.span));
                output_buf.push((CALL as u32 | ((fptr as u32) << 8), node.span));
                if dest != 0 { output_buf.push((MOVE as u32 | ((dest as u32) << 8), node.span)) }
            },
        }
    }
}