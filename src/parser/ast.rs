use std::path::PathBuf;

use crate::{common::*, semantic_checker::types::Type};
use super::ftypes::FrontendType;

#[derive(Debug, Clone)]
pub struct ASTModule {
    pub path: PathBuf,
    pub nodes: Box<[ASTNode]>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTNodeType {
    IntLit(i64),
    FloatLit(f64),
    StringLit(String),
    Boolean(bool),
    Identifier(String),
    Semi(Box<ASTNode>),
    Block(Vec<ASTNode>),
    Unit,
    BinaryOp {
        op: Operator,
        lhs: Box<ASTNode>,
        rhs: Box<ASTNode>,
        op_tys: Option<(Type, Type)>,
    },
    UnaryOp {
        op: Operator,
        operand: Box<ASTNode>,
        op_ty: Option<Type>,
    },
    LetDecl {
        name: String,
        ty: Option<FrontendType>,
        init: Option<Box<ASTNode>>,
    },
    VarDecl {
        name: String,
        ty: Option<FrontendType>,
        init: Option<Box<ASTNode>>,
    },
    If {
        condition: Box<ASTNode>,
        then_body: Box<ASTNode>,
        else_body: Option<Box<ASTNode>>,
    },
    While {
        condition: Box<ASTNode>,
        body: Box<ASTNode>,
    },
    FunDef {
        name: String,
        args: Vec<(String, Option<FrontendType>, Span)>,
        return_ty: Option<FrontendType>,
        body: Box<ASTNode>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTNode {
    pub ty: ASTNodeType,
    pub span: Span,
}