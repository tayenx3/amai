use crate::{Span, operator::Operator};
use super::pattern::Pattern;

#[derive(Debug, Clone)]
pub struct AmaiASTModule {
    pub name: String,
    pub nodes: Vec<AmaiASTNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AmaiASTNodeKind {
    IntLit(i64),
    FloatLit(f64),
    Boolean(bool),
    Identifier(String),
    Semi(Box<AmaiASTNode>),
    Tuple(Vec<AmaiASTNode>),
    Block(Vec<AmaiASTNode>),
    Unit,
    BinaryOp {
        op: Operator,
        lhs: Box<AmaiASTNode>,
        rhs: Box<AmaiASTNode>,
    },
    UnaryOp {
        op: Operator,
        operand: Box<AmaiASTNode>,
    },
    LetDecl {
        pat: Pattern,
        init: Option<Box<AmaiASTNode>>,
    },
    VarDecl {
        pat: Pattern,
        init: Option<Box<AmaiASTNode>>,
    },
    If {
        condition: Box<AmaiASTNode>,
        then_body: Box<AmaiASTNode>,
        else_body: Option<Box<AmaiASTNode>>,
    },
    While {
        condition: Box<AmaiASTNode>,
        body: Box<AmaiASTNode>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct AmaiASTNode {
    pub kind: AmaiASTNodeKind,
    pub span: Span,
}