use crate::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum PatternLiteral {
    Integer(i64),
    Float(f64),
    Boolean(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    Identifier(String),
    Literal(PatternLiteral),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
}