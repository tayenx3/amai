use crate::{Span, operator::Operator};
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    Func, Let, Var, If, Else, While, For, In,
    True, False,
    IntLit, FloatLit,
    Identifier,
    Operator(Operator),
    LParen, RParen, LSquare, RSquare, LCurly, RCurly,
    Semicolon, Colon, Comma, Dot, QuestionMark, Hashtag,
}

impl TokenKind {
    pub fn err_str(&self) -> String {
        match self {
            TokenKind::IntLit => "integer".to_string(),
            TokenKind::FloatLit => "float".to_string(),
            TokenKind::Identifier => "identifier".to_string(),
            TokenKind::Func => "keyword `func`".to_string(),
            TokenKind::Let => "keyword `let`".to_string(),
            TokenKind::Var => "keyword `var`".to_string(),
            TokenKind::If => "keyword `if`".to_string(),
            TokenKind::Else => "keyword `else`".to_string(),
            TokenKind::While => "keyword `while`".to_string(),
            TokenKind::For => "keyword `for`".to_string(),
            TokenKind::In => "keyword `in`".to_string(),
            TokenKind::True => "boolean `true`".to_string(),
            TokenKind::False => "boolean `false`".to_string(),
            TokenKind::Operator(op) => format!("`{}`", op.err_str()),
            TokenKind::LParen => "`(`".to_string(),
            TokenKind::RParen => "`)`".to_string(),
            TokenKind::LSquare => "`[`".to_string(),
            TokenKind::RSquare => "`]`".to_string(),
            TokenKind::LCurly => "`{`".to_string(),
            TokenKind::RCurly => "`}`".to_string(),
            TokenKind::Semicolon => "`;`".to_string(),
            TokenKind::Colon => "`:`".to_string(),
            TokenKind::Comma => "`,`".to_string(),
            TokenKind::Dot => "`.`".to_string(),
            TokenKind::QuestionMark => "`?`".to_string(),
            TokenKind::Hashtag => "`#`".to_string(),
        }
    }
}

#[derive(Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub literal: Option<LexLiteral>,
    pub span: Span,
}

impl Token {
    pub fn fmt_span(&self) -> String {
        format!("({:?}){:?}", self.span, self)
    }

    pub fn err_str(&self) -> String {
        match self.kind {
            TokenKind::IntLit => format!("integer `{}`", unsafe { self.literal.unwrap().integer }),
            TokenKind::FloatLit => format!("float `{}`", unsafe { self.literal.unwrap().integer }),
            TokenKind::Identifier => format!("identifier `{}`", self.lexeme),
            TokenKind::Func | TokenKind::Let | TokenKind::Var
            | TokenKind::If | TokenKind::Else | TokenKind::While
            | TokenKind::For | TokenKind::In => format!("reserved keyword `{}`", self.lexeme),
            _ => format!("`{}`", self.lexeme),
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            TokenKind::IntLit => write!(f, "IntLit({})", unsafe { self.literal.unwrap().integer }),
            TokenKind::FloatLit => write!(f, "FloatLit({})", unsafe { self.literal.unwrap().float }),
            TokenKind::Identifier => write!(f, "Identifier(`{}`)", self.lexeme),
            _ => write!(f, "{:?}", self.kind),
        }
    }
}

#[derive(Clone, Copy)]
pub union LexLiteral {
    pub integer: i64,
    pub float: f64,
}