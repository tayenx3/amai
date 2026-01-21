use crate::common::{Operator, Span};
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    IntLit, FloatLit, StringLit, Operator(Operator),
    Identifier,
    Let, Var, If, Else, While, For, In, Return, Extern, Export, Do, Then, With,
    True, False,
    LParen, RParen, LSquare, RSquare, LCurly, RCurly,
    Semicolon, Colon, Comma, Dot, QuestionMark, Hashtag,
    At, Arrow,
}

impl TokenType {
    pub fn err_str(&self) -> String {
        match self {
            Self::IntLit => "integer".to_string(),
            Self::FloatLit => "float".to_string(),
            Self::StringLit => "string".to_string(),
            Self::Identifier => "identifier".to_string(),
            Self::Let => "keyword `let`".to_string(),
            Self::Var => "keyword `var`".to_string(),
            Self::If => "keyword `if`".to_string(),
            Self::Else => "keyword `else`".to_string(),
            Self::While => "keyword `while`".to_string(),
            Self::For => "keyword `for`".to_string(),
            Self::In => "keyword `in`".to_string(),
            Self::Return => "keyword `return`".to_string(),
            Self::Extern => "keyword `extern`".to_string(),
            Self::Export => "keyword `export`".to_string(),
            Self::Do => "keyword `do`".to_string(),
            Self::Then => "keyword `then`".to_string(),
            Self::With => "keyword `with`".to_string(),
            Self::True => "boolean `true`".to_string(),
            Self::False => "boolean `false`".to_string(),
            Self::Operator(op) => format!("`{}`", op.err_str()),
            Self::LParen => "`(`".to_string(),
            Self::RParen => "`)`".to_string(),
            Self::LSquare => "`[`".to_string(),
            Self::RSquare => "`]`".to_string(),
            Self::LCurly => "`{`".to_string(),
            Self::RCurly => "`}`".to_string(),
            Self::Semicolon => "`;`".to_string(),
            Self::Colon => "`:`".to_string(),
            Self::Comma => "`,`".to_string(),
            Self::Dot => "`.`".to_string(),
            Self::QuestionMark => "`?`".to_string(),
            Self::Hashtag => "`#`".to_string(),
            Self::At => "`@`".to_string(),
            Self::Arrow => "`->`".to_string(),
        }
    }
}

#[derive(Clone)]
pub struct Token<'tk> {
    pub ty: TokenType,
    pub lit: Option<LexLiteral>,
    pub lex: &'tk str,
    pub span: Span,
}

impl Token<'_> {
    pub fn err_str(&self) -> String {
        match self.ty {
            TokenType::IntLit => format!("integer `{}`", unsafe { self.lit.unwrap().int_num }),
            TokenType::FloatLit => format!("float `{}`", unsafe { self.lit.unwrap().float_num }),
            TokenType::StringLit => format!("string `\"{}\"`", self.lex),
            TokenType::Identifier => format!("identifier `{}`", self.lex),
            TokenType::Let | TokenType::Var
            | TokenType::If | TokenType::Else | TokenType::While
            | TokenType::For | TokenType::In => format!("reserved keyword `{}`", self.lex),
            _ => format!("`{}`", self.lex),
        }
    }
}

impl fmt::Debug for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            TokenType::IntLit => write!(f, "IntLit({})", unsafe { self.lit.unwrap().int_num }),
            TokenType::FloatLit => write!(f, "FloatLit({})", unsafe { self.lit.unwrap().float_num }),
            TokenType::StringLit => write!(f, "StringLit(\"{}\")", self.lex),
            TokenType::Identifier => write!(f, "Identifier(`{}`)", self.lex),
            _ => write!(f, "{:?}", self.ty),
        }
    }
}

#[derive(Clone, Copy)]
pub union LexLiteral {
    pub int_num: i64,
    pub float_num: f64,
}