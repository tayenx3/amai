use crate::semantic_checker::types::Type;

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Operator {
    Plus, Minus, Star, Slash, Modulo,
    Assign,
    Eq, Ne, Gt, Lt, Ge, Le,
    Concat,
    Tilde,
    LogOr, LogAnd, Bang,
    Pipe, Ampersand, Caret,
    Lsh, Rsh,
}

impl Operator {
    pub(crate) fn precedence(&self) -> (u32, u32) {
        match self {
            Operator::Star | Operator::Slash | Operator::Modulo => (110, 111),
            Operator::Plus | Operator::Minus => (100, 101),
            Operator::Lsh | Operator::Rsh => (90, 91),
            Operator::Gt | Operator::Lt | Operator::Ge | Operator::Le => (80, 81),
            Operator::Eq | Operator::Ne => (70, 71),
            Operator::Ampersand => (60, 61),
            Operator::Caret => (50, 51),
            Operator::Pipe => (40, 41),
            Operator::LogAnd => (30, 31),
            Operator::LogOr => (20, 21),
            Operator::Concat => (11, 10),
            _ => (0, 1),
        }
    }

    pub(crate) fn is_infix(&self) -> bool {
        ![Operator::Tilde, Operator::Bang].contains(self)
    }

    pub(crate) fn is_prefix(&self) -> bool {
        [Operator::Plus, Operator::Minus, Operator::Tilde, Operator::Bang].contains(self)
    }

    pub fn err_str(&self) -> &str {
        match self {
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Star => "*",
            Operator::Slash => "/",
            Operator::Modulo => "%",
            Operator::Assign => "=",
            Operator::Eq => "==",
            Operator::Ne => "!=",
            Operator::Gt => ">",
            Operator::Lt => "<",
            Operator::Ge => ">=",
            Operator::Le => "<=",
            Operator::Concat => "++",
            Operator::Tilde => "~",
            Operator::LogOr => "||",
            Operator::LogAnd => "&&",
            Operator::Bang => "!",
            Operator::Pipe => "|",
            Operator::Ampersand => "&",
            Operator::Caret => "^",
            Operator::Lsh => "<<",
            Operator::Rsh => ">>",
        }
    }

    pub fn infix_output(&self, lhs: &Type, rhs: &Type) -> Option<Type> {
        match self {
            Operator::Plus | Operator::Minus
            | Operator::Star | Operator::Slash
            | Operator::Modulo => match (lhs, rhs) { // numeric
                (Type::Int, Type::Int) => Some(Type::Int),
                (Type::Float, Type::Float) => Some(Type::Float),
                _ => None,
            },
            | Operator::Gt
            | Operator::Lt | Operator::Ge
            | Operator::Le => match (lhs, rhs) { // numeric
                (Type::Int, Type::Int) | (Type::Float, Type::Float) => Some(Type::Bool),
                _ => None,
            },
            Operator::Concat => match (lhs, rhs) {
                (Type::String, Type::String) => Some(Type::String),
                (Type::Vector(l), Type::Vector(r))
                    if l == r => Some(Type::Vector(l.clone())),
                _ => None,
            },
            Operator::Pipe | Operator::Ampersand
            | Operator::Caret | Operator::Lsh
            | Operator::Rsh => if let (Type::Int, Type::Int) = (lhs, rhs) {
                Some(Type::Int)
            } else {
                None
            },
            Operator::LogOr | Operator::LogAnd => if let (Type::Bool, Type::Bool) = (lhs, rhs) {
                Some(Type::Bool)
            } else {
                None
            },
            Operator::Assign => unreachable!("Assignations should be handled separately"),
            Operator::Eq | Operator::Ne => if lhs == rhs {
                Some(Type::Bool)
            } else {
                None
            },
            Operator::Tilde | Operator::Bang => unreachable!("Prefix ops should not be called here")
        }
    }

    pub fn prefix_output(&self, operand: &Type) -> Option<Type> {
        match self {
            Operator::Plus | Operator::Minus => match operand { // numeric
                Type::Int | Type::Float => Some(operand.clone()),
                _ => None,
            },
            Operator::Tilde => if *operand == Type::Int { Some(Type::Int )}
                else { None },
            Operator::Bang => if *operand == Type::Bool { Some(Type::Bool )}
                else { None },
            _ => unreachable!("Infix ops should not be called here")
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Span {
    pub(crate) start: usize,
    pub(crate) end: usize,
}

impl From<std::ops::Range<usize>> for Span {
    fn from(value: std::ops::Range<usize>) -> Self {
        Self { start: value.start, end: value.end }
    }
}