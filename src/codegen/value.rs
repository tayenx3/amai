use crate::vm::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum ValueBuilder {
    Int(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    FuncPointer(usize),
    Unit,
}

impl ValueBuilder {
    pub fn to_value(&self) -> Value {
        match self {
            Self::Int(n) => Value::from_int(*n),
            Self::Float(n) => Value::from_float(*n),
            Self::Boolean(n) => Value::from_bool(*n),
            Self::FuncPointer(n) => Value(*n as u64),
            Self::Unit => Value::nil(),
            _ => Value::nil(),
        }
    }

    pub fn is_large(&self) -> bool {
        match self {
            Self::String(_) => true,
            _ => false,
        }
    }
}