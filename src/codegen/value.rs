use crate::vm::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum ValueBuilder {
    Int(i64), Float(f64), Bool(bool), Unit, String(Vec<u8>),
}

impl ValueBuilder {
    pub fn is_large(&self) -> bool {
        match self {
            Self::Int(_) | Self::Float(_) | Self::Bool(_) | Self::Unit => false,
            _ => true,
        }
    }

    pub fn size(&self) -> usize {
        match self {
            Self::Int(_) | Self::Float(_) | Self::Bool(_) | Self::Unit => 0,
            Self::String(chars) => chars.len() + 4,
        }
    }

    pub fn align(&self) -> usize {
        match self {
            Self::Int(_) | Self::Float(_) | Self::Bool(_) | Self::Unit => 0,
            Self::String(_) => 1,
        }
    }

    pub fn data(&self) -> Vec<u8> {
        match self {
            Self::Int(_) | Self::Float(_) | Self::Bool(_) | Self::Unit => vec![],
            Self::String(chars) => {
                let mut t = (chars.len() as u32).to_le_bytes().to_vec();
                t.extend(chars);
                t
            },
        }
    }

    pub fn to_value(&self) -> Value {
        match self {
            Self::Int(n) => Value::from_int(*n),
            Self::Float(n) => Value::from_float(*n),
            Self::Bool(n) => Value::from_bool(*n),
            Self::Unit => Value::nil(),
            _ => Value::nil(),
        }
    }
}