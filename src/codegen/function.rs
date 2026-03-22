use std::collections::HashMap;

use super::value::ValueBuilder;
use crate::common::Span;


pub struct FunctionBuilder {
    pub bytecode: Vec<(u32, Span)>,
    pub registers: [ValueBuilder; 64],
    pub variables: Vec<(HashMap<String, u8>, usize)>,
    pub name: String,
    pub func_scope_id: usize,
    pub next_scope_id: usize,
}

impl FunctionBuilder {
    pub fn new(name: String, func_scope_id: usize) -> Self {
        Self {
            bytecode: Vec::new(),
            registers: [const { ValueBuilder::Unit }; 64],
            variables: vec![(HashMap::new(), 0)],
            name,
            func_scope_id,
            next_scope_id: 1,
        }
    }

    pub fn get_var(&mut self, name: &str) -> Option<u8> {
        let mut offset = 0;

        for (scope, i) in self.variables.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Some(*v + offset);
            }

            offset += (i * scope.len()) as u8;
        }

        None
    }

    pub fn define_var(&mut self, name: &str) -> u8 {
        let (scope, _) = self.variables.last_mut().unwrap();
        let id = scope
            .iter()
            .last()
            .map(|x| *x.1).unwrap_or(0);
        scope.insert(name.to_string(), id);

        id
    }
}