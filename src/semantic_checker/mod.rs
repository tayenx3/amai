pub mod types;

use std::{collections::HashMap, path::PathBuf};
use crate::{common::{Operator, Span}, diagnostic::Diagnostic, parser::{ast::*, ftypes::*}};
use types::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub ty: Type,
    pub mutability: bool,
    pub is_unitialized: bool,
    defined_at: Span,
}

pub struct SemanticChecker {
    path: PathBuf,
    symbols: Vec<HashMap<String, Symbol>>,
    type_registry: HashMap<String, Type>,
}

impl SemanticChecker {
    pub fn new(path: PathBuf) -> Self {
        let mut t = SemanticChecker {
            path,
            symbols: vec![HashMap::new()],
            type_registry: HashMap::new()
        };

        t.type_registry.insert("int".to_string(), Type::Int);
        t.type_registry.insert("float".to_string(), Type::Float);
        t.type_registry.insert("string".to_string(), Type::String);
        t.type_registry.insert("bool".to_string(), Type::Bool);

        t
    }

    pub fn define_symbol(
        &mut self,
        name: &str,
        ty: Type,
        mutability: bool,
        is_unitialized: bool,
        defined_at: Span,
    ) {
        self.symbols.last_mut()
            .unwrap()
            .insert(name.to_string(), Symbol { ty, mutability, is_unitialized, defined_at });
    }

    pub fn mutate_symbol(
        &mut self,
        name: &str,
        ty: &Type,
        span: Span,
    ) -> Result<(), Diagnostic> {
        for scope in self.symbols.iter_mut().rev() {
            if let Some(symbol) = scope.get_mut(name) {
                if !symbol.mutability || !symbol.is_unitialized {
                    return Err(
                        Diagnostic::new(
                            self.path.display(),
                            format!("Variable `{name}` is immutable"),
                            span,
                        ).with_secondary_message(Some("Variable was defined here:"), symbol.defined_at.clone())
                    );
                }

                if symbol.ty == Type::Unknown {
                    symbol.ty = ty.clone();
                    symbol.is_unitialized = false;
                }
                if symbol.ty != *ty {
                    return Err(
                        Diagnostic::new(
                            self.path.display(),
                            format!("Variable `{name}` is defined as `{}` but found `{}`", symbol.ty.display(), ty.display()),
                            span,
                        ).with_secondary_message(Some("Variable was defined here:"), symbol.defined_at.clone())
                    );
                }
                return Ok(());
            }
        }

        return Err(
            Diagnostic::new(
                self.path.display(),
                format!("Couldn't find variable `{name}` in scope"),
                span,
            )
        );
    }

    pub fn find_symbol(&mut self, name: &str, span: Span) -> Result<&Symbol, Diagnostic>  {
        for scope in self.symbols.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Ok(symbol);
            }
        }

        return Err(
            Diagnostic::new(
                self.path.display(),
                format!("Couldn't find variable `{name}` in scope"),
                span,
            )
        );
    }

    pub fn resolve_type(&self, ftype: &FrontendType) -> Result<Type, Diagnostic> {
        match &ftype.ty {
            FrontendTypeType::Identifier(ident) => self.type_registry.get(ident).cloned()
                .ok_or(Diagnostic::new(self.path.display(), format!("Cannot find type `{}`", ident), ftype.span)),
            FrontendTypeType::Unit => Ok(Type::Unit),
            FrontendTypeType::Vector(vec) => Ok(Type::Vector(Box::new(self.resolve_type(vec)?))),
        }
    }
    
    fn collect_function(&mut self, node: &ASTNode) -> Result<(), Diagnostic> {
        match &node.ty {
            ASTNodeType::FunDef { name, params, return_ty, body: _ } => {
                let mut params_ty = Vec::new();
                for (_, ty, _) in params {
                    params_ty.push(self.resolve_type(ty)?);
                }
                let return_ty = match return_ty {
                    Some(ty) => Box::new(self.resolve_type(ty)?),
                    None => Box::new(Type::Unit),
                };
                self.define_symbol(name, Type::Func(params_ty, return_ty), false, false, node.span);
            },
            _ => {},
        }

        Ok(())
    }
    fn collect_function_deep(&mut self, node: &ASTNode) -> Result<(), Diagnostic> {
        match &node.ty {
            ASTNodeType::FunDef { name, params, return_ty, body: _ } => {
                let mut params_ty = Vec::new();
                for (_, ty, _) in params {
                    params_ty.push(self.resolve_type(ty)?);
                }
                let return_ty = match return_ty {
                    Some(ty) => Box::new(self.resolve_type(ty)?),
                    None => Box::new(Type::Unit),
                };
                self.define_symbol(name, Type::Func(params_ty, return_ty), false, false, node.span);
            },
            ASTNodeType::Semi(s) => self.collect_function_deep(s)?,
            _ => {},
        }

        Ok(())
    }

    pub fn validate(&mut self, ast: &mut ASTModule) -> Result<(), Vec<Diagnostic>> {
        let mut diagnostics = Vec::new();

        for node in &ast.nodes {
            if let Err(diag) = self.collect_function_deep(node) {
                diagnostics.push(diag);
            }
        }

        for node in &mut ast.nodes {
            if let Err(diag) = self.validate_node(node, false, false) {
                diagnostics.extend(diag);
            }
        }

        if !diagnostics.is_empty() {
            return Err(diagnostics);
        }

        Ok(())
    }

    pub fn validate_node(&mut self, node: &mut ASTNode, force_exhaustive: bool, recollect: bool) -> Result<Type, Vec<Diagnostic>> {
        if recollect {
            self.collect_function(node).map_err(|err| vec![err])?;
        }
        match &mut node.ty {
            ASTNodeType::IntLit(_) => Ok(Type::Int),
            ASTNodeType::FloatLit(_) => Ok(Type::Float),
            ASTNodeType::StringLit(_) => Ok(Type::String),
            ASTNodeType::Boolean(_) => Ok(Type::Bool),
            ASTNodeType::Identifier(s) => self
                .find_symbol(s, node.span.clone())
                .map(|sy| sy.ty.clone())
                .map_err(|err| vec![err]),
            ASTNodeType::Semi(stmt) => {
                self.validate_node(stmt, false, true)?;
                Ok(Type::Unit)
            },
            ASTNodeType::Block(stmts) => {
                let mut last_ty = Type::Unit;

                for stmt in &*stmts {
                    self.collect_function_deep(stmt).map_err(|err| vec![err])?;
                }
                for stmt in stmts {
                    last_ty = self.validate_node(stmt, false, false)?;
                }

                Ok(last_ty)
            },
            ASTNodeType::Unit => Ok(Type::Unit),
            ASTNodeType::BinaryOp { op, lhs, rhs, op_tys } => {
                if [
                    Operator::Assign,
                    Operator::PlusAssign,
                    Operator::MinusAssign,
                    Operator::StarAssign,
                    Operator::SlashAssign,
                    Operator::ModuloAssign
                    ].contains(op)
                {
                    match &lhs.ty {
                        ASTNodeType::Identifier(s) => {
                            let rhs_ty = self.validate_node(rhs, true, true)?;
                            self.mutate_symbol(s, &rhs_ty, node.span.clone()).map_err(|err| vec![err])?;
                            let sym = self
                                .find_symbol(s, node.span)
                                .map_err(|err| vec![err])?.clone();
                            if *op != Operator::Assign && ![Type::Int, Type::Float].contains(&sym.ty) {
                                return Err(
                                    vec![Diagnostic::new(
                                        self.path.display(),
                                        format!("Cannot use arithmetic mutation on variable of type `{}`", sym.ty.display()),
                                        node.span.clone(),
                                    ).with_secondary_message(Some(format!("Variable `{s}` was defined here:")), sym.defined_at)]
                                )
                            }
                            let var_ty = sym.ty;
                            *op_tys = Some((var_ty, rhs_ty));
                            return Ok(Type::Unit);
                        },
                        _ => return Err(
                            vec![Diagnostic::new(
                                self.path.display(),
                                "Can only mutate variables",
                                node.span.clone(),
                            )]
                        ),
                    }
                }
                
                let lhs_ty = self.validate_node(lhs, true, true)?;
                let rhs_ty = self.validate_node(rhs, true, true)?;

                if let Some(output) = op.infix_output(&lhs_ty, &rhs_ty) {
                    *op_tys = Some((lhs_ty, rhs_ty));
                    Ok(output)
                } else {
                    Err(
                        vec![Diagnostic::new(
                            self.path.display(),
                            format!(
                                "Cannot apply `{}` as an infix operator on types `{}` and `{}`",
                                op.err_str(),
                                lhs_ty.display(),
                                rhs_ty.display()
                            ),
                            node.span.clone(),
                        )]
                    )
                }
            },
            ASTNodeType::UnaryOp { op, operand, op_ty} => {
                let operand_ty = self.validate_node(operand, true, true)?;

                if let Some(output) = op.prefix_output(&operand_ty) {
                    *op_ty = Some(operand_ty);
                    Ok(output)
                } else {
                    Err(
                        vec![Diagnostic::new(
                            self.path.display(),
                            format!(
                                "Cannot apply `{}` as a unary operator on type `{}`",
                                op.err_str(),
                                operand_ty.display()
                            ),
                            node.span.clone(),
                        )]
                    )
                }
            },
            ASTNodeType::LetDecl { name, ty, init } => {
                let mut var_ty = if let Some(ty) = ty {
                    self.resolve_type(ty).map_err(|err| vec![err])?
                } else {
                    Type::Unknown
                };
                if let Some(i) = init {
                    let init_ty = self.validate_node(i, true, true)?;
                    if var_ty == Type::Unknown {
                        var_ty = init_ty.clone();
                    }
                    if init_ty != var_ty {
                        return Err(
                            vec![Diagnostic::new(
                                self.path.display(),
                                format!(
                                    "Variable `{name}` is declared as `{}` but initialized as `{}`",
                                    var_ty.display(),
                                    init_ty.display(),
                                ),
                                i.span.clone(),
                            )]
                        )
                    }
                    self.define_symbol(
                        name,
                        var_ty,
                        false,
                        true,
                        node.span
                    );
                } else {
                    self.define_symbol(
                        name,
                        var_ty,
                        false,
                        false,
                        node.span
                    );
                }

                Ok(Type::Unit)
            },
            ASTNodeType::VarDecl { name, ty, init } => {
                let mut var_ty = if let Some(ty) = ty {
                    self.resolve_type(ty).map_err(|err| vec![err])?
                } else {
                    Type::Unknown
                };
                if let Some(i) = init {
                    let init_ty = self.validate_node(i, true, true)?;
                    if var_ty == Type::Unknown {
                        var_ty = init_ty.clone();
                    }
                    if init_ty != var_ty {
                        return Err(
                            vec![Diagnostic::new(
                                self.path.display(),
                                format!(
                                    "Variable `{name}` is declared as `{}` but initialized as `{}`",
                                    var_ty.display(),
                                    init_ty.display(),
                                ),
                                i.span.clone(),
                            )]
                        )
                    }
                    self.define_symbol(
                        name,
                        var_ty,
                        true,
                        true,
                        node.span
                    );
                } else {
                    self.define_symbol(
                        name,
                        var_ty,
                        true,
                        false,
                        node.span
                    );
                }

                Ok(Type::Unit)
            },
            ASTNodeType::If { condition, then_body, else_body } => {
                let mut errors = Vec::new();
                let _ = self.validate_node(condition, true, true)
                    .inspect_err(|err| errors.extend(err.clone()))
                    .inspect(
                        |ty| if *ty != Type::Bool {
                            errors.push(
                                Diagnostic::new(
                                    self.path.display(),
                                    "Expected boolean condition in `if`",
                                    condition.span.clone(),
                                )
                            )
                        }
                    );

                let then_body_ty = self.validate_node(then_body, force_exhaustive, true)?;
                if force_exhaustive {
                    if let Some(else_body) = else_body {
                        let else_body_ty = self.validate_node(else_body, force_exhaustive, true)?;

                        if else_body_ty != then_body_ty {
                            errors.push(
                                Diagnostic::new(
                                    self.path.display(),
                                    format!(
                                        "`if`'s clauses has different return types: `{}` and `{}`",
                                        then_body_ty.display(), else_body_ty.display(),
                                    ),
                                    node.span.clone(),
                                )
                            );
                            Err(errors)
                        } else {
                            Ok(then_body_ty)
                        }
                    } else {
                        errors.push(
                            Diagnostic::new(
                                self.path.display(),
                                format!(
                                    "Missing `else` clause that evaluates to type `{}`",
                                    then_body_ty.display(),
                                ),
                                node.span.clone(),
                            )
                        );
                        Err(errors)
                    }
                } else {
                    Ok(Type::Unit)
                }
            },
            ASTNodeType::While { condition, body } => {
                let mut errors = Vec::new();
                let cond_ty = self.validate_node(condition, true, true)?;
                if cond_ty != Type::Bool {
                    errors.push(
                        Diagnostic::new(
                            self.path.display(),
                            "Expected boolean condition in `while`",
                            condition.span.clone(),
                        )
                    )
                }

                self.symbols.push(HashMap::new());
                let _ = self.validate_node(body, force_exhaustive, true).inspect_err(|err| errors.extend_from_slice(err.as_slice()));
                if !errors.is_empty() { return Err(errors) }
                self.symbols.pop();
                Ok(Type::Unit)
            },
            ASTNodeType::FunDef { name, params, return_ty, body } => {
                let mut scope = HashMap::new();
                for (name, ty, span) in params {
                    scope.insert(name.clone(),
                        Symbol {
                            ty: self.resolve_type(ty).map_err(|err| vec![err])?,
                            mutability: false,
                            is_unitialized: false,
                            defined_at: *span
                        });
                }
                self.symbols.push(scope);
                let body_ty = self.validate_node(body, true, true)?;
                let return_ty = return_ty.as_ref()
                    .map(|ty| self.resolve_type(ty))
                    .unwrap_or(Ok(Type::Unit))
                    .map_err(|err| vec![err])?;
                if body_ty != return_ty {
                    return Err(vec![Diagnostic::new(
                        self.path.display(),
                        format!(
                            "Function `{name}` is declared as a function of return type `{}`, but body returns `{}`",
                            return_ty.display(),
                            body_ty.display()
                        ),
                        body.span
                    )]);
                }
                self.symbols.pop();
                Ok(return_ty)
            },
            ASTNodeType::FunCall { callee, args } => {
                let symbol = self.find_symbol(&callee, node.span).map_err(|err| vec![err])?.clone();
                if let Type::Func(params_ty, ty) = symbol.ty {
                    for (i, arg) in args.iter_mut().enumerate() {
                        let arg_ty = self.validate_node(arg, true, true)?;
                        if params_ty[i] != arg_ty {
                            return Err(vec![Diagnostic::new(
                                self.path.display(),
                                format!(
                                    "Function has argument #{i} as type `{}` but found `{}`",
                                    params_ty[i].display(),
                                    arg_ty.display()
                                ),
                                node.span
                            )]);
                        }
                    }
                    Ok(*ty)
                } else {
                    Err(vec![Diagnostic::new(self.path.display(), format!("Identifier {callee} is not a function"), node.span)])
                }
            },
        }
    }
}
