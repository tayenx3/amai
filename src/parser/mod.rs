pub mod ast;
//pub mod pattern;
pub mod ftypes;

use std::path::{Path, PathBuf};

use crate::{diagnostic::Diagnostic, common::Operator};
use super::lexer::token::*;
use ast::*;
//use pattern::*;
use ftypes::*;

pub struct Parser<'p> {
    path: PathBuf,
    tokens: &'p Box<[Token<'p>]>,
    pos: usize,
}

impl<'p> Parser<'p> {
    pub fn new<P: AsRef<Path>>(path: P, tokens: &'p Box<[Token]>) -> Parser<'p> {
        Parser {
            path: path.as_ref().to_path_buf(),
            tokens,
            pos: 0,
        }
    }

    pub fn parse(&mut self) -> Result<ASTModule, Vec<Diagnostic>> {
        let mut module = Vec::new();
        if self.tokens.is_empty() {
            return Ok(ASTModule {
                path: self.path.clone(),
                nodes: module.into_boxed_slice(),
            });
        }
        let mut diagnostics = Vec::new();

        while self.tokens.get(self.pos).is_some() {
            let stmt = self.parse_stmt();
            match stmt {
                Ok(node) => module.push(node),
                Err(err) => {
                    diagnostics.push(err);
                    while let Some(token) = self.tokens.get(self.pos) {
                        // syncronize
                        if token.ty == TokenType::Semicolon
                            || token.ty == TokenType::RCurly
                            || token.ty == TokenType::RParen
                            || token.ty == TokenType::RSquare
                            || token.ty == TokenType::LCurly
                        {
                            break;
                        }
                        self.pos += 1;
                    }
                    break;
                },
            }
        }

        if !diagnostics.is_empty() {
            return Err(diagnostics);
        }
        Ok(ASTModule {
            path: self.path.clone(),
            nodes: module.into_boxed_slice(),
        })
    }

    fn parse_stmt(&mut self) -> Result<ASTNode, Diagnostic> {
        let mut node = self.parse_expr(0)?;

        let mut advance = false;
        if let Some(Token { ty: TokenType::Semicolon, span, .. }) = self.tokens.get(self.pos) {
            advance = true;
            let span = node.span.start..span.end;
            node = ASTNode {
                ty: ASTNodeType::Semi(Box::new(node)),
                span: span.into(),
            };
        }

        if advance { self.pos += 1 }

        Ok(node)
    }

    fn parse_expr(&mut self, min_bp: u32) -> Result<ASTNode, Diagnostic> {
        let mut lhs = self.parse_primary()?;
        
        while let Some(Token { ty: TokenType::Operator(op), .. }) = self.tokens.get(self.pos).cloned() {
            if !op.is_infix() { break }
            let (lbp, rbp) = op.precedence();
            if lbp < min_bp { break }
            self.pos += 1;

            let rhs = self.parse_expr(rbp)?;

            let span = lhs.span.start..rhs.span.end;
            lhs = ASTNode {
                ty: ASTNodeType::BinaryOp { op, lhs: Box::new(lhs), rhs: Box::new(rhs), op_tys: None },
                span: span.into(),
            };
        }

        Ok(lhs)
    }

    fn parse_primary(&mut self) -> Result<ASTNode, Diagnostic> {
        let token = if let Some(token) = self.tokens.get(self.pos).cloned() {
            token
        } else {
            return Err(Diagnostic::new(
                self.path.display(),
                "Expected expression, found end of input",
                self.tokens.last().unwrap().span.clone()
            ));
        };

        match token.ty {
            TokenType::IntLit => {
                self.pos += 1;
                Ok(ASTNode {
                    ty: ASTNodeType::IntLit( unsafe { token.lit.unwrap().int_num } ),
                    span: token.span,
                })
            },
            TokenType::FloatLit => {
                self.pos += 1;
                Ok(ASTNode {
                    ty: ASTNodeType::FloatLit( unsafe { token.lit.unwrap().float_num } ),
                    span: token.span,
                })
            },
            TokenType::StringLit => {
                self.pos += 1;
                Ok(ASTNode {
                    ty: ASTNodeType::StringLit(token.lex.to_string()),
                    span: token.span,
                })
            },
            TokenType::True => {
                self.pos += 1;
                Ok(ASTNode {
                    ty: ASTNodeType::Boolean(true),
                    span: token.span,
                })
            },
            TokenType::False => {
                self.pos += 1;
                Ok(ASTNode {
                    ty: ASTNodeType::Boolean(false),
                    span: token.span,
                })
            },
            TokenType::Identifier => {
                self.pos += 1;
                Ok(ASTNode {
                    ty: ASTNodeType::Identifier(token.lex.to_string()),
                    span: token.span,
                })
            },
            TokenType::Operator(op) if op.is_prefix() => {
                self.pos += 1;
                let operand = self.parse_primary()?;
                let span = token.span.start..operand.span.end;
                Ok(ASTNode {
                    ty: ASTNodeType::UnaryOp { op, operand: Box::new(operand), op_ty: None },
                    span: span.into(),
                })
            },
            TokenType::LParen => {
                self.pos += 1;
                if let Some(Token { ty: TokenType::RParen, span, .. }) = self.tokens.get(self.pos) {
                    let span = token.span.start..span.end;
                    Ok(ASTNode {
                        ty: ASTNodeType::Unit,
                        span: span.into(),
                    })
                } else {
                    Ok(self.parse_expr(0)?)
                }
            },
            TokenType::LCurly => self.parse_block(),
            TokenType::Let => self.parse_let(),
            TokenType::Var => self.parse_var(),
            TokenType::If => self.parse_if(),
            TokenType::While => self.parse_while(),
            _ => Err(Diagnostic::new(
                self.path.display(),
                format!("Expected expression, found {}", token.err_str()),
                token.span
            )),
        }
    }

    fn parse_block(&mut self) -> Result<ASTNode, Diagnostic> {
        let mut stmt_span = self.tokens.get(self.pos).unwrap().span.clone();
        self.pos += 1;

        let mut stmts = Vec::new();
        while let Some(token) = self.tokens.get(self.pos) {
            if token.ty == TokenType::RCurly { break }

            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
        }

        stmt_span.end = self.expect(TokenType::RCurly)?.span.end;

        Ok(ASTNode {
            ty: ASTNodeType::Block(stmts),
            span: stmt_span,
        })
    }

    fn parse_let(&mut self) -> Result<ASTNode, Diagnostic> {
        let mut stmt_span = self.tokens.get(self.pos).unwrap().span.clone();
        self.pos += 1;

        let ident = self.expect(TokenType::Identifier)?;
        let name = ident.lex.to_string();
        stmt_span.end = ident.span.end;

        if self.expect(TokenType::LParen).is_ok() {
            let mut args = Vec::new();
            while let Some(tok) = self.tokens.get(self.pos) {
                if tok.ty == TokenType::RParen { break }
                let ident = self.expect(TokenType::Identifier)?;
                let mut arg_span = ident.span;
                let name = ident.lex.to_string();
                if self.expect(TokenType::Comma).is_ok() {
                    let ty = self.parse_type()?;
                    arg_span.end = ty.span.end;
                    args.push((name, Some(ty), arg_span));
                } else {
                    args.push((name, None, arg_span));
                }
                if self.expect(TokenType::Comma).is_err() {
                    break
                }
            }
            self.expect(TokenType::RParen)?;
            let return_ty = if self.expect(TokenType::Comma).is_ok() {
                Some(self.parse_type()?)
            } else {
                None
            };
            self.expect(TokenType::Operator(Operator::Assign))?;
            let body = self.parse_expr(0)?;
            stmt_span.end = body.span.end;

            return Ok(ASTNode {
                ty: ASTNodeType::FunDef { name, args, return_ty, body: Box::new(body) },
                span: stmt_span,
            });
        }

        let ty = if self.expect(TokenType::Colon).is_ok() {
            let expr = self.parse_type()?;
            stmt_span.end = expr.span.end;
            Some(expr)
        } else {
            None
        };

        let init = if self.expect(TokenType::Operator(Operator::Assign)).is_ok() {
            let expr = self.parse_expr(0)?;
            stmt_span.end = expr.span.end;
            Some(expr)
        } else {
            None
        };

        Ok(ASTNode {
            ty: ASTNodeType::LetDecl {
                name, ty,
                init: init.map(|expr| Box::new(expr))
            },
            span: stmt_span,
        })
    }

    fn parse_var(&mut self) -> Result<ASTNode, Diagnostic> {
        let mut stmt_span = self.tokens.get(self.pos).unwrap().span.clone();
        self.pos += 1;

        let ident = self.expect(TokenType::Identifier)?;
        let name = ident.lex.to_string();
        stmt_span.end = ident.span.end;

        let ty = if self.expect(TokenType::Colon).is_ok() {
            let expr = self.parse_type()?;
            stmt_span.end = expr.span.end;
            Some(expr)
        } else {
            None
        };

        let init = if self.expect(TokenType::Operator(Operator::Assign)).is_ok() {
            let expr = self.parse_expr(0)?;
            stmt_span.end = expr.span.end;
            Some(expr)
        } else {
            None
        };

        Ok(ASTNode {
            ty: ASTNodeType::VarDecl {
                name, ty,
                init: init.map(|expr| Box::new(expr))
            },
            span: stmt_span,
        })
    }

    fn parse_if(&mut self) -> Result<ASTNode, Diagnostic> {
        let mut stmt_span = self.tokens.get(self.pos).unwrap().span.clone();
        self.pos += 1;

        let condition = self.parse_expr(0)?;
        self.expect(TokenType::Then)?;

        let then_body = self.parse_expr(0)?;
        stmt_span.end = then_body.span.end;

        let else_body = if self.expect(TokenType::Else).is_ok() {
            let expr = self.parse_expr(0)?;
            stmt_span.end = expr.span.end;
            Some(expr)
        } else {
            None
        };

        Ok(ASTNode {
            ty: ASTNodeType::If {
                condition: Box::new(condition),
                then_body: Box::new(then_body),
                else_body: else_body.map(|expr| Box::new(expr)),
            },
            span: stmt_span,
        })
    }

    fn parse_while(&mut self) -> Result<ASTNode, Diagnostic> {
        let mut stmt_span = self.tokens.get(self.pos).unwrap().span.clone();
        self.pos += 1;

        let condition = self.parse_expr(0)?;
        self.expect(TokenType::Do)?;

        let body = self.parse_expr(0)?;
        stmt_span.end = body.span.end;

        Ok(ASTNode {
            ty: ASTNodeType::While {
                condition: Box::new(condition),
                body: Box::new(body),
            },
            span: stmt_span,
        })
    }

    fn parse_type(&mut self) -> Result<FrontendType, Diagnostic> {
        let token = if let Some(token) = self.tokens.get(self.pos).cloned() {
            token
        } else {
            return Err(Diagnostic::new(
                self.path.display(),
                "Expected type, found end of input",
                self.tokens.last().unwrap().span.clone()
            ));
        };

        match token.ty {
            TokenType::Identifier => {
                self.pos += 1;
                Ok(FrontendType {
                    ty: FrontendTypeType::Identifier(token.lex.to_string()),
                    span: token.span,
                })
            },
            TokenType::LParen => {
                self.pos += 1;
                if let Some(Token { ty: TokenType::RParen, span, .. }) = self.tokens.get(self.pos) {
                    let span = token.span.start..span.end;
                    Ok(FrontendType { ty: FrontendTypeType::Unit, span: span.into() })
                } else {
                    Ok(self.parse_type()?)
                }
            },
            TokenType::LSquare => {
                self.pos += 1;
                let inner_ty = self.parse_type()?;
                self.expect(TokenType::RSquare)?;
                Ok(FrontendType {
                    ty: FrontendTypeType::Vector(Box::new(inner_ty)),
                    span: token.span,
                })
            },
            _ => Err(Diagnostic::new(
                self.path.display(),
                format!("Expected type, found {}", token.err_str()),
                self.tokens.last().unwrap().span.clone()
            )),
        }
    }

    /*
    fn parse_pattern(&mut self) -> Result<Pattern, Diagnostic> {
        let token = if let Some(token) = self.tokens.get(self.pos).cloned() {
            token
        } else {
            return Err(Diagnostic::new(
                &self.path,
                "Expected pattern, found end of input",
                self.tokens.last().unwrap().span.clone()
            ));
        };

        match token.ty {
            TokenType::IntLit => {
                self.pos += 1;
                Ok(Pattern {
                    ty: PatternType::Literal(PatternLiteral::Integer( unsafe { token.lit.unwrap().int_num } )),
                    span: token.span,
                })
            },
            TokenType::FloatLit => {
                self.pos += 1;
                Ok(Pattern {
                    ty: PatternType::Literal(PatternLiteral::Float( unsafe { token.lit.unwrap().float_num } )),
                    span: token.span,
                })
            },
            TokenType::True => {
                self.pos += 1;
                Ok(Pattern {
                    ty: PatternType::Literal(PatternLiteral::Boolean(true)),
                    span: token.span,
                })
            },
            TokenType::False => {
                self.pos += 1;
                Ok(Pattern {
                    ty: PatternType::Literal(PatternLiteral::Boolean(false)),
                    span: token.span,
                })
            },
            TokenType::Identifier => {
                self.pos += 1;
                Ok(Pattern {
                    ty: PatternType::Identifier(token.lex.to_string()),
                    span: token.span,
                })
            },
            _ => Err(Diagnostic::new(
                &self.path,
                format!("Expected pattern, found {}", token.err_str()),
                self.tokens.last().unwrap().span.clone()
            )),
        }
    }*/

    fn expect(&mut self, expected: TokenType) -> Result<Token, Diagnostic> {
        if let Some(token) = self.tokens.get(self.pos).cloned() {
            if token.ty == expected {
                self.pos += 1;
                Ok(token)
            } else {
                Err(Diagnostic::new(
                    self.path.display(),
                    format!("Expected {}, found {}", expected.err_str(), token.err_str()),
                    token.span
                ))
            }
        } else {
            Err(Diagnostic::new(
                self.path.display(),
                format!("Expected {}, found end of input", expected.err_str()),
                self.tokens.last().unwrap().span.clone()
            ))
        }
    }
}