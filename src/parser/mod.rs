pub mod ast;
pub mod pattern;

use crate::{diagnostic::Diagnostic, operator::Operator};
use super::lexer::token::*;
use ast::*;
use pattern::*;

pub struct Parser<'p> {
    path: String,
    tokens: &'p [Token],
    pos: usize,
}

impl<'p> Parser<'p> {
    pub fn new<P: AsRef<str>>(path: P, tokens: &'p [Token]) -> Parser<'p> {
        Parser {
            path: path.as_ref().to_string(),
            tokens,
            pos: 0,
        }
    }

    pub fn current(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    pub fn parse(&mut self) -> Result<AmaiASTModule, Vec<Diagnostic>> {
        use std::path::PathBuf;

        let module_name = PathBuf::from(&self.path)
            .file_stem()
            .unwrap()
            .display()
            .to_string();

        let mut module = AmaiASTModule {
            name: module_name,
            nodes: Vec::new(),
        };
        if self.tokens.is_empty() {
            return Ok(module);
        }
        let mut diagnostics = Vec::new();

        while self.current().is_some() {
            let stmt = self.parse_stmt();
            match stmt {
                Ok(node) => module.nodes.push(node),
                Err(err) => {
                    diagnostics.push(err);
                    while let Some(token) = self.current() {
                        // syncronize
                        if token.kind == TokenKind::Semicolon
                            || token.kind == TokenKind::RCurly
                            || token.kind == TokenKind::RParen
                            || token.kind == TokenKind::RSquare
                            || token.kind == TokenKind::LCurly
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
        Ok(module)
    }

    fn parse_stmt(&mut self) -> Result<AmaiASTNode, Diagnostic> {
        let mut node = self.parse_expr(0)?;

        let mut advance = false;
        if let Some(Token { kind: TokenKind::Semicolon, span, .. }) = self.current() {
            advance = true;
            let span = node.span.start..span.end;
            node = AmaiASTNode {
                kind: AmaiASTNodeKind::Semi(Box::new(node)),
                span,
            };
        }

        if advance { self.pos += 1 }

        Ok(node)
    }

    fn parse_expr(&mut self, min_bp: u32) -> Result<AmaiASTNode, Diagnostic> {
        let mut lhs = self.parse_primary()?;
        
        while let Some(Token { kind: TokenKind::Operator(op), .. }) = self.current().cloned() {
            if !op.is_infix() { break }
            let (lbp, rbp) = op.precedence();
            if lbp < min_bp { break }
            self.pos += 1;

            let rhs = self.parse_expr(rbp)?;

            let span = lhs.span.start..rhs.span.end;
            lhs = AmaiASTNode {
                kind: AmaiASTNodeKind::BinaryOp { op, lhs: Box::new(lhs), rhs: Box::new(rhs) },
                span,
            };
        }

        Ok(lhs)
    }

    fn parse_primary(&mut self) -> Result<AmaiASTNode, Diagnostic> {
        let token = if let Some(token) = self.current().cloned() {
            token
        } else {
            return Err(Diagnostic::new(
                &self.path,
                "Expected expression, found end of input",
                self.tokens.last().unwrap().span.clone()
            ));
        };

        match token.kind {
            TokenKind::IntLit => {
                self.pos += 1;
                Ok(AmaiASTNode {
                    kind: AmaiASTNodeKind::IntLit( unsafe { token.literal.unwrap().integer } ),
                    span: token.span.clone(),
                })
            },
            TokenKind::FloatLit => {
                self.pos += 1;
                Ok(AmaiASTNode {
                    kind: AmaiASTNodeKind::FloatLit( unsafe { token.literal.unwrap().float } ),
                    span: token.span.clone(),
                })
            },
            TokenKind::True => {
                self.pos += 1;
                Ok(AmaiASTNode {
                    kind: AmaiASTNodeKind::Boolean(true),
                    span: token.span.clone(),
                })
            },
            TokenKind::False => {
                self.pos += 1;
                Ok(AmaiASTNode {
                    kind: AmaiASTNodeKind::Boolean(false),
                    span: token.span.clone(),
                })
            },
            TokenKind::Identifier => {
                self.pos += 1;
                Ok(AmaiASTNode {
                    kind: AmaiASTNodeKind::Identifier(token.lexeme),
                    span: token.span.clone(),
                })
            },
            TokenKind::Operator(op) if op.is_prefix() => {
                self.pos += 1;
                let operand = self.parse_primary()?;
                let span = token.span.start..operand.span.end;
                Ok(AmaiASTNode {
                    kind: AmaiASTNodeKind::UnaryOp { op, operand: Box::new(operand) },
                    span,
                })
            },
            TokenKind::LParen => self.parse_paren(),
            TokenKind::LCurly => self.parse_block(),
            TokenKind::Let => self.parse_let(),
            TokenKind::Var => self.parse_var(),
            TokenKind::If => self.parse_if(),
            TokenKind::While => self.parse_while(),
            _ => Err(Diagnostic::new(
                &self.path,
                format!("Expected expression, found {}", token.err_str()),
                token.span
            )),
        }
    }

    fn parse_paren(&mut self) -> Result<AmaiASTNode, Diagnostic> {
        let mut stmt_span = self.current().unwrap().span.clone();
        self.pos += 1;

        let mut in_tuple = false;
        let mut items = Vec::new();
        while let Some(token) = self.current() {
            if token.kind == TokenKind::RParen { break }

            let expr = self.parse_expr(0)?;
            items.push(expr);

            if self.expect(TokenKind::Comma).is_ok() {
                in_tuple = true;
            } else {
                break;
            }
        }

        stmt_span.end = self.expect(TokenKind::RParen)?.span.end;
        
        if in_tuple {
            Ok(AmaiASTNode {
                kind: AmaiASTNodeKind::Tuple(items),
                span: stmt_span,
            })
        } else if items.len() == 1 {
            Ok(items[0].clone())
        } else {
            Ok(AmaiASTNode {
                kind: AmaiASTNodeKind::Unit,
                span: stmt_span,
            })
        }
    }

    fn parse_block(&mut self) -> Result<AmaiASTNode, Diagnostic> {
        let mut stmt_span = self.current().unwrap().span.clone();
        self.pos += 1;

        let mut stmts = Vec::new();
        while let Some(token) = self.current() {
            if token.kind == TokenKind::RCurly { break }

            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
        }

        stmt_span.end = self.expect(TokenKind::RCurly)?.span.end;

        Ok(AmaiASTNode {
            kind: AmaiASTNodeKind::Block(stmts),
            span: stmt_span,
        })
    }

    fn parse_let(&mut self) -> Result<AmaiASTNode, Diagnostic> {
        let mut stmt_span = self.current().unwrap().span.clone();
        self.pos += 1;

        let pat = self.parse_pattern()?;
        stmt_span.end = pat.span.end;

        let init = if self.expect(TokenKind::Operator(Operator::Assign)).is_ok() {
            let expr = self.parse_expr(0)?;
            stmt_span.end = expr.span.end;
            Some(expr)
        } else {
            None
        };

        Ok(AmaiASTNode {
            kind: AmaiASTNodeKind::LetDecl {
                pat,
                init: init.map(|expr| Box::new(expr))
            },
            span: stmt_span,
        })
    }

    fn parse_var(&mut self) -> Result<AmaiASTNode, Diagnostic> {
        let mut stmt_span = self.current().unwrap().span.clone();
        self.pos += 1;

        let pat = self.parse_pattern()?;
        stmt_span.end = pat.span.end;

        let init = if self.expect(TokenKind::Operator(Operator::Assign)).is_ok() {
            let expr = self.parse_expr(0)?;
            stmt_span.end = expr.span.end;
            Some(expr)
        } else {
            None
        };

        Ok(AmaiASTNode {
            kind: AmaiASTNodeKind::VarDecl {
                pat,
                init: init.map(|expr| Box::new(expr))
            },
            span: stmt_span,
        })
    }

    fn parse_if(&mut self) -> Result<AmaiASTNode, Diagnostic> {
        let mut stmt_span = self.current().unwrap().span.clone();
        self.pos += 1;

        let condition = self.parse_expr(0)?;
        self.expect(TokenKind::Colon)?;

        let then_body = self.parse_expr(0)?;
        stmt_span.end = then_body.span.end;

        let else_body = if self.expect(TokenKind::Else).is_ok() {
            self.expect(TokenKind::Colon)?;
            let expr = self.parse_expr(0)?;
            stmt_span.end = expr.span.end;
            Some(expr)
        } else {
            None
        };

        Ok(AmaiASTNode {
            kind: AmaiASTNodeKind::If {
                condition: Box::new(condition),
                then_body: Box::new(then_body),
                else_body: else_body.map(|expr| Box::new(expr)),
            },
            span: stmt_span,
        })
    }

    fn parse_while(&mut self) -> Result<AmaiASTNode, Diagnostic> {
        let mut stmt_span = self.current().unwrap().span.clone();
        self.pos += 1;

        let condition = self.parse_expr(0)?;
        self.expect(TokenKind::Colon)?;

        let body = self.parse_expr(0)?;
        stmt_span.end = body.span.end;

        Ok(AmaiASTNode {
            kind: AmaiASTNodeKind::While {
                condition: Box::new(condition),
                body: Box::new(body),
            },
            span: stmt_span,
        })
    }

    fn parse_pattern(&mut self) -> Result<Pattern, Diagnostic> {
        let token = if let Some(token) = self.current().cloned() {
            token
        } else {
            return Err(Diagnostic::new(
                &self.path,
                "Expected pattern, found end of input",
                self.tokens.last().unwrap().span.clone()
            ));
        };

        match token.kind {
            TokenKind::IntLit => {
                self.pos += 1;
                Ok(Pattern {
                    kind: PatternKind::Literal(PatternLiteral::Integer( unsafe { token.literal.unwrap().integer } )),
                    span: token.span.clone(),
                })
            },
            TokenKind::FloatLit => {
                self.pos += 1;
                Ok(Pattern {
                    kind: PatternKind::Literal(PatternLiteral::Float( unsafe { token.literal.unwrap().float } )),
                    span: token.span.clone(),
                })
            },
            TokenKind::True => {
                self.pos += 1;
                Ok(Pattern {
                    kind: PatternKind::Literal(PatternLiteral::Boolean(true)),
                    span: token.span.clone(),
                })
            },
            TokenKind::False => {
                self.pos += 1;
                Ok(Pattern {
                    kind: PatternKind::Literal(PatternLiteral::Boolean(false)),
                    span: token.span.clone(),
                })
            },
            TokenKind::Identifier => {
                self.pos += 1;
                Ok(Pattern {
                    kind: PatternKind::Identifier(token.lexeme),
                    span: token.span.clone(),
                })
            },
            _ => Err(Diagnostic::new(
                &self.path,
                format!("Expected pattern, found {}", token.err_str()),
                self.tokens.last().unwrap().span.clone()
            )),
        }
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Token, Diagnostic> {
        if let Some(token) = self.current().cloned() {
            if token.kind == expected {
                self.pos += 1;
                Ok(token)
            } else {
                Err(Diagnostic::new(
                    &self.path,
                    format!("Expected {}, found {}", expected.err_str(), token.err_str()),
                    token.span
                ))
            }
        } else {
            Err(Diagnostic::new(
                &self.path,
                format!("Expected {}, found end of input", expected.err_str()),
                self.tokens.last().unwrap().span.clone()
            ))
        }
    }
}