pub mod token;

use token::*;
use crate::{common::{Operator, Span}, diagnostic::Diagnostic};

const MULTICHAR_SYMBOLS: &[&str] = &[
    "+=", "-=", "*=", "/=", "%=",
    "==", "!=", ">=", "<=",
    "||", "&&", "..", "..=",
    "<<", ">>", "++", "->",
];

fn parse_int(string: &str) -> Option<i64> {
    if string.starts_with("0x") {
        let new = string.replace("_", "").to_ascii_lowercase();
        i64::from_str_radix(&new[2..], 16).ok()
    } else if string.starts_with("0b") {
        let new = string.replace("_", "").to_ascii_lowercase();
        i64::from_str_radix(&new[2..], 2).ok()
    } else if string.starts_with("0o") {
        let new = string.replace("_", "").to_ascii_lowercase();
        i64::from_str_radix(&new[2..], 8).ok()
    } else {
        let new = string.replace("_", "").to_ascii_lowercase();
        i64::from_str_radix(&new, 10).ok()
    }
}

fn parse_float(string: &str) -> Option<f64> {
    let mut dot_found = false;
    let mut float: f64 = 0.0;
    let mut fraction_idx = 0.1;

    for ch in string.chars() {
        if ch == '.' {
            if dot_found { return None }
            dot_found = true;
            continue;
        }
        if ch == '_' { continue }
        if !ch.is_ascii_digit() {
            return None;
        }

        let i = (ch as u8 - b'0') as f64;
        if dot_found {
            float += fraction_idx * i;
            fraction_idx *= 0.1;
        } else {
            float = float * 10.0 + i;
        }
    }

    if !dot_found {
        return None;
    }

    Some(float)
}

fn classify<'lex>(lex: &'lex str, span: Span) -> Option<Token<'lex>> {
    let (ty, lit) = match lex {
        "let" => (TokenType::Let, None),
        "var" => (TokenType::Var, None),
        "if" => (TokenType::If, None),
        "else" => (TokenType::Else, None),
        "while" => (TokenType::While, None),
        "for" => (TokenType::For, None),
        "in" => (TokenType::In, None),
        "return" => (TokenType::Return, None),
        "extern" => (TokenType::Extern, None),
        "export" => (TokenType::Export, None),
        "do" => (TokenType::Do, None),
        "then" => (TokenType::Then, None),
        "with" => (TokenType::With, None),
        "true" => (TokenType::True, None),
        "false" => (TokenType::False, None),
        "(" => (TokenType::LParen, None),
        ")" => (TokenType::RParen, None),
        "[" => (TokenType::LSquare, None),
        "]" => (TokenType::RSquare, None),
        "{" => (TokenType::LCurly, None),
        "}" => (TokenType::RCurly, None),
        ";" => (TokenType::Semicolon, None),
        ":" => (TokenType::Colon, None),
        "," => (TokenType::Comma, None),
        "." => (TokenType::Dot, None),
        "?" => (TokenType::QuestionMark, None),
        "#" => (TokenType::Hashtag, None),
        "@" => (TokenType::At, None),
        "->" => (TokenType::Arrow, None),
        "+" => (TokenType::Operator(Operator::Plus), None),
        "-" => (TokenType::Operator(Operator::Minus), None),
        "*" => (TokenType::Operator(Operator::Star), None),
        "/" => (TokenType::Operator(Operator::Slash), None),
        "%" => (TokenType::Operator(Operator::Modulo), None),
        "=" => (TokenType::Operator(Operator::Assign), None),
        "+=" => (TokenType::Operator(Operator::PlusAssign), None),
        "-=" => (TokenType::Operator(Operator::MinusAssign), None),
        "*=" => (TokenType::Operator(Operator::StarAssign), None),
        "/=" => (TokenType::Operator(Operator::SlashAssign), None),
        "%=" => (TokenType::Operator(Operator::ModuloAssign), None),
        "==" => (TokenType::Operator(Operator::Eq), None),
        "!=" => (TokenType::Operator(Operator::Ne), None),
        ">" => (TokenType::Operator(Operator::Gt), None),
        "<" => (TokenType::Operator(Operator::Lt), None),
        ">=" => (TokenType::Operator(Operator::Ge), None),
        "<=" => (TokenType::Operator(Operator::Le), None),
        "|" => (TokenType::Operator(Operator::Pipe), None),
        "||" => (TokenType::Operator(Operator::LogOr), None),
        "&" => (TokenType::Operator(Operator::Ampersand), None),
        "&&" => (TokenType::Operator(Operator::LogAnd), None),
        "^" => (TokenType::Operator(Operator::Caret), None),
        "~" => (TokenType::Operator(Operator::Tilde), None),
        "!" => (TokenType::Operator(Operator::Bang), None),
        ".." => (TokenType::Operator(Operator::Range), None),
        "..=" => (TokenType::Operator(Operator::RangeInclus), None),
        "<<" => (TokenType::Operator(Operator::Lsh), None),
        ">>" => (TokenType::Operator(Operator::Rsh), None),
        "++" => (TokenType::Operator(Operator::Concat), None),
        _ => if let Some(n) = parse_int(lex) {
            (TokenType::IntLit, Some(LexLiteral { int_num: n }))
        } else if let Some(n) = parse_float(lex) {
            (TokenType::FloatLit, Some(LexLiteral { float_num: n }))
        } else if lex.chars().all(|c| c == '_' || c.is_alphanumeric()) {
            (TokenType::Identifier, None)
        } else {
            return None;
        },
    };

    Some(Token {
        ty,
        lit,
        lex,
        span,
    })
}

pub fn lex<'lex>(path: &str, source: &'lex str) -> Result<Box<[Token<'lex>]>, Diagnostic> {
    let mut chars = source.char_indices().peekable();
    let len = source.len();

    let mut tokens = Vec::new();
    let mut tok_start = 0usize;
    let mut in_string = false;
    let mut skip = 0usize;

    while let Some((pos, ch)) = chars.next() {
        if skip > 0 {
            skip -= 1;
            continue;
        }

        if in_string {
            if ch == '"' {
                tokens.push(Token {
                    ty: TokenType::StringLit,
                    lit: None,
                    lex: &source[(tok_start + 1)..pos],
                    span: Span::from(tok_start..(pos + 1)),
                });
                tok_start = pos + 1;
                in_string = false;
            }
            continue;
        }

        match ch {
            ' ' | '\r' | '\n' | '\t' => {
                if tok_start < pos {
                    let tok = classify(&source[tok_start..pos], Span::from(tok_start..pos));
                    if let Some(tok) = tok {
                        tokens.push(tok);
                    } else {
                        return Err(
                            Diagnostic::new(
                                path,
                                format!("Unrecognized sequence of characters: `{:?}`", &source[tok_start..pos]),
                                Span::from(tok_start..pos)
                            )
                        );
                    }
                }
            
                tok_start = pos+1;
            },
            c if c.is_alphanumeric() || c == '_' => continue,
            '"' => {
                in_string = true;
                if tok_start < pos {
                    let tok = classify(&source[tok_start..pos], Span::from(tok_start..pos));
                    if let Some(tok) = tok {
                        tokens.push(tok);
                    } else {
                        return Err(
                            Diagnostic::new(
                                path,
                                format!("Unrecognized sequence of characters: `{:?}`", &source[tok_start..pos]),
                                Span::from(tok_start..pos)
                            )
                        );
                    }
                }
                tok_start = pos;
            },
            _ => {
                if ch == '.' {
                    if (&source[tok_start..pos]).chars().all(|c| c.is_ascii_digit() || c == '_') {
                        continue
                    } else if tok_start == pos && pos+1 < len {
                        if ch.is_ascii_digit() {
                            continue
                        }
                    }
                }

                if tok_start < pos {
                    let tok = classify(&source[tok_start..pos], Span::from(tok_start..pos));
                    if let Some(tok) = tok {
                        tokens.push(tok);
                    } else {
                        return Err(
                            Diagnostic::new(
                                path,
                                format!("Unrecognized sequence of characters: `{:?}`", &source[tok_start..pos]),
                                Span::from(tok_start..pos)
                            )
                        );
                    }
                }

                tok_start = pos;

                if ch == '/' {
                    if let Some((_, '/')) = chars.peek() {
                        chars.next();
                        let mut pos = 0;
                        while let Some((p, ch)) = chars.next() {
                            if ch == '\n' { pos = p; break }
                        }
                        tok_start = pos+1;
                        continue;
                    }
                    if let Some((_, '*')) = chars.peek() {
                        chars.next();
                        let mut terminated = false;
                        let mut pos = 0;
                        while let Some((p, ch)) = chars.next() {
                            if ch == '*' {
                                if let Some((_, '/')) = chars.peek() {
                                    terminated = true;
                                    chars.next();
                                    pos = p;
                                    break
                                }
                            }
                        }
                        if !terminated {
                            return Err(
                                Diagnostic::new(
                                    path,
                                    format!("Unterminated block comment"),
                                    Span::from(tok_start..pos+1)
                                )
                            );
                        }
                        tok_start = pos+1;
                        continue;
                    }
                }

                let mut continue_main_loop = false;
                for symbol in MULTICHAR_SYMBOLS {
                    if pos + symbol.len() - 1 >= len {
                        continue
                    }
                    
                    let built_symbol = &source[pos..pos + symbol.len()];

                    if *symbol == built_symbol {
                        tokens.push(classify(
                            &source[tok_start..(pos + symbol.len())],
                            Span::from(tok_start..(pos + symbol.len()))
                        ).unwrap());
                        skip = symbol.len() - 1;
                        tok_start = pos + symbol.len();
                        continue_main_loop = true;
                        break;
                    }
                }
                if continue_main_loop { continue }

                let tok = classify(
                    &source[pos..(pos + 1)],
                    Span::from(pos..(pos + 1)),
                );
                if let Some(tok) = tok {
                    tokens.push(tok);
                } else {
                    return Err(
                        Diagnostic::new(
                            path,
                            format!("Unrecognized sequence of characters: `{:?}`", &source[tok_start..pos]),
                            Span::from(tok_start..pos)
                        )
                    );
                }

                tok_start = pos + 1;
            }
        }
    }

    if tok_start < len {
        let tok = classify(&source[tok_start..len], Span::from(tok_start..len));
        if let Some(tok) = tok {
            tokens.push(tok);
        } else {
            return Err(
                Diagnostic::new(
                    path,
                    format!("Unrecognized sequence of characters: `{:?}`", &source[tok_start..len]),
                    Span::from(tok_start..len)
                )
            );
        }
    }

    Ok(tokens.into_boxed_slice())
}