pub mod token;

use token::*;
use crate::{Span, operator::Operator};

const MULTICHAR_SYMBOLS: &[&str] = &[
    "+=", "-=", "*=", "/=", "%=",
    "==", "!=", ">=", "<=",
    "||", "&&", "..", "..=",
    "<<", ">>", "++",
];

fn classify(lex: &str, span: Span) -> Token {
    let (kind, literal) = match lex {
        "func" => (TokenKind::Func, None),
        "let" => (TokenKind::Let, None),
        "var" => (TokenKind::Var, None),
        "if" => (TokenKind::If, None),
        "else" => (TokenKind::Else, None),
        "while" => (TokenKind::While, None),
        "for" => (TokenKind::For, None),
        "in" => (TokenKind::In, None),
        "true" => (TokenKind::True, None),
        "false" => (TokenKind::False, None),
        "(" => (TokenKind::LParen, None),
        ")" => (TokenKind::RParen, None),
        "[" => (TokenKind::LSquare, None),
        "]" => (TokenKind::RSquare, None),
        "{" => (TokenKind::LCurly, None),
        "}" => (TokenKind::RCurly, None),
        ";" => (TokenKind::Semicolon, None),
        ":" => (TokenKind::Colon, None),
        "," => (TokenKind::Comma, None),
        "." => (TokenKind::Dot, None),
        "?" => (TokenKind::QuestionMark, None),
        "#" => (TokenKind::Hashtag, None),
        "+" => (TokenKind::Operator(Operator::Plus), None),
        "-" => (TokenKind::Operator(Operator::Minus), None),
        "*" => (TokenKind::Operator(Operator::Star), None),
        "/" => (TokenKind::Operator(Operator::Slash), None),
        "%" => (TokenKind::Operator(Operator::Modulo), None),
        "=" => (TokenKind::Operator(Operator::Assign), None),
        "+=" => (TokenKind::Operator(Operator::PlusAssign), None),
        "-=" => (TokenKind::Operator(Operator::MinusAssign), None),
        "*=" => (TokenKind::Operator(Operator::StarAssign), None),
        "/=" => (TokenKind::Operator(Operator::SlashAssign), None),
        "%=" => (TokenKind::Operator(Operator::ModuloAssign), None),
        "==" => (TokenKind::Operator(Operator::Eq), None),
        "!=" => (TokenKind::Operator(Operator::Ne), None),
        ">" => (TokenKind::Operator(Operator::Gt), None),
        "<" => (TokenKind::Operator(Operator::Lt), None),
        ">=" => (TokenKind::Operator(Operator::Ge), None),
        "<=" => (TokenKind::Operator(Operator::LogOr), None),
        "|" => (TokenKind::Operator(Operator::Pipe), None),
        "||" => (TokenKind::Operator(Operator::Ampersand), None),
        "&" => (TokenKind::Operator(Operator::LogAnd), None),
        "&&" => (TokenKind::Operator(Operator::Le), None),
        "^" => (TokenKind::Operator(Operator::Caret), None),
        "~" => (TokenKind::Operator(Operator::Tilde), None),
        "!" => (TokenKind::Operator(Operator::Bang), None),
        ".." => (TokenKind::Operator(Operator::Range), None),
        "..=" => (TokenKind::Operator(Operator::RangeInclus), None),
        "<<" => (TokenKind::Operator(Operator::Lsh), None),
        ">>>" => (TokenKind::Operator(Operator::LRsh), None),
        ">>" => (TokenKind::Operator(Operator::ARsh), None),
        "++" => (TokenKind::Operator(Operator::Concat), None),
        _ => if let Ok(int) = lex.parse::<i64>()
            && lex.chars().all(|c| c.is_ascii_digit() || c == '_')
        {
            (TokenKind::IntLit, Some(LexLiteral { integer: int }))
        } else if let Ok(float) = lex.parse::<f64>()
            && lex.chars().all(|c| c.is_ascii_digit() || c == '_' || c == '.')
        {
            (TokenKind::FloatLit, Some(LexLiteral { float: float }))
        } else {
            (TokenKind::Identifier, None)
        },
    };

    Token {
        kind,
        lexeme: lex.to_string(),
        literal,
        span,
    }
}

pub fn tokenize(src: &str) -> Vec<Token> {
    let chars = src.chars().collect::<Vec<_>>();

    let mut tokens = Vec::new();
    let mut current = String::new();
    let mut pos = 0usize;
    let mut tok_start = 0usize;

    'lex_loop : while pos < chars.len() {
        let ch = chars[pos];

        match ch {
            '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => {
                if current.is_empty() {
                    tok_start = pos;
                }
                current.push(ch);
            },
            _ if " \t\n\r".contains(ch) => if !current.is_empty() {
                tokens.push(classify(&current, tok_start..pos));
                tok_start = pos+1;
                current.clear();
            },
            _ => {
                if ch == '.' {
                    if current.chars().all(|c| c.is_ascii_digit() || c == '_') {
                        current.push(ch);
                        pos += 1;

                        continue;
                    } else if current.is_empty() && pos+1 < chars.len() {
                        if chars[pos].is_ascii_digit() {
                            current.push(ch);
                            pos += 1;

                            continue;
                        }
                    }
                }
                if !current.is_empty() {
                    tokens.push(classify(&current, tok_start..pos));
                    current.clear();
                }

                tok_start = pos;

                for symbol in MULTICHAR_SYMBOLS {
                    if pos + symbol.len() - 1 >= chars.len() {
                        continue
                    }

                    let mut built_symbol = String::new();
                    for i in 0..symbol.len() {
                        built_symbol.push(chars[pos+i]);
                    }

                    if *symbol == built_symbol {
                        tokens.push(classify(
                            symbol,
                            tok_start..(pos + symbol.len())
                        ));
                        pos += symbol.len();
                        continue 'lex_loop;
                    }
                }

                tokens.push(classify(
                    &ch.to_string(),
                    tok_start..(pos + 1),
                ));

                tok_start = pos+1;
            }
        }

        pos += 1;
    }

    if !current.is_empty() {
        tokens.push(classify(&current, tok_start..pos));
    }

    tokens
}