mod lexer;
mod parser;
mod operator;
mod cli;
mod diagnostic;

use crate::cli::Cli;
use colored::Colorize;

type Span = std::ops::Range<usize>;

fn main() {
    use clap::Parser;

    let cli = Cli::parse();
    if let Err(err) = run(cli) {
        println!("{err}");
    }
}

fn run(cli: Cli) -> Result<(), String> {
    use std::fs;

    let contents = fs::read_to_string(&cli.input)
        .map_err(|_| format!("{}: No such file: {}", "error".bright_red().bold(), cli.input))?
        .replace("\r\n", "\n");

    let tokens = lexer::tokenize(&contents);

    if cli.debug {
        eprintln!("Tokens: [");
        for tok in &tokens {
            eprintln!("\t{},", tok.fmt_span());
        }
        eprintln!("]");
    }

    let mut parser = parser::Parser::new(&cli.input, &tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(err) => {
            let lines = contents.lines().collect::<Vec<_>>();
            let line_starts = line_starts(&contents);
            return Err(
                err
                    .iter()
                    .map(|d| d.display(&line_starts, &lines))
                    .collect::<Vec<_>>().join("\n\n")
            );
        },
    };

    if cli.debug {
        eprintln!("AST: {:#?}", ast.nodes);
    }

    Ok(())
}

fn line_starts(s: &str) -> Vec<usize> {
    let mut indices = vec![0];
    
    for (i, ch) in s.char_indices() {
        if ch == '\n' {
            indices.push(i + 1);
        }
    }

    indices
}