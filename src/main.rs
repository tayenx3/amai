mod lexer;
mod parser;
mod semantic_checker;
mod codegen;
mod vm;
mod common;
mod cli;
mod diagnostic;
mod tools;

use crate::cli::{Cli, Command};
use colored::Colorize;

fn main() {
    use clap::Parser;

    let cli = Cli::parse();
    if let Err(err) = run_cli(cli) {
        println!("{err}");
    }
}

pub fn run_cli(cli: Cli) -> Result<(), String> {
    match cli.command {
        Command::Run { input, show_bytecode } => match input {
            Some(path) => run_path(&path, show_bytecode),
            None => Err(format!("{}: Project runs are not supported for now", "error".bright_red().bold())),
        }
    }
}

pub fn run_path(input: &str, show_bytecode: bool) -> Result<(), String> {
    use std::fs;

    let contents = fs::read_to_string(&input)
        .map_err(|_| format!("{}: No such file: `{}`", "error".bright_red().bold(), input.italic()))?
        .replace("\r\n", "\n");

    let tokens = match lexer::lex(&input, &contents) {
        Ok(toks) => toks,
        Err(err) => {
            let lines = contents.lines().collect::<Vec<_>>();
            let line_starts = line_starts(&contents);
            return Err(err.display(&line_starts, &lines));
        },
    };

    let mut parser = parser::Parser::new(&input, &tokens);
    let mut ast = match parser.parse() {
        Ok(ast) => ast,
        Err(err) => {
            let lines = contents.lines().collect::<Vec<_>>();
            let line_starts = line_starts(&contents);
            return Err(
                err
                    .iter()
                    .map(|d| d.display(&line_starts, &lines))
                    .collect::<Vec<_>>().join("\n")
            );
        },
    };

    let mut sch = semantic_checker::SemanticChecker::new(ast.path.clone());

    sch.validate(&mut ast).map_err(|errors| {
        let lines = contents.lines().collect::<Vec<_>>();
        let line_starts = line_starts(&contents);
        errors
            .iter()
            .map(|d| d.display(&line_starts, &lines))
            .collect::<Vec<_>>().join("\n")
    })?;

    let mut astc = codegen::ASTCompiler::new();
    let f = astc.compile(&ast);
    
    if show_bytecode {
        let disassembled = tools::bytecode_disassembler::disassemble(&f.iter().map(|s| s.0).collect::<Vec<_>>());
        println!("{disassembled}");
    }
    
    let mut vm = vm::AmaiVM::new(false);
    vm.precompile_constants(astc.constants.into_boxed_slice());

    vm.add_function(f.into_boxed_slice());
    vm.call_function(0, Box::new([]));
    vm.run().map_err(|(err, span)| {
        let diag = diagnostic::Diagnostic::new(&input, format!("{err}. Traced error happened here:"), span);
        let lines = contents.lines().collect::<Vec<_>>();
        let line_starts = line_starts(&contents);
        diag.display(&line_starts, &lines)
    })?;
    let frame = vm.frames.last().unwrap();
    vm.return_function();

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