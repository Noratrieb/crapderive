use std::{io, process};

use crate::error::CompilerError;

mod error;
mod interpret;
mod ir;
mod parser;

fn main() -> Result<(), io::Error> {
    let file = std::fs::read_to_string("./test.at")?;
    let result = parser::parse(&file);

    let ast = result.unwrap_or_else(|e| report_and_exit(&file, e));
    dbg_pls::color!(&ast);
    let stmts = ir::compile(ast.into_iter()).unwrap_or_else(|e| report_and_exit(&file, e));
    dbg_pls::color!(&stmts.0);

    interpret::interpret(stmts.0).unwrap_or_else(|e| report_and_exit(&file, e));

    Ok(())
}

fn report_and_exit(file: &str, error: CompilerError) -> ! {
    error::report(error, "test.at", &file);
    process::exit(1);
}
