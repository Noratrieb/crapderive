use std::io;

mod error;
mod ir;
mod parser;

fn main() -> Result<(), io::Error> {
    let file = std::fs::read_to_string("./test.at")?;
    let result = parser::parse(&file);

    match result {
        Ok(ast) => {
            dbg_pls::color!(ast);
        }
        Err(error) => error::report(error, "test.at", &file),
    }

    Ok(())
}
