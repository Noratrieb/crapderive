use std::io;

mod error;
mod parser;

fn main() -> Result<(), io::Error> {
    let file = std::fs::read_to_string("./test.at")?;
    let result = parser::parse(&file);
    let _ = dbg_pls::color!(result);
    Ok(())
}
