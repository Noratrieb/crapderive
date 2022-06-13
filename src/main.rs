use std::io;

mod parser;

fn main() -> Result<(), io::Error> {
    let file = std::fs::read_to_string("test.asm")?;
    parser::run(&file);
    Ok(())
}
