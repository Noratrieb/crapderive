use dbg_pls::DebugPls;
use logos::Span;

#[derive(Debug, DebugPls)]
pub struct CompilerError {
    pub msg: String,
    pub span: Span,
}

pub type Result<T> = std::result::Result<T, CompilerError>;
