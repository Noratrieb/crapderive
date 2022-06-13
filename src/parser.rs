use dbg_pls::DebugPls;
use logos::{Lexer, Logos};

#[derive(Debug, Clone, PartialEq, Eq, Logos, DebugPls)]
pub enum Token<'a> {
    #[token("mov")]
    Mov,
    #[token("jmp")]
    Jmp,
    #[token("je")]
    Je,
    #[token("cmp")]
    Cmp,
    #[token("add")]
    Add,
    #[token("sub")]
    Sub,
    #[token("mul")]
    Mul,
    #[token("div")]
    Div,
    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClose,
    #[token(",")]
    Comma,
    #[regex(r"\w+:")]
    Label(&'a str),
    #[regex(r"[0-9]+", |lex| lex.slice().parse::<u64>())]
    Number(u64),
    #[regex(r"[a-zA-Z]\w+")]
    Word(&'a str),

    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[regex("//[^\n]*", logos::skip)]
    #[error]
    Error,
}

pub fn lex(src: &str) -> Lexer<'_, Token<'_>> {
    <Token as Logos>::lexer(src)
}

pub fn run(src: &str) {
    let tokens = lex(src).collect::<Vec<_>>();
    dbg_pls::color!(tokens);
}
