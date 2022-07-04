use std::{fmt::Debug, iter::Peekable};

use dbg_pls::DebugPls;
use logos::{Lexer, Logos, Span};

use crate::error::{CompilerError, Result};

#[derive(Debug, Clone, PartialEq, Eq, Logos, DebugPls)]
pub enum Token<'a> {
    #[token("mov")]
    Mov,
    #[token("movb")]
    Movb,
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
    #[token("int")]
    Int,
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

#[derive(Debug, PartialEq, Eq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

impl DebugPls for Stmt {
    fn fmt(&self, f: dbg_pls::Formatter<'_>) {
        DebugPls::fmt(&self.kind, f)
    }
}

#[derive(Debug, PartialEq, Eq, DebugPls)]
pub enum StmtKind {
    Mov { to: Expr, from: Expr },
    Movb { to: Expr, from: Expr },
    Add { to: Expr, value: Expr },
    Sub { to: Expr, value: Expr },
    Mul { to: Expr, value: Expr },
    Div { to: Expr, value: Expr },
    Int { number: u64 },
    Jmp { to: Expr },
    Je { to: Expr },
    Cmp { lhs: Expr, rhs: Expr },
    Label { name: String },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl DebugPls for Expr {
    fn fmt(&self, f: dbg_pls::Formatter<'_>) {
        DebugPls::fmt(&self.kind, f)
    }
}

#[derive(Debug, PartialEq, Eq, DebugPls)]
pub enum ExprKind {
    Register(u8),
    Number(u64),
    Addr(Box<Expr>),
    Symbol(String),
}

struct Parser<'a, I>
where
    I: Iterator<Item = (Token<'a>, Span)>,
{
    iter: Peekable<I>,
}

impl CompilerError {
    fn not_allowed(span: Span, token: &str) -> Self {
        Self::simple(format!("`{token}` is not allowed here"), span)
    }

    fn invalid_token(span: Span) -> Self {
        Self::simple("Invalid token".to_string(), span)
    }
    fn eof() -> Self {
        Self {
            span: 0..0,
            msg: "Unexpected end of file".to_string(),
            notes: Vec::new(),
            help: None,
        }
    }
}

macro_rules! expect {
    ($self:ident, $token:pat) => {{
        let (next, span) = $self.next()?;
        if let $token = next {
            span
        } else {
            return Err(CompilerError::simple(
                format!(
                    concat!("Expected ", stringify!($token), ", found {:?}"),
                    next,
                ),
                span,
            ));
        }
    }};
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = (Token<'a>, Span)>,
{
    fn program(&mut self) -> Result<Vec<Stmt>> {
        let mut stmts = Vec::new();
        while let Ok(_) = self.peek() {
            let stmt = self.stmt()?;
            stmts.push(stmt);
        }
        Ok(stmts)
    }

    fn stmt(&mut self) -> Result<Stmt> {
        let stmt = |span, kind| Stmt { kind, span };

        let (token, span) = self.next()?;
        Ok(match token {
            Token::Mov => {
                let to = self.expr()?;
                expect!(self, Token::Comma);
                let from = self.expr()?;
                stmt(span.start..from.span.end, StmtKind::Mov { to, from })
            }
            Token::Movb => {
                let to = self.expr()?;
                expect!(self, Token::Comma);
                let from = self.expr()?;
                stmt(span.start..from.span.end, StmtKind::Movb { to, from })
            }
            Token::Jmp => {
                let to = self.expr()?;
                stmt(span.start..to.span.end, StmtKind::Jmp { to })
            }
            Token::Je => {
                let to = self.expr()?;
                stmt(span.start..to.span.end, StmtKind::Je { to })
            }
            Token::Cmp => {
                let lhs = self.expr()?;
                expect!(self, Token::Comma);
                let rhs = self.expr()?;
                stmt(span.start..rhs.span.end, StmtKind::Cmp { lhs, rhs })
            }
            Token::Add => {
                let to = self.expr()?;
                expect!(self, Token::Comma);
                let value = self.expr()?;
                stmt(span.start..value.span.end, StmtKind::Add { to, value })
            }
            Token::Sub => {
                let to = self.expr()?;
                expect!(self, Token::Comma);
                let value = self.expr()?;
                stmt(span.start..value.span.end, StmtKind::Sub { to, value })
            }
            Token::Mul => {
                let to = self.expr()?;
                expect!(self, Token::Comma);
                let value = self.expr()?;
                stmt(span.start..value.span.end, StmtKind::Mul { to, value })
            }
            Token::Div => {
                let to = self.expr()?;
                expect!(self, Token::Comma);
                let value = self.expr()?;
                stmt(span.start..value.span.end, StmtKind::Div { to, value })
            }
            Token::Int => {
                let (next, next_span) = self.next()?;
                if let Token::Number(number) = next {
                    stmt(span.start..next_span.end, StmtKind::Int { number })
                } else {
                    return Err(CompilerError::simple(
                        format!("Expected number, found {:?}", next,),
                        next_span,
                    ));
                }
            }
            Token::Label(name) => {
                let name = name
                    .strip_suffix(":")
                    .expect("lexer produced invalid label")
                    .to_owned();
                stmt(span, StmtKind::Label { name })
            }
            Token::BracketOpen => return Err(CompilerError::not_allowed(span, "[")),
            Token::BracketClose => return Err(CompilerError::not_allowed(span, "]")),
            Token::Comma => return Err(CompilerError::not_allowed(span, ",")),
            Token::Number(_) => return Err(CompilerError::not_allowed(span, "{number}")),
            Token::Word(word) => {
                return Err(CompilerError::new(
                    "{word}".to_string(),
                    span.clone(),
                    vec![],
                    Some(format!("Consider using a label instead: `{}:`", word)),
                ))
            }
            Token::Error => return Err(CompilerError::invalid_token(span)),
        })
    }

    fn expr(&mut self) -> Result<Expr> {
        let expr = |kind, span| Expr { kind, span };

        let (token, span) = self.next()?;
        Ok(match token {
            Token::BracketOpen => {
                let inner = self.expr()?;
                let bclose_span = expect!(self, Token::BracketClose);
                expr(ExprKind::Addr(Box::new(inner)), span.start..bclose_span.end)
            }
            Token::Number(n) => expr(ExprKind::Number(n), span),
            Token::Word(name) => {
                if let Some(r_number) = name.strip_prefix("r") {
                    if let Ok(n) = r_number.parse::<u8>() {
                        if n > 15 {
                            return Err(CompilerError::new(
                                format!("Invalid register number: {n}"),
                                span.clone(),
                                vec![("Registers available: r0..r15".to_owned(), span)],
                                None,
                            ));
                        }
                        return Ok(expr(ExprKind::Register(n), span));
                    }
                }
                expr(ExprKind::Symbol(name.to_owned()), span)
            }
            Token::Mov => return Err(CompilerError::not_allowed(span, "mov")),
            Token::Movb => return Err(CompilerError::not_allowed(span, "movb")),
            Token::Jmp => return Err(CompilerError::not_allowed(span, "jmp")),
            Token::Je => return Err(CompilerError::not_allowed(span, "je")),
            Token::Cmp => return Err(CompilerError::not_allowed(span, "cmp")),
            Token::Add => return Err(CompilerError::not_allowed(span, "add")),
            Token::Sub => return Err(CompilerError::not_allowed(span, "sub")),
            Token::Mul => return Err(CompilerError::not_allowed(span, "mul")),
            Token::Div => return Err(CompilerError::not_allowed(span, "div")),
            Token::Int => return Err(CompilerError::not_allowed(span, "int")),
            Token::BracketClose => return Err(CompilerError::not_allowed(span, "]")),
            Token::Comma => return Err(CompilerError::not_allowed(span, ",")),
            Token::Label(_) => return Err(CompilerError::not_allowed(span, "{label}")),
            Token::Error => return Err(CompilerError::invalid_token(span)),
        })
    }

    fn peek(&mut self) -> Result<&(Token<'a>, Span)> {
        self.iter.peek().ok_or(CompilerError::eof())
    }

    fn next(&mut self) -> Result<(Token<'a>, Span)> {
        self.iter.next().ok_or(CompilerError::eof())
    }
}

// tag::parse[]
pub fn parse(src: &str) -> Result<Vec<Stmt>> {
    // end::parse[]
    let lexer = lex(src).spanned();
    let mut parser = Parser {
        iter: lexer.peekable(),
    };
    parser.program()
}

#[cfg(test)]
mod tests {
    #[test]
    fn program() {
        let result = super::parse(
            "
mov r0, 3
cmp r0, 8
je true
jmp false
true:
jmp exit

// loop
false:
mov r1, [8]
jmp false


exit:
        ",
        );
        insta::assert_debug_snapshot!(result);
    }
}
