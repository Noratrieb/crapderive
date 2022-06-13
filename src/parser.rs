use std::{fmt::Debug, iter::Peekable};

use dbg_pls::DebugPls;
use logos::{Lexer, Logos, Span};

use crate::error::{CompilerError, Result};

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

#[derive(Debug, PartialEq, Eq, DebugPls)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, DebugPls)]
pub enum StmtKind {
    Mov { to: Expr, from: Expr },
    Add { to: Expr, value: Expr },
    Sub { to: Expr, value: Expr },
    Mul { to: Expr, value: Expr },
    Div { to: Expr, value: Expr },
    Jmp { to: Expr },
    Je { to: Expr },
    Cmp { rhs: Expr, lhs: Expr },
    Label { name: String },
}

#[derive(Debug, PartialEq, Eq, DebugPls)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, DebugPls)]
pub enum ExprKind {
    Register(u8),
    Number(u64),
    Addr(Box<Expr>),
    Name(String),
}

struct Parser<'a, I>
where
    I: Iterator<Item = (Token<'a>, Span)>,
{
    iter: Peekable<I>,
}

impl CompilerError {
    fn new(msg: String, span: Span) -> Self {
        Self { span, msg }
    }

    fn not_allowed(span: Span, token: &str) -> Self {
        Self::new(format!("`{token}` is not allowed here"), span)
    }

    fn invalid_token(span: Span) -> Self {
        Self::new("Invalid token".to_string(), span)
    }
    fn eof() -> Self {
        Self {
            span: Default::default(),
            msg: "Unexpected end of file".to_string(),
        }
    }
}

macro_rules! expect {
    ($self:ident, $token:pat) => {{
        let (next, span) = $self.next()?;
        if let $token = next {
            span
        } else {
            return Err(CompilerError {
                msg: format!(
                    concat!("Expected ", stringify!($token), ", found {:?}"),
                    next,
                ),
                span,
            });
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
        let stmt = |kind, span| Stmt { kind, span };

        let (token, span) = self.next()?;
        Ok(match token {
            Token::Mov => {
                let to = self.expr()?;
                expect!(self, Token::Comma);
                let from = self.expr()?;
                stmt(StmtKind::Mov { to, from }, Default::default())
            }
            Token::Jmp => {
                let to = self.expr()?;
                Stmt {
                    kind: StmtKind::Jmp { to },
                    span: Default::default(),
                }
            }
            Token::Je => {
                let to = self.expr()?;
                stmt(StmtKind::Je { to }, Default::default())
            }
            Token::Cmp => {
                let lhs = self.expr()?;
                expect!(self, Token::Comma);
                let rhs = self.expr()?;
                stmt(StmtKind::Cmp { lhs, rhs }, Default::default())
            }
            Token::Add => {
                let to = self.expr()?;
                expect!(self, Token::Comma);
                let value = self.expr()?;
                stmt(StmtKind::Add { to, value }, Default::default())
            }
            Token::Sub => {
                let to = self.expr()?;
                expect!(self, Token::Comma);
                let value = self.expr()?;
                stmt(StmtKind::Sub { to, value }, Default::default())
            }
            Token::Mul => {
                let to = self.expr()?;
                expect!(self, Token::Comma);
                let value = self.expr()?;
                stmt(StmtKind::Mul { to, value }, Default::default())
            }
            Token::Div => {
                let to = self.expr()?;
                expect!(self, Token::Comma);
                let value = self.expr()?;
                stmt(StmtKind::Div { to, value }, Default::default())
            }
            Token::Label(name) => stmt(
                StmtKind::Label {
                    name: name.to_owned(),
                },
                Default::default(),
            ),
            Token::BracketOpen => return Err(CompilerError::not_allowed(span, "[")),
            Token::BracketClose => return Err(CompilerError::not_allowed(span, "]")),
            Token::Comma => return Err(CompilerError::not_allowed(span, ",")),
            Token::Number(_) => return Err(CompilerError::not_allowed(span, "{number}")),
            Token::Word(_) => return Err(CompilerError::not_allowed(span, "{word}")),
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
                                format!("Only registers from 0..15 are available. Invalid register: {n}"),
                                span,
                            ));
                        }
                        return Ok(expr(ExprKind::Register(n), span));
                    }
                }
                expr(ExprKind::Name(name.to_owned()), span)
            }
            Token::Mov => return Err(CompilerError::not_allowed(span, "mov")),
            Token::Jmp => return Err(CompilerError::not_allowed(span, "jmp")),
            Token::Je => return Err(CompilerError::not_allowed(span, "je")),
            Token::Cmp => return Err(CompilerError::not_allowed(span, "cmp")),
            Token::Add => return Err(CompilerError::not_allowed(span, "add")),
            Token::Sub => return Err(CompilerError::not_allowed(span, "sub")),
            Token::Mul => return Err(CompilerError::not_allowed(span, "mul")),
            Token::Div => return Err(CompilerError::not_allowed(span, "div")),
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

pub fn parse(src: &str) -> Result<Vec<Stmt>> {
    let lexer = lex(src).spanned();
    let mut parser = Parser {
        iter: lexer.peekable(),
    };
    parser.program()
}

#[cfg(test)]
mod tests {}
