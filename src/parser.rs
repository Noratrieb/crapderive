use dbg_pls::DebugPls;
use logos::{Lexer, Logos, Span};
use std::iter::Peekable;

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

#[derive(Debug, PartialEq, Eq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum StmtKind {
    Mov { to: Expr, from: Expr },
    Add { to: Expr, value: Expr },
    Sub { to: Expr, value: Expr },
    Mul { to: Expr, value: Expr },
    Div { to: Expr, value: Expr },
    Jmp { to: Expr },
    Je { to: Expr },
    Cmp { rhs: Expr, lhs: Expr },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExprKind {
    Register,
    Number,
    Addr(Box<Expr>),
}

struct Parser<'a, I>
where
    I: Iterator<Item = Token<'a>>,
{
    iter: Peekable<I>,
}

struct CompilerError;

type Result<T> = std::result::Result<T, CompilerError>;

macro_rules! expect {
    ($self:ident, $token:pat) => {
        if let $token = $self.next()? {
            return Err(CompilerError);
        }
    };
}

fn stmt(kind: StmtKind, span: Span) -> Stmt {
    Stmt { kind, span }
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Token<'a>>,
{
    fn program(&mut self) -> Result<Vec<Stmt>> {
        todo!()
    }

    fn stmt(&mut self) -> Result<Stmt> {
        Ok(match self.next()? {
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
            Token::Add => {}
            Token::Sub => {}
            Token::Mul => {}
            Token::Div => {}
            Token::BracketOpen => {}
            Token::BracketClose => {}
            Token::Comma => {}
            Token::Label(_) => {}
            Token::Number(_) => {}
            Token::Word(_) => {}
            Token::Error => {}
        })
    }

    fn expr(&mut self) -> Result<Expr> {
        todo!()
    }

    fn peek(&mut self) -> Result<&Token<'a>> {
        self.iter.peek().ok_or(CompilerError)
    }

    fn next(&mut self) -> Result<Token<'a>> {
        self.iter.next().ok_or(CompilerError)
    }
}
