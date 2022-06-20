use std::collections::HashMap;

use dbg_pls::DebugPls;
use logos::Span;

use crate::{
    error::{CompilerError, Result},
    parser,
    parser::{ExprKind, StmtKind},
};

#[derive(Debug, Clone, Copy, DebugPls)]
pub struct Register(u8);

#[derive(Debug, Clone, Copy, DebugPls)]
pub enum Place {
    Register(Register),
    /// `[r5]`
    AddrRegister(Register),
    /// `[353]`
    AddrLiteral(u64),
}

#[derive(Debug, Clone, Copy, DebugPls)]
pub enum Value {
    Literal(u64),
    Place(Place),
}

#[derive(Debug, Clone, Copy, DebugPls)]
pub struct Location {
    index: usize,
}

#[derive(Debug, Clone, Copy, DebugPls)]
pub enum Stmt {
    Mov { to: Place, from: Value },
    Add { to: Place, value: Value },
    Sub { to: Place, value: Value },
    Mul { to: Place, value: Value },
    Div { to: Place, value: Value },
    Jmp { to: Location },
    Je { to: Location },
    Cmp { lhs: Value, rhs: Value },
}

struct CompileCtx {
    stmts: Vec<Stmt>,
    spans: Vec<Span>,
    labels: HashMap<String, usize>,
}

impl CompileCtx {
    fn compile(&mut self, ast: impl Iterator<Item = parser::Stmt>) -> Result<()> {
        for stmt in ast {
            self.compile_stmt(stmt)?;
        }
        Ok(())
    }

    fn compile_stmt(&mut self, p_stmt: parser::Stmt) -> Result<()> {
        match p_stmt.kind {
            StmtKind::Mov { from, to } => {
                let from = self.compile_value(from)?;
                let to = self.compile_place(to)?;
                let stmt = Stmt::Mov { from, to };
                self.stmts.push(stmt);
            }
            StmtKind::Add { to, value } => {
                let to = self.compile_place(to)?;
                let value = self.compile_value(value)?;
                let stmt = Stmt::Add { to, value };
                self.stmts.push(stmt);
            }
            StmtKind::Sub { to, value } => {
                let to = self.compile_place(to)?;
                let value = self.compile_value(value)?;
                let stmt = Stmt::Sub { to, value };
                self.stmts.push(stmt);
            }
            StmtKind::Mul { to, value } => {
                let to = self.compile_place(to)?;
                let value = self.compile_value(value)?;
                let stmt = Stmt::Mul { to, value };
                self.stmts.push(stmt);
            }
            StmtKind::Div { to, value } => {
                let to = self.compile_place(to)?;
                let value = self.compile_value(value)?;
                let stmt = Stmt::Div { to, value };
                self.stmts.push(stmt);
            }
            StmtKind::Jmp { .. } => todo!("jmp"),
            StmtKind::Je { .. } => todo!("je"),
            StmtKind::Cmp { lhs, rhs } => {
                let lhs = self.compile_value(lhs)?;
                let rhs = self.compile_value(rhs)?;
                let stmt = Stmt::Cmp { lhs, rhs };
                self.stmts.push(stmt);
            }
            StmtKind::Label { .. } => {}
        }
        self.spans.push(p_stmt.span);
        Ok(())
    }

    fn compile_place(&mut self, expr: parser::Expr) -> Result<Place> {
        match expr.kind {
            ExprKind::Register(r) => Ok(Place::Register(Register(r))),
            ExprKind::Number(_) => Err(CompilerError::simple(
                "number literals are not allowed in place position".to_string(),
                expr.span,
            )),
            ExprKind::Symbol(_) => Err(CompilerError::simple(
                "symbol literals are not allowed in place position".to_string(),
                expr.span,
            )),
            ExprKind::Addr(addr) => match addr.kind {
                ExprKind::Register(r) => Ok(Place::AddrRegister(Register(r))),
                ExprKind::Number(n) => Ok(Place::AddrLiteral(n)),
                ExprKind::Addr(nested) => Err(CompilerError::help(
                    "cannot dereference result of another dereference".to_string(),
                    nested.span,
                    "save the first result in a temporary register".to_string(),
                )),
                ExprKind::Symbol(_) => todo!("compile to AddrLiteral"),
            },
        }
    }

    fn compile_value(&mut self, expr: parser::Expr) -> Result<Value> {
        match expr.kind {
            ExprKind::Number(n) => Ok(Value::Literal(n)),
            ExprKind::Symbol(_) => todo!("compile to Literal"),
            _ => Ok(Value::Place(self.compile_place(expr)?)),
        }
    }
}

pub fn compile(ast: impl Iterator<Item = parser::Stmt>) -> Result<(Vec<Stmt>, Vec<Span>)> {
    let mut ctx = CompileCtx {
        stmts: Vec::new(),
        spans: Vec::new(),
        labels: HashMap::new(),
    };
    ctx.compile(ast)?;
    Ok((ctx.stmts, ctx.spans))
}
