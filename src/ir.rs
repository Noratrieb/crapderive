use std::{collections::HashMap, mem};

use dbg_pls::DebugPls;
use logos::Span;

use crate::{
    error::{CompilerError, Result},
    parser,
    parser::{ExprKind, StmtKind},
};

#[derive(Debug, Clone, Copy, DebugPls)]
pub struct Register(pub u8);

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
    pub index: usize,
}

#[derive(Debug, Clone, Copy, DebugPls)]
// tag::stmt[]
pub enum Stmt {
    Mov { to: Place, from: Value },
    Movb { to: Place, from: Value },
    Add { to: Place, value: Value },
    Sub { to: Place, value: Value },
    Mul { to: Place, value: Value },
    Div { to: Place, value: Value },
    Int { number: u64 },
    Jmp { to: Location },
    Je { to: Location },
    Cmp { lhs: Value, rhs: Value },
}
// end::stmt[]

struct CompileCtx {
    stmts: Vec<Stmt>,
    spans: Vec<Span>,
    labels: HashMap<String, (Location, Span)>,
    /// a vector of index and label name into `stmts` that contain references to labels that may
    /// be declared after them
    location_to_resolve: Vec<(usize, String)>,
}

impl CompileCtx {
    fn compile(&mut self, ast: impl Iterator<Item = parser::Stmt>) -> Result<()> {
        for stmt in ast {
            self.compile_stmt(stmt)?;
        }

        let locations = mem::take(&mut self.location_to_resolve);
        for (index, label) in locations {
            self.resolve_location(index, &label)?;
        }

        Ok(())
    }

    fn compile_stmt(&mut self, p_stmt: parser::Stmt) -> Result<()> {
        let stmt = match p_stmt.kind {
            StmtKind::Mov { from, to } => {
                let from = self.compile_value(from)?;
                let to = self.compile_place(to)?;
                Stmt::Mov { from, to }
            }
            StmtKind::Movb { from, to } => {
                let from = self.compile_value(from)?;
                let to = self.compile_place(to)?;
                Stmt::Movb { from, to }
            }
            StmtKind::Add { to, value } => {
                let to = self.compile_place(to)?;
                let value = self.compile_value(value)?;
                Stmt::Add { to, value }
            }
            StmtKind::Sub { to, value } => {
                let to = self.compile_place(to)?;
                let value = self.compile_value(value)?;
                Stmt::Sub { to, value }
            }
            StmtKind::Mul { to, value } => {
                let to = self.compile_place(to)?;
                let value = self.compile_value(value)?;
                Stmt::Mul { to, value }
            }
            StmtKind::Div { to, value } => {
                let to = self.compile_place(to)?;
                let value = self.compile_value(value)?;
                Stmt::Div { to, value }
            }
            StmtKind::Int { number } => {
                if number > 2 {
                    return Err(CompilerError::simple(
                        "invalid interrupt".to_string(),
                        p_stmt.span,
                    ));
                }
                Stmt::Int { number }
            }
            StmtKind::Jmp { to } => {
                let to = self.compile_location(to, self.stmts.len())?;
                Stmt::Jmp { to }
            }
            StmtKind::Je { to } => {
                let to = self.compile_location(to, self.stmts.len())?;
                Stmt::Je { to }
            }
            StmtKind::Cmp { lhs, rhs } => {
                let lhs = self.compile_value(lhs)?;
                let rhs = self.compile_value(rhs)?;
                Stmt::Cmp { lhs, rhs }
            }
            StmtKind::Label { name } => {
                self.compile_label(name, p_stmt.span)?;
                // no statement to emit here
                return Ok(());
            }
        };

        self.stmts.push(stmt);
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
                ExprKind::Symbol(_) => Err(CompilerError::simple(
                    "symbol not allowed here".to_owned(),
                    expr.span,
                ))
            },
        }
    }

    fn compile_value(&mut self, expr: parser::Expr) -> Result<Value> {
        match expr.kind {
            ExprKind::Number(n) => Ok(Value::Literal(n)),
            ExprKind::Symbol(_) => {
                Err(CompilerError::simple(
                    "symbol not allowed here".to_owned(),
                    expr.span,
                ))
            }
            _ => Ok(Value::Place(self.compile_place(expr)?)),
        }
    }

    fn get_label_position(&mut self, label: String, index: usize) -> Location {
        let location = self.labels.get(&label);
        location.map(|(location, _)| *location).unwrap_or_else(|| {
            self.location_to_resolve.push((index, label));
            Location { index: usize::MAX }
        })
    }

    fn compile_location(&mut self, expr: parser::Expr, index: usize) -> Result<Location> {
        match expr.kind {
            ExprKind::Symbol(sym) => Ok(self.get_label_position(sym, index)),
            ExprKind::Register(_) => Err(CompilerError::simple(
                "cannot jump to a register".to_string(),
                expr.span,
            )),
            ExprKind::Number(_) => Err(CompilerError::simple(
                "cannot jump to a number literal".to_string(),
                expr.span,
            )),
            ExprKind::Addr(_) => Err(CompilerError::simple(
                "cannot jump to a dereferenced address".to_string(),
                expr.span,
            )),
        }
    }

    fn compile_label(&mut self, label: String, span: Span) -> Result<()> {
        let index = self.stmts.len();
        let old = self
            .labels
            .insert(label, (Location { index }, span.clone()));
        if let Some((_, old_span)) = old {
            return Err(CompilerError::new_notes(
                "duplicate label found".to_string(),
                span,
                vec![("previous label defined here".to_string(), old_span)],
            ));
        }
        Ok(())
    }

    fn resolve_location(&mut self, index: usize, label: &str) -> Result<()> {
        let (location, _) = self.labels.get(label).ok_or_else(|| {
            CompilerError::simple(
                format!("label {label} not found"),
                self.spans[index].clone(),
            )
        })?;

        let instr = &mut self.stmts[index];
        match instr {
            Stmt::Je { to } => *to = *location,
            Stmt::Jmp { to } => *to = *location,
            _ => unreachable!(),
        }
        Ok(())
    }
}

pub fn compile(ast: impl Iterator<Item = parser::Stmt>) -> Result<(Vec<Stmt>, Vec<Span>)> {
    let mut ctx = CompileCtx {
        stmts: Vec::new(),
        spans: Vec::new(),
        labels: HashMap::new(),
        location_to_resolve: Vec::new(),
    };
    ctx.compile(ast)?;
    Ok((ctx.stmts, ctx.spans))
}
