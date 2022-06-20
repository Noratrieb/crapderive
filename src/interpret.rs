//!
//! The interpreter is little endian
//!
//! ```text
//! | 0 | 1 | 2 | 3
//! ---------------
//!  1   0   0   0
//! --------------
//! Decimal 1
//! ```

use logos::Span;

use crate::{
    error::Result,
    ir::{Place, Register, Stmt, Value},
};

impl Register {
    fn as_index(self) -> usize {
        self.0.into()
    }
}

const MEMORY_SIZE: usize = 1024 * 1024 * 1024;

struct InterpretCtx {
    memory: Vec<u8>,
    registers: [u64; 16],
    flag: bool,
    ip: usize,
}

impl InterpretCtx {
    fn interpret(&mut self, stmts: Vec<Stmt>) -> Result<()> {
        let stmt_i = self.ip;
        while stmt_i < stmts.len() {
            self.ip += 1;
            self.interpret_stmt(stmt_i, &stmts)?
        }
        Ok(())
    }

    fn interpret_stmt(&mut self, stmt_i: usize, stmts: &[Stmt]) -> Result<()> {
        let stmt = &stmts[stmt_i];
        match stmt {
            Stmt::Mov { from, to } => {
                let value = self.read_value(from);
                self.write_place(to, value);
            }
            Stmt::Add { to, value } => {
                let old = self.read_place(to);
                let value = self.read_value(value);
                let new = old.wrapping_add(value);
                self.write_place(to, new);
            }
            Stmt::Sub { to, value } => {
                let old = self.read_place(to);
                let value = self.read_value(value);
                let new = old.wrapping_sub(value);
                self.write_place(to, new);
            }
            Stmt::Mul { to, value } => {
                let old = self.read_place(to);
                let value = self.read_value(value);
                let new = old.wrapping_mul(value);
                self.write_place(to, new);
            }
            Stmt::Div { to, value } => {
                let old = self.read_place(to);
                let value = self.read_value(value);
                let new = old.wrapping_div(value);
                self.write_place(to, new);
            }
            Stmt::Int { number } => {
                self.interrupt(*number);
            }
            Stmt::Jmp { to } => {
                let index = to.index;
                self.ip = index;
            }
            Stmt::Cmp { lhs, rhs } => {
                let lhs = self.read_value(lhs);
                let rhs = self.read_value(rhs);
                self.flag = lhs == rhs;
            }
            Stmt::Je { to } => {
                let index = to.index;
                if self.flag {
                    self.ip = index;
                }
            }
        }
        Ok(())
    }

    fn interrupt(&mut self, number: u64) {
        match number {
            0 => todo!("exit"),
            1 => todo!("print"),
            _ => panic!("invalid interrupt!"),
        }
    }

    fn read_value(&self, value: &Value) -> u64 {
        match value {
            Value::Literal(n) => *n,
            Value::Place(place) => self.read_place(place),
        }
    }

    fn read_place(&self, place: &Place) -> u64 {
        match place {
            Place::Register(reg) => self.registers[reg.as_index()],
            Place::AddrRegister(reg) => {
                let addr = self.registers[reg.as_index()] as usize;
                self.read_addr(addr)
            }
            Place::AddrLiteral(addr) => {
                let addr = *addr as usize;
                self.read_addr(addr)
            }
        }
    }

    fn write_place(&mut self, place: &Place, value: u64) {
        match place {
            Place::Register(reg) => {
                let r = &mut self.registers[reg.as_index()];
                *r = value;
            }
            Place::AddrRegister(reg) => {
                let addr = self.registers[reg.as_index()] as usize;
                self.write_addr(addr, value);
            }
            Place::AddrLiteral(addr) => {
                let addr = *addr as usize;
                self.write_addr(addr, value);
            }
        }
    }

    fn read_addr(&self, addr: usize) -> u64 {
        u64::from_le_bytes([
            self.memory[addr],
            self.memory[addr + 1],
            self.memory[addr + 2],
            self.memory[addr + 3],
            self.memory[addr + 4],
            self.memory[addr + 5],
            self.memory[addr + 6],
            self.memory[addr + 7],
        ])
    }

    fn write_addr(&mut self, addr: usize, value: u64) {
        assert!(addr + 7 < self.memory.len());
        let bytes = value.to_le_bytes();
        for i in 0..8 {
            self.memory[addr + i] = bytes[i];
        }
    }
}

pub fn interpret(stmts: Vec<Stmt>, _spans: Vec<Span>) -> Result<()> {
    let mut ctx = InterpretCtx {
        memory: vec![0; MEMORY_SIZE],
        registers: [0; 16],
        flag: false,
        ip: 0,
    };

    ctx.interpret(stmts)
}
