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

use std::io::{Read, Write};

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
        while self.ip < stmts.len() {
            let stmt_i = self.ip;
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
            Stmt::Movb { from, to } => {
                let value = self.read_byte_value(from);
                self.write_byte_place(to, value);
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
            0 => {
                let code = self.reg(Register(0));
                std::process::exit(code as i32);
            }
            1 => {
                let str_addr = self.reg_addr(Register(0));
                let str_len = self.reg_addr(Register(1));
                let slice = &self.memory[str_addr..][..str_len];
                let is_ok = std::io::stdout().lock().write_all(slice).is_ok();
                *self.reg_mut(Register(0)) = if is_ok { 0 } else { 1 };
            }
            2 => {
                let buffer_addr = self.reg_addr(Register(0));
                let buffer_len = self.reg_addr(Register(1));
                let slice = &mut self.memory[buffer_addr..][..buffer_len];
                let is_ok = std::io::stdin().read_exact(slice).is_ok();
                *self.reg_mut(Register(0)) = if is_ok { 0 } else { 1 };
            }
            _ => panic!("invalid interrupt!"),
        }
    }

    fn read_value(&self, value: &Value) -> u64 {
        match value {
            Value::Literal(n) => *n,
            Value::Place(place) => self.read_place(place),
        }
    }

    fn read_byte_value(&self, value: &Value) -> u8 {
        match value {
            Value::Literal(n) => *n as u8,
            Value::Place(place) => self.read_byte_place(place),
        }
    }

    fn read_place(&self, place: &Place) -> u64 {
        match place {
            Place::Register(reg) => self.reg(*reg),
            Place::AddrRegister(reg) => {
                let addr = self.reg_addr(*reg);
                self.read_addr(addr)
            }
            Place::AddrLiteral(addr) => {
                let addr = *addr as usize;
                self.read_addr(addr)
            }
        }
    }

    fn read_byte_place(&self, place: &Place) -> u8 {
        match place {
            Place::Register(reg) => self.reg(*reg) as u8,
            Place::AddrRegister(reg) => {
                let addr = self.reg_addr(*reg);
                self.memory[addr]
            }
            Place::AddrLiteral(addr) => {
                let addr = *addr as usize;
                self.memory[addr]
            }
        }
    }

    fn write_place(&mut self, place: &Place, value: u64) {
        match place {
            Place::Register(reg) => {
                *self.reg_mut(*reg) = value;
            }
            Place::AddrRegister(reg) => {
                let addr = self.reg_addr(*reg);
                self.write_addr(addr, value);
            }
            Place::AddrLiteral(addr) => {
                let addr = *addr as usize;
                self.write_addr(addr, value);
            }
        }
    }

    fn write_byte_place(&mut self, place: &Place, value: u8) {
        match place {
            Place::Register(reg) => {
                *self.reg_mut(*reg) = value.into();
            }
            Place::AddrRegister(reg) => {
                let addr = self.reg_addr(*reg);
                self.memory[addr] = value;
            }
            Place::AddrLiteral(addr) => {
                let addr = *addr as usize;
                self.memory[addr] = value;
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

    fn reg(&self, reg: Register) -> u64 {
        self.registers[reg.as_index()]
    }

    fn reg_mut(&mut self, reg: Register) -> &mut u64 {
        &mut self.registers[reg.as_index()]
    }

    fn reg_addr(&self, reg: Register) -> usize {
        self.reg(reg) as usize
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
