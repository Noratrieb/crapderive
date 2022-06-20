#[derive(Debug, Clone, Copy)]
pub struct Register(u8);

#[derive(Debug, Clone, Copy)]
pub enum Place {
    Register(Register),
    /// `[r5]`
    AddrRegister(Register),
    /// `[353]`
    AddrLiteral(u64),
}

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Literal(u64),
    Place(Place),
}

#[derive(Debug, Clone, Copy)]
pub struct Location {
    index: usize,
}

#[derive(Debug, Clone, Copy)]
pub enum Stmt {
    Mov { to: Place, from: Value },
    Add { to: Place, value: Value },
    Sub { to: Place, value: Value },
    Mul { to: Place, value: Value },
    Div { to: Place, value: Value },
    Jmp { to: Location },
    Cmp { lhs: Value, rhs: Value },
}
