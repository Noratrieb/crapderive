# crap-derive

```asm
mov r0, 3
cmp r0, 8
je true
jmp false
true:
jmp exit

// loop
false:
mov r5, [8]
jmp false


exit:
```

# Specification

The crapderive virtual machine has 16 registers, `r0` to `r15`.

It has 100MB (1024^3) bytes of memory, addressed by the addresses `0x0` to `0x40000000` (not included).

The machine word (register) size is 64 bit.

Arithmetic is defined as wrappings, and division by zero traps (cannot be caught).

The VM has a `flag` that is used in some operations.

## Definitions

### Place

A place is a location in the VM. A place can either be a register (`r0`-`r15`)
or a memory address (`[X]` where `X` is an `index`).

### Index

An index is used to dereference memory and can be one of the following:

* A register
* A literal number
* A symbol (TODO)

### Value

A value is a value that can be obtained from the VM. A value can either be a place, a literal number or a symbol.

### Location

A location is a location in the VM code. It must contain a symbol with the label to redirect execution to.

## Instructions

### Mov

`mov A, B`

Mov moves the `value` `B` to the `place` `A`.

### Add

`add A, B`

Add adds the `value` `B` to the `place` `A`.
The result is stored in the `place` `A`.

### Sub

`sub A, B`

Sub subtracts the `value` `B` from* the `place` `A`.
The result is stored in the `place` `A`.

### Mul

`mul A, B`

Mul multiplies the `place` `A` with the `value` `B`.
The result is stored in the `place` `A`.

### Div

`div A, B`

Div divides the `place` `A` by the `value` `B`.
The result is stored in the `place` `A`.

### Cmp

`cmp A, B`

Cmp compares the `value` `A` with the `value` `B`.
If they are equal, the `flag` is set, otherwise the `flag` is cleared.

### Jmp

`jmp L`

Jmp jumps to the `location` `L`. The next instruction executed will be the instruction located at `L`.

### Je

`je L`

Je jumps to the `location` `L` if the `flag` is set. If the jump happens,
the next instruction executed will be the instruction located at `L`.

### Label
`<IDENT>:`

A label is an identifier (regex: `[a-zA-Z]\w+`) followed by a colon (`:`). Jumps (like `jmp` or `je`) will be able
to jump to this label. A label is applied to the *next* instruction in the code.