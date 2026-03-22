# Amai

> **Early development ⚠️**: Bugs are prone and the codebase can be "janky"

```ml
let count(x: int) = {
    if x > 0 then count(x - 1)
}
```

Amai is an embeddable, statically-typed scripting language with a minimal footprint, tailored to make developers' lives a little sweeter.

## Why Amai?
Amai is:
- **Type-safe**, so you can spend your time building instead of debugging runtime errors.
- **Simple**, so you won't spend your time fighting languages like C or Rust for less critical systems.
- **Small**, so you don't have to worry about installing 500 tools just to run a program

Amai is perfect for game development where errors are minor

## Installing Amai
You can install Amai all in 1-2 steps:
### 1. Install Rust (if you haven't)
Run this command if you're using Linux/Mac:
```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```
Or visit https://rustup.rs for others

### 2. Install Amai with Cargo
```bash
cargo install --git https://github.com/tayenx3/amai.git
```