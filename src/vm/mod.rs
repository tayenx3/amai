pub mod value;
pub mod inst;
pub mod function;
pub mod call_frame;
pub mod arena;

use std::rc::Rc;
use value::*;
use inst::*;
use function::Function;
use call_frame::CallFrame;
use arena::Arena;
use crate::common::Span;

pub struct AmaiVM {
    pub frames: Vec<CallFrame>,
    pub constants: Box<[Value]>,
    pub running: bool,
    pub functions: Vec<(Rc<Function>, [Value; 64])>,
    pub allow_large_bytecode: bool,
    pub external_functions: Vec<Rc<dyn Fn(&mut AmaiVM, &[Value])>>,
    pub arena: Arena,
}

impl AmaiVM {
    pub fn new(allow_large_bytecode: bool) -> Self {
        Self {
            frames: Vec::new(),
            constants: Box::new([]),
            running: false,
            functions: Vec::new(),
            allow_large_bytecode,
            external_functions: Vec::new(),
            arena: Arena::new(),
        }
    }

    #[allow(unused)]
    pub fn add_extern_fn<F: Fn(&mut AmaiVM, &[Value]) + 'static>(&mut self, f: F) -> u32 {
        self.external_functions.push(Rc::new(f));
        self.external_functions.len() as u32 - 1
    }

    
    #[inline(always)]
    pub fn add_function(&mut self, bytecode: Box<[(u32, Span)]>, registers: &[Value]) -> usize {
        if !self.allow_large_bytecode {
            assert!(bytecode.len() < 65536, "Bytecode length is out of jump bounds");
        }

        let func = Function { bytecode };
        self.functions.push((Rc::new(func), registers.try_into().unwrap()));
        self.functions.len() - 1
    }

    #[inline(always)]
    pub fn call_function(&mut self, id: usize, caller_args: Box<[Value]>) {
        let (function, registers) = self.functions[id].clone();
        let new_frame = CallFrame {
            caller_args,
            callee_args: Vec::new(),
            function,
            registers,
            ip: 0,
        };
        self.frames.push(new_frame);
    }
    #[inline(always)]
    pub fn call_external(&mut self, id: usize, caller_args: &[Value]) {
        let f = self.external_functions[id].clone();
        f(self, caller_args);
    }

    #[inline(always)]
    pub fn return_function(&mut self) {
        if let Some(mut callee_frame) = self.frames.pop() {
            if let Some(caller_frame) = self.frames.last_mut() {
                caller_frame.registers[0] 
                = callee_frame.callee_args.pop().unwrap_or(Value::nil());
            }
        }
    }

    pub fn run(&mut self) -> Result<(), (String, Span)> {
        self.running = true;
        while self.running {
            unsafe { self.cycle()? }
        }

        Ok(())
    }

    #[inline(always)]
    #[allow(unsafe_op_in_unsafe_fn)]
    pub unsafe fn cycle(&mut self) -> Result<(), (String, Span)> {
        if self.frames.is_empty() { self.running = false; return Ok(()) }
        let frame = self.frames.last_mut().unwrap() as *mut CallFrame;
        let (inst, span) = if let Some(inst) = (&(*frame).function).bytecode.get((*frame).ip) {
            inst
        } else {
            self.running = false;
            return Ok(());
        };
        let mut next_ip = (*frame).ip + 1;

        let opcode = (inst & 0xFF) as u8;

        match opcode {
            NOP => {},
            LOAD => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let id = ((inst >> 16) & 0xFFFF) as u16;
                let constant = *self.constants.get_unchecked(id as usize);

                (*frame).registers[dest as usize] = constant;
            },
            IADD => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.iadd(src2).map_err(|err| (err, *span))?;
            },
            ISUB => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.isub(src2).map_err(|err| (err, *span))?;
            },
            IMUL => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.imul(src2).map_err(|err| (err, *span))?;
            },
            IDIV => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.idiv(src2).map_err(|err| (err, *span))?;
            },
            IREM => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.irem(src2).map_err(|err| (err, *span))?;
            },
            FADD => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.fadd(src2);
            },
            FSUB => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.fsub(src2);
            },
            FMUL => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.fmul(src2);
            },
            FDIV => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.fdiv(src2).ok_or(("Division by zero".to_string(), *span))?;
            },
            FREM => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.frem(src2).ok_or(("Division by zero".to_string(), *span))?;
            },
            BOR => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.bor(src2);
            },
            BAND => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.band(src2);
            },
            BXOR => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.bxor(src2);
            },
            BNOT => {
                let src = (*frame).registers[((inst >> 16) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src.bnot();
            },
            LOR => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.lor(src2);
            },
            LAND => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.land(src2);
            },
            LNOT => {
                let src = (*frame).registers[((inst >> 16) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src.lnot();
            },
            CMEQ => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.cmeq(src2);
            },
            CMNE => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.cmne(src2);
            },
            ICGT => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.icgt(src2);
            },
            ICLT => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.iclt(src2);
            },
            ICGE => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.icge(src2);
            },
            ICLE => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.icle(src2);
            },
            FCGT => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.fcgt(src2);
            },
            FCLT => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.fclt(src2);
            },
            FCGE => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.fcge(src2);
            },
            FCLE => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.fcle(src2);
            },
            JUMP => {
                let addr = ((inst >> 8) & 0xFFFF) as i16;

                if addr >= 0 {
                    next_ip = (*frame).ip + addr as usize;
                } else {
                    next_ip = (*frame).ip - addr.abs() as usize;
                }
            },
            JITR => {
                let addr = ((inst >> 8) & 0xFFFF) as i16;
                let src = (*frame).registers[((inst >> 24) & 0xFF) as usize].to_bool();

                if src {
                    if addr >= 0 {
                        next_ip = (*frame).ip + addr as usize;
                    } else {
                        next_ip = (*frame).ip - addr.abs() as usize;
                    }
                }
            },
            JIFL => {
                let addr = ((inst >> 8) & 0xFFFF) as i16;
                let src = (*frame).registers[((inst >> 24) & 0xFF) as usize].to_bool();

                if !src {
                    if addr >= 0 {
                        next_ip = (*frame).ip + addr as usize;
                    } else {
                        next_ip = (*frame).ip - addr.abs() as usize;
                    }
                }
            },
            CALL => {
                let id = (inst >> 8) & 0xFF;
                self.call_function(
                    (*frame).registers[id as usize].to_ptr(),
                    (*frame).callee_args.clone().into_boxed_slice(),
                );
                (*frame).callee_args.clear();
            },
            RETN => self.return_function(),
            INEG => {
                let src = (*frame).registers[((inst >> 16) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src.ineg();
            },
            FNEG => {
                let src = (*frame).registers[((inst >> 16) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src.fneg();
            },
            MOVE => {
                let src = (*frame).registers[((inst >> 16) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src;
            },
            PARG => {
                let src = (*frame).registers[((inst >> 8) & 0xFF) as usize];

                (*frame).callee_args.push(src);
            },
            CARG => {
                let arg_id = ((inst >> 16) & 0xFFFF) as usize;

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = (*frame).caller_args[arg_id];
            },
            CEXT => {
                let id = (inst >> 8) & 0xFFFFFF;

                self.call_external(id as usize, &(*frame).callee_args);
                (*frame).callee_args.clear();
            },
            LSHF => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.lshf(src2).map_err(|err| (err, *span))?;
            },
            RSHF => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.rshf(src2).map_err(|err| (err, *span))?;
            },
            SCON => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.scon(src2, &mut self.arena);
            },
            SCEQ => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.sceq(src2, &mut self.arena);
            },
            SCNE => {
                let src1 = (*frame).registers[((inst >> 16) & 0xFF) as usize];
                let src2 = (*frame).registers[((inst >> 24) & 0xFF) as usize];

                (*frame).registers[((inst >> 8) & 0xFF) as usize] = src1.scne(src2, &mut self.arena);
            },
            HALT => self.running = false,
            _ => panic!("Unknown opcode: {opcode:#04X}"),
        }

        (*frame).ip = next_ip;

        Ok(())
    }
}