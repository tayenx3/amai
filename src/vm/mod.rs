pub mod value;
pub mod inst;

use value::*;
use inst::*;

#[derive(Clone)]
pub struct AmaiVM<'vm> {
    frames: Vec<CallFrame<'vm>>,
    constants: &'vm [Value],
    running: bool,
}

impl<'vm> AmaiVM<'vm> {
    pub fn new(constants: &'vm [Value]) -> Self {
        Self {
            frames: Vec::new(),
            constants,
            running: false
        }
    }

    pub fn call_function(&mut self, bytecode: &'vm [u8], constant_count: usize) {
        let new_frame = CallFrame {
            function: Function { constant_count, bytecode },
            registers: [Value::nil(); 256],
            constant_idx_base: self.frames
                .last()
                .map(|f|
                    f.constant_idx_base + f.function.constant_count
                ).unwrap_or(0),
            ip: bytecode.as_ptr(),
        };
        self.frames.push(new_frame);
    }

    pub fn run(&mut self) -> Result<(), &'static str> {
        self.running = true;
        while self.running {
            self.cycle()?;
        }

        Ok(())
    }

    pub fn cycle(&mut self) -> Result<(), &'static str> {
        let frame = self.frames.last_mut().ok_or("No call frames active")?;
        let opcode = unsafe{ *frame.ip };

        match opcode {
            NOP => {},
            LOAD => {
                let dest = unsafe { *frame.ip.wrapping_add(1) };
                let id = unsafe { *frame.ip.wrapping_add(2) } as u16
                | (( unsafe { *frame.ip.wrapping_add(3) } as u16 ) << 8);
                let abs_idx = frame.constant_idx_base + id as usize;
                let constant = *self.constants.get(abs_idx).ok_or("Invalid constant index")?;

                frame.registers[dest as usize] = constant;
                frame.ip = frame.ip.wrapping_add(4);
            },
            IADD => {
                let dest = unsafe { *frame.ip.wrapping_add(1) };
                let src1 = frame.registers[ unsafe { *frame.ip.wrapping_add(2) } as usize ];
                let src2 = frame.registers[ unsafe { *frame.ip.wrapping_add(3) } as usize ];

                frame.registers[dest as usize] = src1.iadd(src2);
                frame.ip = frame.ip.wrapping_add(4);
            },
            ISUB => {
                let dest = unsafe { *frame.ip.wrapping_add(1) };
                let src1 = frame.registers[ unsafe { *frame.ip.wrapping_add(2) } as usize ];
                let src2 = frame.registers[ unsafe { *frame.ip.wrapping_add(3) } as usize ];

                frame.registers[dest as usize] = src1.isub(src2);
                frame.ip = frame.ip.wrapping_add(4);
            },
            IMUL => {
                let dest = unsafe { *frame.ip.wrapping_add(1) };
                let src1 = frame.registers[ unsafe { *frame.ip.wrapping_add(2) } as usize ];
                let src2 = frame.registers[ unsafe { *frame.ip.wrapping_add(3) } as usize ];

                frame.registers[dest as usize] = src1.imul(src2);
                frame.ip = frame.ip.wrapping_add(4);
            },
            IDIV => {
                let dest = unsafe { *frame.ip.wrapping_add(1) };
                let src1 = frame.registers[ unsafe { *frame.ip.wrapping_add(2) } as usize ];
                let src2 = frame.registers[ unsafe { *frame.ip.wrapping_add(3) } as usize ];

                frame.registers[dest as usize] = src1.idiv(src2);
                frame.ip = frame.ip.wrapping_add(4);
            },
            FADD => {
                let dest = unsafe { *frame.ip.wrapping_add(1) };
                let src1 = frame.registers[ unsafe { *frame.ip.wrapping_add(2) } as usize ];
                let src2 = frame.registers[ unsafe { *frame.ip.wrapping_add(3) } as usize ];

                frame.registers[dest as usize] = src1.fadd(src2);
                frame.ip = frame.ip.wrapping_add(4);
            },
            FSUB => {
                let dest = unsafe { *frame.ip.wrapping_add(1) };
                let src1 = frame.registers[ unsafe { *frame.ip.wrapping_add(2) } as usize ];
                let src2 = frame.registers[ unsafe { *frame.ip.wrapping_add(3) } as usize ];

                frame.registers[dest as usize] = src1.fsub(src2);
                frame.ip = frame.ip.wrapping_add(4);
            },
            FMUL => {
                let dest = unsafe { *frame.ip.wrapping_add(1) };
                let src1 = frame.registers[ unsafe { *frame.ip.wrapping_add(2) } as usize ];
                let src2 = frame.registers[ unsafe { *frame.ip.wrapping_add(3) } as usize ];

                frame.registers[dest as usize] = src1.fmul(src2);
                frame.ip = frame.ip.wrapping_add(4);
            },
            FDIV => {
                let dest = unsafe { *frame.ip.wrapping_add(1) };
                let src1 = frame.registers[ unsafe { *frame.ip.wrapping_add(2) } as usize ];
                let src2 = frame.registers[ unsafe { *frame.ip.wrapping_add(3) } as usize ];

                frame.registers[dest as usize] = src1.fdiv(src2);
                frame.ip = frame.ip.wrapping_add(4);
            },
            BOR => {
                let dest = unsafe { *frame.ip.wrapping_add(1) };
                let src1 = frame.registers[ unsafe { *frame.ip.wrapping_add(2) } as usize ];
                let src2 = frame.registers[ unsafe { *frame.ip.wrapping_add(3) } as usize ];

                frame.registers[dest as usize] = src1.bor(src2);
                frame.ip = frame.ip.wrapping_add(4);
            },
            BAND => {
                let dest = unsafe { *frame.ip.wrapping_add(1) };
                let src1 = frame.registers[ unsafe { *frame.ip.wrapping_add(2) } as usize ];
                let src2 = frame.registers[ unsafe { *frame.ip.wrapping_add(3) } as usize ];

                frame.registers[dest as usize] = src1.band(src2);
                frame.ip = frame.ip.wrapping_add(4);
            },
            BXOR => {
                let dest = unsafe { *frame.ip.wrapping_add(1) };
                let src1 = frame.registers[ unsafe { *frame.ip.wrapping_add(2) } as usize ];
                let src2 = frame.registers[ unsafe { *frame.ip.wrapping_add(3) } as usize ];

                frame.registers[dest as usize] = src1.bxor(src2);
                frame.ip = frame.ip.wrapping_add(4);
            },
            BNOT => {
                let dest = unsafe { *frame.ip.wrapping_add(1) };
                let src = frame.registers[ unsafe { *frame.ip.wrapping_add(2) } as usize ];

                frame.registers[dest as usize] = src.bnot();
                frame.ip = frame.ip.wrapping_add(3);
            },
            LOR => {
                let dest = unsafe { *frame.ip.wrapping_add(1) };
                let src1 = frame.registers[ unsafe { *frame.ip.wrapping_add(2) } as usize ];
                let src2 = frame.registers[ unsafe { *frame.ip.wrapping_add(3) } as usize ];

                frame.registers[dest as usize] = src1.lor(src2);
                frame.ip = frame.ip.wrapping_add(4);
            },
            LAND => {
                let dest = unsafe { *frame.ip.wrapping_add(1) };
                let src1 = frame.registers[ unsafe { *frame.ip.wrapping_add(2) } as usize ];
                let src2 = frame.registers[ unsafe { *frame.ip.wrapping_add(3) } as usize ];

                frame.registers[dest as usize] = src1.land(src2);
                frame.ip = frame.ip.wrapping_add(4);
            },
            LNOT => {
                let dest = unsafe { *frame.ip.wrapping_add(1) };
                let src = frame.registers[ unsafe { *frame.ip.wrapping_add(2) } as usize ];

                frame.registers[dest as usize] = src.lnot();
                frame.ip = frame.ip.wrapping_add(3);
            },
            HALT => self.running = false,
            _ => todo!(),
        }

        Ok(())
    }
}

#[derive(Clone, Copy)]
pub struct CallFrame<'cf> {
    pub function: Function<'cf>,
    pub registers: [Value; 256],
    pub constant_idx_base: usize,
    pub ip: *const u8,
}

#[allow(unused)]
#[derive(Clone, Copy)]
pub struct Function<'func> {
    pub constant_count: usize,
    pub bytecode: &'func [u8],
}