use crate::vm::inst::*;

pub fn disassemble(bytecode: &[u32]) -> String {
    let mut output = String::new();
    for inst in bytecode {
        let opcode = (inst & 0xFF) as u8;
        match opcode {
            NOP => output.push_str("NOP\n"),
            LOAD => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let const_id = ((inst >> 16) & 0xFFFF) as u16;
                output.push_str(&format!("LOAD r{dest} #{const_id}\n"));
            },
            IADD => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("IADD r{dest} r{src1} r{src2}\n"));
            },
            ISUB => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("ISUB r{dest} r{src1} r{src2}\n"));
            },
            IMUL => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("IMUL r{dest} r{src1} r{src2}\n"));
            },
            IDIV => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("IDIV r{dest} r{src1} r{src2}\n"));
            },
            IREM => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("IREM r{dest} r{src1} r{src2}\n"));
            },
            FADD => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("FADD r{dest} r{src1} r{src2}\n"));
            },
            FSUB => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("FSUB r{dest} r{src1} r{src2}\n"));
            },
            FMUL => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("FMUL r{dest} r{src1} r{src2}\n"));
            },
            FDIV => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("FDIV r{dest} r{src1} r{src2}\n"));
            },
            FREM => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("FREM r{dest} r{src1} r{src2}\n"));
            },
            BOR => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("BOR r{dest} r{src1} r{src2}\n"));
            },
            BAND => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("BAND r{dest} r{src1} r{src2}\n"));
            },
            BNOT => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                output.push_str(&format!("BNOT r{dest} r{src1}\n"));
            },
            BXOR => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("BXOR r{dest} r{src1} r{src2}\n"));
            },
            LOR => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("LOR r{dest} r{src1} r{src2}\n"));
            },
            LAND => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("LAND r{dest} r{src1} r{src2}\n"));
            },
            LNOT => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                output.push_str(&format!("LNOT r{dest} r{src1}\n"));
            },
            CMEQ => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("CMEQ r{dest} r{src1} r{src2}\n"));
            },
            CMNE => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("CMNE r{dest} r{src1} r{src2}\n"));
            },
            ICGT => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("ICGT r{dest} r{src1} r{src2}\n"));
            },
            ICLT => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("ICLT r{dest} r{src1} r{src2}\n"));
            },
            ICGE => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("ICGE r{dest} r{src1} r{src2}\n"));
            },
            ICLE => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("ICLE r{dest} r{src1} r{src2}\n"));
            },
            FCGT => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("FCGT r{dest} r{src1} r{src2}\n"));
            },
            FCLT => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("FCLT r{dest} r{src1} r{src2}\n"));
            },
            FCGE => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("FCGE r{dest} r{src1} r{src2}\n"));
            },
            FCLE => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("FCLE r{dest} r{src1} r{src2}\n"));
            },
            JUMP => {
                let a = ((inst >> 8) & 0xFFFF) as i16;
                let dest = if a >= 0 { format!("+{}", a) } else { a.to_string() };
                output.push_str(&format!("JUMP {dest}\n"));
            },
            JITR => {
                let a = ((inst >> 8) & 0xFFFF) as i16;
                let dest = if a >= 0 { format!("+{}", a) } else { a.to_string() };
                let src1 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("JITR {dest} r{src1}\n"));
            },
            JIFL => {
                let a = ((inst >> 8) & 0xFFFF) as i16;
                let dest = if a >= 0 { format!("+{}", a) } else { a.to_string() };
                let src1 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("JIFL {dest} r{src1}\n"));
            },
            CALL => {
                let func = (inst >> 8) & 0xFFFFFF;
                output.push_str(&format!("CALL ${func}\n"));
            },
            RETN => output.push_str("RETN"),
            INEG => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                output.push_str(&format!("INEG r{dest} r{src1}\n"));
            },
            FNEG => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                output.push_str(&format!("FNEG r{dest} r{src1}\n"));
            },
            MOVE => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                output.push_str(&format!("MOVE r{dest} r{src1}\n"));
            },
            PARG => {
                let src = ((inst >> 8) & 0xFF) as u8;
                output.push_str(&format!("PARG r{src}\n"));
            },
            CARG => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let index = ((inst >> 16) & 0xFFFF) as u16;
                output.push_str(&format!("CARG r{dest} #{index}\n"));
            },
            CEXT => {
                let func = (inst >> 8) & 0xFFFFFF;
                output.push_str(&format!("CEXT ${func}\n"));
            },
            LSHF => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("LSHF r{dest} r{src1} r{src2}\n"));
            },
            RSHF => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("RSHF r{dest} r{src1} r{src2}\n"));
            },
            SCON => {
                let dest = ((inst >> 8) & 0xFF) as u8;
                let src1 = ((inst >> 16) & 0xFF) as u8;
                let src2 = ((inst >> 24) & 0xFF) as u8;
                output.push_str(&format!("SCON r{dest} r{src1} r{src2}\n"));
            },
            HALT => output.push_str("HALT\n"),
            _ => todo!()
        }
    }

    output
}