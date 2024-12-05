//!
//! Executable Binary file writer
//!
use crate::bytecode::{Bytecode, DataValue, Label, OpCode, Operand, Register};
use common::{
    DATA_SEGMENT_TYPE, EXEC_DEBUG_FLAG, OPCODE_ADD, OPCODE_AND, OPCODE_CALL, OPCODE_CMP,
    OPCODE_CVTI2F, OPCODE_DIV, OPCODE_DYNAMIC_CALL, OPCODE_FADD, OPCODE_FCMP, OPCODE_FDIV,
    OPCODE_FMUL, OPCODE_FSUB, OPCODE_JE, OPCODE_JG, OPCODE_JGE, OPCODE_JL, OPCODE_JLE, OPCODE_JMP,
    OPCODE_JNE, OPCODE_MOV, OPCODE_MUL, OPCODE_NOP, OPCODE_NOT, OPCODE_OR, OPCODE_POP, OPCODE_PUSH,
    OPCODE_RET, OPCODE_SETE, OPCODE_SETG, OPCODE_SETGE, OPCODE_SETL, OPCODE_SETLE, OPCODE_SETNE,
    OPCODE_SHL, OPCODE_SHR, OPCODE_SUB, OPCODE_SYSCALL, OPCODE_XOR, TEXT_SEGMENT_TYPE,
};
use log::error;
use std::collections::HashMap;
use std::fs::File;
use std::io::{Error, Write};
use std::mem;
use std::path::Path;

#[repr(C)]
struct ExecutableHeader {
    magic: [u8; 4],
    version: u8,
    flags: u8,
    entry: u32,
    data_header: SectionHeader,
    text_header: SectionHeader,
    debug_header: SectionHeader,
}

#[repr(C)]
struct SectionHeader {
    offset: u32,
    size: u32,
}

pub struct Executable<'a> {
    bytecode: &'a Bytecode,
    data_labels: HashMap<String, u32>,
    text_labels: HashMap<String, u32>,
}

impl Executable<'_> {
    pub fn new(bytecode: &Bytecode) -> Executable {
        Executable {
            bytecode,
            data_labels: HashMap::new(),
            text_labels: HashMap::new(),
        }
    }

    pub fn gen_executable(
        &mut self,
        entry_label: &str,
        output_file: &Path,
        include_debug_symbols: bool,
    ) -> Result<(), Error> {
        let mut file: File = File::create(output_file)?;
        let mut flags: u8 = 0;

        // compute the labels offsets
        self.fill_label_from_data();
        self.fill_label_map_from_text(include_debug_symbols);

        // data is the first section after the global header
        let data_section = self.gen_section_data();
        let data_header = SectionHeader {
            offset: mem::size_of::<ExecutableHeader>() as u32,
            size: data_section.len() as u32,
        };

        let mut debug_symbols: Vec<u8> = vec![];

        // text section is after the data section
        let base_text_addr = data_header.offset + data_header.size;
        let text_section = self.gen_section_text(include_debug_symbols, &mut debug_symbols);
        let text_header = SectionHeader {
            offset: base_text_addr,
            size: text_section.len() as u32,
        };

        // source map section is optional, after the text section
        let debug_header = if include_debug_symbols {
            flags |= EXEC_DEBUG_FLAG;
            let debug_size = debug_symbols.len();
            SectionHeader {
                offset: text_header.offset + text_header.size,
                size: debug_size as u32,
            }
        } else {
            SectionHeader { offset: 0, size: 0 }
        };

        let header = ExecutableHeader {
            magic: [0x43, 0x4f, 0x4f, 0x4c], // COOL
            version: 2,
            flags,
            entry: *self
                .text_labels
                .get(entry_label)
                .expect("Cannot resolve entry point"), // addr relative to the text section
            data_header,
            text_header,
            debug_header,
        };

        let header_bytes: &[u8] = unsafe { Executable::convert_bytes(&header) };
        file.write(header_bytes)?;
        file.write(data_section.as_ref())?;
        file.write(text_section.as_ref())?;
        if include_debug_symbols {
            file.write(debug_symbols.as_ref())?;
        }

        Ok(())
    }

    fn write_u8(&self, byte: u8, buffer: &mut Vec<u8>) {
        buffer.push(byte);
    }

    fn write_i8(&self, byte: i8, buffer: &mut Vec<u8>) {
        buffer.push(byte as u8);
    }

    fn write_i16(&self, word: i16, buffer: &mut Vec<u8>) {
        self.write_u16(word as u16, buffer);
    }

    fn write_u16(&self, word: u16, buffer: &mut Vec<u8>) {
        // little endian
        buffer.push((word & 0xff) as u8);
        buffer.push(((word >> 8) & 0xff) as u8);
    }

    fn write_i32(&self, word: i32, buffer: &mut Vec<u8>) {
        self.write_u32(word as u32, buffer);
    }

    fn write_u32(&self, word: u32, buffer: &mut Vec<u8>) {
        // little endian
        buffer.push((word & 0xff) as u8);
        buffer.push(((word >> 8) & 0xff) as u8);
        buffer.push(((word >> 16) & 0xff) as u8);
        buffer.push(((word >> 24) & 0xff) as u8);
    }

    pub fn write_string(&mut self, string: &str, buffer: &mut Vec<u8>) {
        for b in string.bytes() {
            self.write_u8(b, buffer);
        }
        self.write_u8(0u8, buffer); // string end
    }

    fn gen_operand(&self, operand: &Operand, buffer: &mut Vec<u8>) {
        match operand {
            Operand::ImmediateValue(i) => {
                self.write_i32(*i, buffer);
            }
            Operand::ImmediateFloatValue(raw) => {
                self.write_u32(*raw, buffer);
            }
            Operand::DataAddress(label) => {
                let addr = self.data_labels.get(label).unwrap_or_else(|| {
                    error!("Cannot resolve label {label}");
                    &0u32
                });
                self.write_u8(DATA_SEGMENT_TYPE, buffer);
                self.write_u32(*addr, buffer);
            }
            Operand::ThisFieldOffset(_) => panic!("Illegal 'this' operand"),
            _ => {
                let (register, is_ref, offset, scale): (&Register, bool, i16, u8) = match operand {
                    Operand::RegisterValue(r) => (r, false, 0, 0),
                    Operand::MemoryAtRegister(r) => (r, true, 0, 0),
                    Operand::MemoryAtRegisterOffset(r, o) => (r, true, *o, 0),
                    Operand::MemoryAtRegisterOffsetScale(r, o, _, s) => (r, true, *o, *s),
                    _ => panic!("Unsupported operand"),
                };
                let has_offset = offset != 0;
                let has_scale = scale != 0;
                let flags = ((is_ref as u8) << 2) | ((has_offset as u8) << 1) | (has_scale as u8);

                let byte = (register.code() << 3) | flags;
                buffer.push(byte);
                if has_offset {
                    self.write_i16(offset, buffer);
                }
                if has_scale {
                    match operand {
                        Operand::MemoryAtRegisterOffsetScale(_, _, r2, s) => {
                            let scale_byte = (r2.code() << 3) | (s & 7);
                            buffer.push(scale_byte);
                        }
                        _ => panic!("Unsupported operand"),
                    }
                }
            }
        }
    }

    fn gen_instr_two_operands(
        &self,
        op_code: u8,
        op1: &Operand,
        op2: &Operand,
        buffer: &mut Vec<u8>,
    ) {
        buffer.push(op_code);
        let op1_imm = op1.is_immediate();
        let op2_imm = op2.is_immediate();
        if op1_imm && op2_imm {
            panic!("Invalid instruction: cannot have two immediate operands")
        }
        // [bits 0 to 1]: size, [bits 2 to 3]: immediate flags, [bits 4 to 5]: address flags, [bit 6 to 7]: reserved
        let imm_flags = ((op1_imm as u8) << 3) | ((op2_imm as u8) << 2);
        let addr_flags = ((op1.is_addr() as u8) << 5) | ((op2.is_addr() as u8) << 4);
        let byte = 3 | imm_flags | addr_flags; // 3 is for 4-byte word
        buffer.push(byte);
        self.gen_operand(op1, buffer);
        self.gen_operand(op2, buffer);
    }

    fn gen_instr_one_operand(&self, op_code: u8, operand: &Operand, buffer: &mut Vec<u8>) {
        buffer.push(op_code);
        let flags = ((operand.is_addr() as u8) << 5) | ((operand.is_immediate() as u8) << 3);
        let byte = 3 | flags; // 3 is for 4-byte word
        buffer.push(byte);
        self.gen_operand(operand, buffer);
    }

    fn gen_instr_no_operand(&self, op_code: u8, buffer: &mut Vec<u8>) {
        buffer.push(op_code);
    }

    fn gen_instr_label(&self, op_code: u8, label: &Label, buffer: &mut Vec<u8>) {
        buffer.push(op_code);
        let addr = self.text_labels.get(label).unwrap_or_else(|| {
            error!("Cannot resolve label {label}");
            &0u32
        });
        self.write_i8(TEXT_SEGMENT_TYPE as i8, buffer);
        self.write_u32(*addr, buffer);
    }

    fn gen_syscall(&self, syscall_num: u8, buffer: &mut Vec<u8>) {
        buffer.push(OPCODE_SYSCALL);
        buffer.push(syscall_num)
    }

    fn operand_size(&self, operand: &Operand) -> u32 {
        match operand {
            Operand::ImmediateValue(_) => 4, // 4-byte word for imm values
            Operand::ImmediateFloatValue(_) => 4,
            Operand::RegisterValue(_) => 1,
            Operand::MemoryAtRegister(_) => 1,
            Operand::MemoryAtRegisterOffset(_, 0) => 1,
            Operand::MemoryAtRegisterOffset(_, _) => 3,
            Operand::MemoryAtRegisterOffsetScale(_, o, _, s) => {
                let mut size = 1;
                if *o != 0 {
                    size += 2;
                }
                if *s != 0 {
                    size += 1;
                }
                size
            }
            Operand::DataAddress(_) => 5, // 1 for segment + 4 for address
            Operand::ThisFieldOffset(_) => panic!("Illegal 'this' operand"),
            Operand::BoolCondition(_) => panic!("Illegal comparison operand"),
        }
    }

    fn fill_label_map_from_text(&mut self, include_debug: bool) {
        let mut current_addr: u32 = 0;
        for instr in &self.bytecode.text {
            if instr.label.is_some() {
                self.text_labels
                    .insert(instr.label.clone().unwrap(), current_addr);
            }
            match &instr.op_code {
                OpCode::Add(dst, src)
                | OpCode::Sub(dst, src)
                | OpCode::Mul(dst, src)
                | OpCode::Div(dst, src) => {
                    current_addr += 2 + self.operand_size(src) + self.operand_size(dst);
                }
                OpCode::FAdd(dst, src)
                | OpCode::FSub(dst, src)
                | OpCode::FMul(dst, src)
                | OpCode::FDiv(dst, src) => {
                    current_addr += 2 + self.operand_size(src) + self.operand_size(dst);
                }
                OpCode::And(dst, src)
                | OpCode::Or(dst, src)
                | OpCode::Xor(dst, src)
                | OpCode::Shl(dst, src)
                | OpCode::Shr(dst, src) => {
                    current_addr += 2 + self.operand_size(src) + self.operand_size(dst);
                }
                OpCode::Not(op) | OpCode::Cvti2f(op) => {
                    current_addr += 2 + self.operand_size(op);
                }
                OpCode::Cmp(op1, op2) | OpCode::FCmp(op1, op2) => {
                    current_addr += 2 + self.operand_size(op1) + self.operand_size(op2);
                }
                OpCode::Jg(_)
                | OpCode::Jge(_)
                | OpCode::Jl(_)
                | OpCode::Jle(_)
                | OpCode::Je(_)
                | OpCode::Jne(_)
                | OpCode::Jmp(_) => {
                    current_addr += 6; // 2 + 4 for addr
                }
                OpCode::SetG(op)
                | OpCode::SetGe(op)
                | OpCode::SetL(op)
                | OpCode::SetLe(op)
                | OpCode::SetE(op)
                | OpCode::SetNe(op) => {
                    current_addr += 2 + self.operand_size(op);
                }
                OpCode::Mov(dst, src) => {
                    current_addr += 2 + self.operand_size(src) + self.operand_size(dst);
                }
                OpCode::Push(op) | OpCode::Pop(op) => {
                    current_addr += 2 + self.operand_size(op);
                }
                OpCode::Call(_) => {
                    current_addr += 6; // 2 + 4 for addr
                }
                OpCode::DynamicCall(op) => {
                    current_addr += 2 + self.operand_size(op);
                }
                OpCode::Ret | OpCode::Nop => {
                    current_addr += 1;
                }
                OpCode::Syscall(_) => {
                    current_addr += 2;
                }
            }
            if include_debug {
                current_addr += 4;
            }
        }
    }

    fn gen_section_text(&mut self, include_debug: bool, debug_symbols: &mut Vec<u8>) -> Box<[u8]> {
        let mut buffer: Vec<u8> = vec![];
        for instr in &self.bytecode.text {
            if include_debug {
                // write pointer to debug symbol
                let symbol_addr: u32 = debug_symbols.len() as u32;
                self.write_u32(symbol_addr, &mut buffer);

                // update debug symbols buffer
                let mut symbol = String::new();
                self.bytecode.print_instr(instr, &mut symbol);
                self.write_string(symbol.as_str(), debug_symbols);
            }

            // generate instruction
            match &instr.op_code {
                OpCode::Add(dst, src) => {
                    self.gen_instr_two_operands(OPCODE_ADD, src, dst, &mut buffer)
                }
                OpCode::Sub(dst, src) => {
                    self.gen_instr_two_operands(OPCODE_SUB, src, dst, &mut buffer)
                }
                OpCode::Mul(dst, src) => {
                    self.gen_instr_two_operands(OPCODE_MUL, src, dst, &mut buffer)
                }
                OpCode::Div(dst, src) => {
                    self.gen_instr_two_operands(OPCODE_DIV, src, dst, &mut buffer)
                }
                OpCode::Cmp(op1, op2) => {
                    self.gen_instr_two_operands(OPCODE_CMP, op1, op2, &mut buffer)
                }
                OpCode::FAdd(dst, src) => {
                    self.gen_instr_two_operands(OPCODE_FADD, src, dst, &mut buffer)
                }
                OpCode::FSub(dst, src) => {
                    self.gen_instr_two_operands(OPCODE_FSUB, src, dst, &mut buffer)
                }
                OpCode::FMul(dst, src) => {
                    self.gen_instr_two_operands(OPCODE_FMUL, src, dst, &mut buffer)
                }
                OpCode::FDiv(dst, src) => {
                    self.gen_instr_two_operands(OPCODE_FDIV, src, dst, &mut buffer)
                }
                OpCode::FCmp(op1, op2) => {
                    self.gen_instr_two_operands(OPCODE_FCMP, op1, op2, &mut buffer)
                }
                OpCode::Cvti2f(op) => self.gen_instr_one_operand(OPCODE_CVTI2F, op, &mut buffer),
                OpCode::And(dst, src) => {
                    self.gen_instr_two_operands(OPCODE_AND, src, dst, &mut buffer)
                }
                OpCode::Or(dst, src) => {
                    self.gen_instr_two_operands(OPCODE_OR, src, dst, &mut buffer)
                }
                OpCode::Xor(dst, src) => {
                    self.gen_instr_two_operands(OPCODE_XOR, src, dst, &mut buffer)
                }
                OpCode::Shl(dst, src) => {
                    self.gen_instr_two_operands(OPCODE_SHL, src, dst, &mut buffer)
                }
                OpCode::Shr(dst, src) => {
                    self.gen_instr_two_operands(OPCODE_SHR, src, dst, &mut buffer)
                }
                OpCode::Not(op) => self.gen_instr_one_operand(OPCODE_NOT, op, &mut buffer),
                OpCode::Jg(lbl) => self.gen_instr_label(OPCODE_JG, lbl, &mut buffer),
                OpCode::Jge(lbl) => self.gen_instr_label(OPCODE_JGE, lbl, &mut buffer),
                OpCode::Jl(lbl) => self.gen_instr_label(OPCODE_JL, lbl, &mut buffer),
                OpCode::Jle(lbl) => self.gen_instr_label(OPCODE_JLE, lbl, &mut buffer),
                OpCode::Je(lbl) => self.gen_instr_label(OPCODE_JE, lbl, &mut buffer),
                OpCode::Jne(lbl) => self.gen_instr_label(OPCODE_JNE, lbl, &mut buffer),
                OpCode::Jmp(lbl) => self.gen_instr_label(OPCODE_JMP, lbl, &mut buffer),
                OpCode::Mov(dst, src) => {
                    self.gen_instr_two_operands(OPCODE_MOV, src, dst, &mut buffer)
                }
                OpCode::Push(op) => self.gen_instr_one_operand(OPCODE_PUSH, op, &mut buffer),
                OpCode::Pop(op) => self.gen_instr_one_operand(OPCODE_POP, op, &mut buffer),
                OpCode::Call(lbl) => self.gen_instr_label(OPCODE_CALL, lbl, &mut buffer),
                OpCode::DynamicCall(op) => {
                    self.gen_instr_one_operand(OPCODE_DYNAMIC_CALL, op, &mut buffer)
                }
                OpCode::Syscall(num) => self.gen_syscall(*num, &mut buffer),
                OpCode::Ret => self.gen_instr_no_operand(OPCODE_RET, &mut buffer),
                OpCode::Nop => self.gen_instr_no_operand(OPCODE_NOP, &mut buffer),
                OpCode::SetG(op) => self.gen_instr_one_operand(OPCODE_SETG, op, &mut buffer),
                OpCode::SetGe(op) => self.gen_instr_one_operand(OPCODE_SETGE, op, &mut buffer),
                OpCode::SetL(op) => self.gen_instr_one_operand(OPCODE_SETL, op, &mut buffer),
                OpCode::SetLe(op) => self.gen_instr_one_operand(OPCODE_SETLE, op, &mut buffer),
                OpCode::SetE(op) => self.gen_instr_one_operand(OPCODE_SETE, op, &mut buffer),
                OpCode::SetNe(op) => self.gen_instr_one_operand(OPCODE_SETNE, op, &mut buffer),
            };
        }
        buffer.into_boxed_slice()
    }

    fn fill_label_from_data(&mut self) {
        let mut current_addr: u32 = 0;
        for item in &self.bytecode.data {
            match &item.label {
                Some(label) => {
                    self.data_labels.insert(label.clone(), current_addr);
                }
                None => {}
            }
            match &item.value {
                DataValue::Ascii(string) => {
                    current_addr += 1 + string.len() as u32;
                }
                DataValue::DataPtr(_) | DataValue::TextPtr(_) => {
                    current_addr += 4;
                }
                DataValue::Skip(size) => {
                    current_addr += *size;
                }
            }
        }
    }

    fn gen_section_data(&mut self) -> Box<[u8]> {
        let mut buffer: Vec<u8> = vec![];
        for item in &self.bytecode.data {
            match &item.value {
                DataValue::Ascii(string) => {
                    for b in string.bytes() {
                        buffer.push(b);
                    }
                    buffer.push(0u8); // string end
                }
                DataValue::DataPtr(label) => {
                    let addr = self
                        .data_labels
                        .get(label)
                        .expect(format!("Cannot find data pointer {label}").as_str());
                    self.write_u32(*addr, &mut buffer);
                }
                DataValue::TextPtr(label) => {
                    let addr = self
                        .text_labels
                        .get(label)
                        .expect(format!("Cannot find text pointer {label}").as_str());
                    self.write_u32(*addr, &mut buffer);
                }
                DataValue::Skip(size) => {
                    for _i in 0..*size {
                        buffer.push(0u8); // uninitialized value
                    }
                }
            }
        }
        buffer.into_boxed_slice()
    }

    unsafe fn convert_bytes<T>(obj: &T) -> &[u8] {
        std::slice::from_raw_parts((obj as *const T) as *const u8, mem::size_of::<T>())
    }
}

#[cfg(test)]
mod tests {
    use crate::bytecode::Register::GP;
    use crate::bytecode::{Bytecode, Operand};
    use crate::executable::Executable;

    #[test]
    fn test_operand_size() {
        let bytecode = Bytecode::new();
        let exec = Executable::new(&bytecode);
        assert_eq!(exec.operand_size(&Operand::ImmediateValue(12)), 4);
        assert_eq!(exec.operand_size(&Operand::RegisterValue(GP(1))), 1);
        assert_eq!(exec.operand_size(&Operand::MemoryAtRegister(GP(2))), 1);
        assert_eq!(
            exec.operand_size(&Operand::MemoryAtRegisterOffset(GP(3), 8)),
            3
        );
        assert_eq!(
            exec.operand_size(&Operand::MemoryAtRegisterOffsetScale(GP(3), 12, GP(4), 2)),
            4
        );
    }
}
