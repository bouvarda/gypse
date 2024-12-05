use common::{CR_REGISTER_ID, DATA_SEGMENT_TYPE, DS_REGISTER_ID, GP_REGISTER_COUNT, SB_REGISTER_ID, SP_REGISTER_ID, TEXT_SEGMENT_TYPE};
use log::error;
use crate::FLAG_ENABLE_TRACE;
use crate::debugger::DebuggerAction;
use crate::memory::PhysicalMemory;
use crate::process::Process;
use crate::syscall::execute_syscall;
use crate::utils::{print_data, print_heap, print_memory, print_stack, Interruption};

#[derive(PartialEq, Debug, Clone)]
struct CpuRegisters {
    ip: u32, // instruction pointer
    sp: u32, // stack pointer
    sb: u32, // stack base
    gp: [u32; GP_REGISTER_COUNT], // general purpose,
    cr: u32, // call return
    ds: u32, // data segment
    flags: u8,
}

const ZERO_FLAG_SHIFT: u8 = 0x1;
const SIGN_FLAG_SHIFT: u8 = 0x2;
const OVERFLOW_FLAG_SHIFT: u8 = 0x4;

#[derive(PartialEq, Debug, Clone)]
pub struct CPU {
    regs: CpuRegisters,
    flags: u8,
    pub cycle_count: u64
}

impl CPU {
    pub fn new(flags: u8) -> CPU {
        CPU {
            regs: CpuRegisters {
                ip: 0x00000000,
                sp: 0xFFFFFFF0,
                sb: 0xFFFFFFF0,
                gp: [0; GP_REGISTER_COUNT],
                cr: 0,
                ds: 0,
                flags: 0x00,
            },
            flags,
            cycle_count: 0
        }
    }

    pub fn execute(&mut self, process: &mut Process, memory: &mut PhysicalMemory, quota_instr: Option<u32>) -> Result<(), Interruption> {
        if process.is_created() {
            self.regs.ip = process.text_entry_addr;
            self.regs.ds = process.data_segment_addr;
        }
        process.set_running();
        let mut instr_count: u32 = 0;

        while process.is_running() && (quota_instr.is_none() || instr_count < quota_instr.unwrap()) {
            match process.debugger.tick() {
                DebuggerAction::LetGo => {
                    let result = self.decode_instruction(process, memory);
                    match result {
                        Ok(_) => {
                            instr_count += 1;
                            self.cycle_count += 1;
                        }
                        Err(Interruption::ProcessFault) | Err(Interruption::ProcessTermination) => {
                            process.stop();
                            break;
                        }
                        Err(Interruption::GraphicsInit(_, _) | Interruption::GraphicsUpdate(_)) => {
                            process.suspend();
                            return result;
                        },
                        Err(Interruption::KernelPanic) => return Err(Interruption::KernelPanic)
                    }
                }
                DebuggerAction::PrintRegisters => {
                    println!("{:?}", self.regs);
                }
                DebuggerAction::PrintStack => {
                    print_stack(process, memory, self.regs.sp, self.regs.sb);
                }
                DebuggerAction::PrintHeap => {
                    print_heap(process, memory);
                }
                DebuggerAction::PrintData => {
                    print_data(process, memory);
                }
                DebuggerAction::PrintMemoryAt(addr) => {
                    print_memory(process, memory, addr);
                }
            }
        }
        if quota_instr.is_some() && instr_count >= quota_instr.unwrap() {
            process.suspend();
        }
        Ok(())
    }

    fn decode_instruction(&mut self, process: &mut Process, memory: &mut PhysicalMemory) -> Result<(), Interruption> {
        self.read_debug_symbol(process, memory);

        let op_code = memory.read_u8(process, self.regs.ip);
        self.regs.ip += 1;
        match op_code {
            common::OPCODE_NOP => return Ok(()),
            _ => {}
        }
        let instr_flags: u8 = memory.read_u8(process, self.regs.ip);
        let mut instr_size: u8 = instr_flags & 3; // bit 0 and 1
        if instr_size == 3 { // uncompress the L value
            instr_size = 4;
        }
        let flag_addr1 = (instr_flags & 32) != 0; // bit 5
        let flag_addr2 = (instr_flags & 16) != 0; // bit 4
        let flag_imm1 = (instr_flags & 8) != 0; // bit 3
        let flag_imm2 = (instr_flags & 4) != 0; // bit 2
        let segment_type = instr_flags & 3; // for labels only (replace the instr_size)
        self.regs.ip += 1;

        match op_code {
            common::OPCODE_PUSH => {
                let value = self.read_operand(instr_size, flag_imm1, flag_addr1, process, memory);
                self.stack_push(instr_size, process, memory, value);
            }
            common::OPCODE_POP => {
                let value = self.stack_pop(instr_size, process, memory);
                self.write_operand(instr_size, process, memory, value);
            }
            common::OPCODE_MOV => {
                let src_value = self.read_operand(instr_size, flag_imm1, flag_addr1, process, memory);
                self.write_operand(instr_size, process, memory, src_value);
            }
            common::OPCODE_ADD => {
                self.decode_arithmetic_32(process, memory, flag_imm1, flag_addr1, |a, b| a.overflowing_add(b));
            }
            common::OPCODE_SUB => {
                self.decode_arithmetic_32(process, memory, flag_imm1, flag_addr1, |a, b| a.overflowing_sub(b));
            }
            common::OPCODE_MUL => {
                self.decode_arithmetic_32(process, memory, flag_imm1, flag_addr1, |a, b| a.overflowing_mul(b));
            }
            common::OPCODE_DIV => {
                self.decode_arithmetic_32(process, memory, flag_imm1, flag_addr1, |a, b| a.overflowing_div(b));
            }
            common::OPCODE_FADD => {
                self.decode_arithmetic_f32(process, memory, flag_imm1, flag_addr1, |a, b| a + b);
            }
            common::OPCODE_FSUB => {
                self.decode_arithmetic_f32(process, memory, flag_imm1, flag_addr1, |a, b| a - b);
            }
            common::OPCODE_FMUL => {
                self.decode_arithmetic_f32(process, memory, flag_imm1, flag_addr1, |a, b| a * b);
            }
            common::OPCODE_FDIV => {
                self.decode_arithmetic_f32(process, memory, flag_imm1, flag_addr1, |a, b| a / b);
            }
            common::OPCODE_AND => {
                self.decode_bitwise_32(process, memory, flag_imm1, flag_addr1, |a, b| a & b);
            }
            common::OPCODE_OR => {
                self.decode_bitwise_32(process, memory, flag_imm1, flag_addr1, |a, b| a | b);
            }
            common::OPCODE_XOR => {
                self.decode_bitwise_32(process, memory, flag_imm1, flag_addr1, |a, b| a ^ b);
            }
            common::OPCODE_SHL => {
                self.decode_bitwise_32(process, memory, flag_imm1, flag_addr1, |a, b| a << b);
            }
            common::OPCODE_SHR => {
                self.decode_bitwise_32(process, memory, flag_imm1, flag_addr1, |a, b| a >> b);
            }
            common::OPCODE_NOT => {
                let ip_backup = self.regs.ip;
                let value = self.read_operand(instr_size, flag_imm1, flag_addr1, process, memory);
                self.regs.ip = ip_backup; // undo first read
                self.write_operand(instr_size, process, memory, !value);
            }
            common::OPCODE_CVTI2F => {
                let ip_backup = self.regs.ip;
                let int_value = self.read_operand(instr_size, flag_imm1, flag_addr1, process, memory);
                self.regs.ip = ip_backup; // undo first read
                self.write_operand(instr_size, process, memory, f32::to_bits(int_value as f32));
            }
            common::OPCODE_CMP => {
                let op1_value = self.read_operand(instr_size, flag_imm1, flag_addr1, process, memory);
                let op2_value = self.read_operand(instr_size, flag_imm2, flag_addr2, process, memory);
                let (result, overflow) = op1_value.overflowing_sub(op2_value);
                self.compute_flags_32(result, overflow);
            }
            common::OPCODE_FCMP => {
                let op1_value = f32::from_bits(self.read_operand(instr_size, flag_imm1, flag_addr1, process, memory));
                let op2_value = f32::from_bits(self.read_operand(instr_size, flag_imm2, flag_addr2, process, memory));
                let result = op1_value - op2_value;
                self.compute_flags_f32(result);
            }
            x if x == common::OPCODE_JG || x == common::OPCODE_SETG => {
                self.flags_jump_or_set(process, memory, segment_type, |zf, sf| zf == 0 && sf == 0, x == common::OPCODE_JG);
            }
            x if x == common::OPCODE_JGE || x == common::OPCODE_SETGE => {
                self.flags_jump_or_set(process, memory, segment_type, |zf, sf| zf == 1 || sf == 0, x == common::OPCODE_JGE);
            }
            x if x == common::OPCODE_JL || x == common::OPCODE_SETL => {
                self.flags_jump_or_set(process, memory, segment_type, |zf, sf| zf == 0 && sf == 1, x == common::OPCODE_JL);
            }
            x if x == common::OPCODE_JLE || x == common::OPCODE_SETLE => {
                self.flags_jump_or_set(process, memory, segment_type, |zf, sf| zf == 1 || sf == 1, x == common::OPCODE_JLE);
            }
            x if x == common::OPCODE_JE || x == common::OPCODE_SETE => {
                self.flags_jump_or_set(process, memory, segment_type, |zf, _| zf == 1, x == common::OPCODE_JE);
            }
            x if x == common::OPCODE_JNE || x == common::OPCODE_SETNE => {
                self.flags_jump_or_set(process, memory, segment_type, |zf, _| zf == 0, x == common::OPCODE_JNE);
            }
            common::OPCODE_JMP => {
                let jmp_addr= self.decode_immediate_memory_address(process, memory, segment_type);
                self.regs.ip = jmp_addr;
            }
            common::OPCODE_CALL => {
                let call_addr= self.decode_immediate_memory_address(process, memory, segment_type);
                self.stack_push(4, process, memory, self.regs.ip); // push %ip return addr
                self.regs.ip = call_addr;
            }
            common::OPCODE_DYNAMIC_CALL => {
                let call_addr: u32 = self.read_operand(instr_size, flag_imm1, flag_addr1, process, memory);
                self.stack_push(4, process, memory, self.regs.ip); // push %ip return addr
                self.regs.ip = self.translate_memory_address(process, call_addr, TEXT_SEGMENT_TYPE);
            }
            common::OPCODE_RET => {
                let return_addr = self.stack_pop(4, process, memory);
                self.regs.ip = return_addr;
            }
            common::OPCODE_SYSCALL => {
                let syscall_num = instr_flags;
                let value = execute_syscall(self.regs.sb, self.regs.sp, process, memory, syscall_num)?;
                self.regs.cr = value;
            }
            _ => {
                error!("Unsupported op code {op_code} of size {instr_size} (%ip: {})", self.regs.ip);
                return Err(Interruption::KernelPanic);
            }
        };
        Ok(())
    }

    fn decode_arithmetic_32(&mut self, process: &mut Process, memory: &mut PhysicalMemory, flag_imm1: bool, flag_addr1: bool, func: fn(u32, u32) -> (u32, bool)) {
        let src_value = self.read_operand(4, flag_imm1, flag_addr1, process, memory);
        let ip_backup = self.regs.ip;
        let dst_value = self.read_operand(4, false, false, process, memory); // TODO cannot be immediate
        self.regs.ip = ip_backup; // undo first read
        let (result, overflow) = func(dst_value, src_value);
        self.write_operand(4, process, memory, result);
        self.compute_flags_32(result, overflow);
    }

    fn decode_arithmetic_f32(&mut self, process: &mut Process, memory: &mut PhysicalMemory, flag_imm1: bool, flag_addr1: bool, func: fn(f32, f32) -> f32) {
        let src_value = f32::from_bits(self.read_operand(4, flag_imm1, flag_addr1, process, memory));
        let ip_backup = self.regs.ip;
        let dst_value = f32::from_bits(self.read_operand(4, false, false, process, memory));
        self.regs.ip = ip_backup; // undo first read
        let result = func(dst_value, src_value);
        self.write_operand(4, process, memory, f32::to_bits(result));
        self.compute_flags_f32(result);
    }

    fn decode_bitwise_32(&mut self, process: &mut Process, memory: &mut PhysicalMemory, flag_imm1: bool, flag_addr1: bool, func: fn(u32, u32) -> u32) {
        let src_value = self.read_operand(4, flag_imm1, flag_addr1, process, memory);
        let ip_backup = self.regs.ip;
        let dst_value = self.read_operand(4, false, false, process, memory); // TODO cannot be immediate
        self.regs.ip = ip_backup; // undo first read
        let result = func(dst_value, src_value);
        self.write_operand(4, process, memory, result);
    }

    fn flags_jump_or_set(&mut self, process: &mut Process, memory: &mut PhysicalMemory, segment_type: u8, check_condition: fn(u8, u8) -> bool, do_jump: bool) {
        let test = check_condition((self.regs.flags >> ZERO_FLAG_SHIFT) & 1, (self.regs.flags >> SIGN_FLAG_SHIFT) & 1);
        if do_jump {
            let jmp_addr= self.decode_immediate_memory_address(process, memory, segment_type);
            if test {
                // jump if true
                self.regs.ip = jmp_addr;
            }
        } else {
            // set bool value in operand
            self.write_operand(1, process, memory, if test { 1 } else { 0 })
        }
    }

    fn compute_flags_32(&mut self, value: u32, overflow: bool) {
        let zf = (value == 0) as u8;
        let sf = ((value >> 31) & 1) as u8;
        let of = overflow as u8;
        self.regs.flags = (zf << ZERO_FLAG_SHIFT) | (sf << SIGN_FLAG_SHIFT) | (of << OVERFLOW_FLAG_SHIFT)
    }

    fn compute_flags_f32(&mut self, value: f32) {
        let zf = (value == 0.0) as u8;
        let sf = (value < 0.0) as u8;
        self.regs.flags = (zf << ZERO_FLAG_SHIFT) | (sf << SIGN_FLAG_SHIFT)
    }

    fn read_operand(&mut self, word_size: u8, is_immediate: bool, is_addr: bool, process: &mut Process, memory: &mut PhysicalMemory) -> u32 {
        if is_immediate {
            let imm_value = memory.read_word(word_size, process, self.regs.ip);
            self.regs.ip += word_size as u32;
            imm_value
        } else if is_addr {
            let segment_type = memory.read_u8(process, self.regs.ip);
            self.regs.ip += 1;
            self.decode_immediate_memory_address(process, memory, segment_type)
        } else {
            let reg_byte = memory.read_u8(process, self.regs.ip);
            self.regs.ip += 1;
            let is_ref = (reg_byte & 0x04) != 0;
            let reg_code = reg_byte >> 3;
            if is_ref {
                let address: u32 = self.decode_register_memory_address(process, memory, reg_code, reg_byte);
                memory.read_word(word_size, process, address)
            } else {
                self.read_register_value(reg_code)
            }
        }
    }

    fn write_operand(&mut self, word_size: u8, process: &mut Process, memory: &mut PhysicalMemory, value: u32) {
        let reg_byte = memory.read_u8(process, self.regs.ip);
        self.regs.ip += 1;
        let is_ref = (reg_byte & 0x04) != 0;
        let reg_code = reg_byte >> 3;
        if is_ref {
            let address = self.decode_register_memory_address(process, memory, reg_code, reg_byte);
            memory.write_word(word_size, process, address, value);
        } else {
            self.write_register_value(reg_code, value);
        }
    }

    fn decode_register_memory_address(&mut self, process: &mut Process, memory: &mut PhysicalMemory, reg_code: u8, reg_byte: u8) -> u32 {
        let has_offset = (reg_byte & 0x02) != 0;
        let has_scale = (reg_byte & 0x01) != 0;
        let mut address: u32 = self.read_register_value(reg_code);

        if has_offset {
            let offset_word: i16 = memory.read_i16(process, self.regs.ip);
            self.regs.ip += 2;
            if offset_word < 0 {
                address -= (-1 * offset_word) as u32;
            } else {
                address += offset_word as u32;
            }
        }
        if has_scale {
            let scale_byte = memory.read_u8(process, self.regs.ip);
            self.regs.ip += 1;
            let scale_value = (scale_byte & 0x07) as u32;
            let scale_reg_code = scale_byte >> 3;
            let scale = self.read_register_value(scale_reg_code) * scale_value;
            address += scale;
        }
        address
    }

    fn decode_immediate_memory_address(&mut self, process: &mut Process, memory: &mut PhysicalMemory, segment_type: u8) -> u32 {
        let addr = memory.read_u32(process, self.regs.ip);
        self.regs.ip += 4;
        self.translate_memory_address(process, addr, segment_type)
    }

    fn translate_memory_address(&self, process: &mut Process, addr: u32, segment_type: u8) -> u32 {
        let base_addr = match segment_type {
            DATA_SEGMENT_TYPE => process.data_segment_addr,
            TEXT_SEGMENT_TYPE => process.text_segment_addr,
            _ => panic!("Unsupported segment type {segment_type}"),
        };
        base_addr + addr
    }

    fn read_register_value(&self, reg_code: u8) -> u32 {
        match reg_code {
            code if code < GP_REGISTER_COUNT as u8 => self.regs.gp[code as usize],
            code if code == SP_REGISTER_ID => self.regs.sp,
            code if code == SB_REGISTER_ID => self.regs.sb,
            code if code == CR_REGISTER_ID => self.regs.cr,
            code if code == DS_REGISTER_ID => self.regs.ds,
            _ => panic!("Unsupported register code {reg_code}")
        }
    }

    fn write_register_value(&mut self, reg_code: u8, value: u32) {
        match reg_code {
            code if code < GP_REGISTER_COUNT as u8 => self.regs.gp[code as usize] = value,
            code if code == SP_REGISTER_ID => self.regs.sp = value,
            code if code == SB_REGISTER_ID => self.regs.sb = value,
            code if code == CR_REGISTER_ID => self.regs.cr = value,
            _ => panic!("Unsupported register code {reg_code}")
        }
    }

    fn stack_push(&mut self, word_size: u8, process: &mut Process, memory: &mut PhysicalMemory, value: u32) {
        // before: high [BYTE, FREE, FREE, FREE, FREE] low
        //                ^
        // after: high  [BYTE, BYTE, BYTE, BYTE, BYTE] low
        //                       3     2     1    0^
        self.regs.sp -= word_size as u32;
        memory.write_word(word_size, process, self.regs.sp, value);
    }

    fn stack_pop(&mut self, word_size: u8, process: &mut Process, memory: &mut PhysicalMemory) -> u32 {
        // before: high [BYTE, BYTE, BYTE, BYTE, BYTE] low
        //                      3     2     1     0^
        // after : high [BYTE, FREE, FREE, FREE, FREE] low
        //                 ^
        let value = memory.read_word(word_size, process, self.regs.sp);
        self.regs.sp += word_size as u32;
        value
    }

    fn read_debug_symbol(&mut self, process: &mut Process, memory: &mut PhysicalMemory) {
        if process.debug_segment_addr.is_some() {
            let debug_symbol_addr = memory.read_u32(process, self.regs.ip);
            if self.flags & FLAG_ENABLE_TRACE != 0 {
                let symbol = memory.read_string(process, process.debug_segment_addr.unwrap() + debug_symbol_addr);
                print!("{}: {}", self.cycle_count, symbol);
            }
            self.regs.ip += 4;
        }
    }
}
