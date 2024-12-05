/*
  Register constants
*/
pub const GP_REGISTER_COUNT: usize = 16;
pub const IP_REGISTER_ID: u8 = (GP_REGISTER_COUNT + 0) as u8;
pub const SP_REGISTER_ID: u8 = (GP_REGISTER_COUNT + 1) as u8;
pub const SB_REGISTER_ID: u8 = (GP_REGISTER_COUNT + 2) as u8;
pub const CR_REGISTER_ID: u8 = (GP_REGISTER_COUNT + 4) as u8;
pub const DS_REGISTER_ID: u8 = (GP_REGISTER_COUNT + 5) as u8;

/*
 Segment constants
*/
pub const DATA_SEGMENT_TYPE: u8 = 1;
pub const TEXT_SEGMENT_TYPE: u8 = 2;

/*
 Opcode constants
*/
pub const OPCODE_ADD: u8 = 1;
pub const OPCODE_SUB: u8 = 2;
pub const OPCODE_MUL: u8 = 3;
pub const OPCODE_DIV: u8 = 4;
pub const OPCODE_NOT: u8 = 5;
pub const OPCODE_CMP: u8 = 7;
pub const OPCODE_JG: u8 = 8;
pub const OPCODE_JGE: u8 = 9;
pub const OPCODE_JL: u8 = 10;
pub const OPCODE_JLE: u8 = 11;
pub const OPCODE_JE: u8 = 12;
pub const OPCODE_JNE: u8 = 13;
pub const OPCODE_JMP: u8 = 16;
pub const OPCODE_MOV: u8 = 17;
pub const OPCODE_PUSH: u8 = 18;
pub const OPCODE_POP: u8 = 19;
pub const OPCODE_CALL: u8 = 20;
pub const OPCODE_SYSCALL: u8 = 21;
pub const OPCODE_RET: u8 = 22;
pub const OPCODE_SETG: u8 = 23;
pub const OPCODE_SETGE: u8 = 24;
pub const OPCODE_SETL: u8 = 25;
pub const OPCODE_SETLE: u8 = 26;
pub const OPCODE_SETE: u8 = 27;
pub const OPCODE_SETNE: u8 = 28;
pub const OPCODE_DYNAMIC_CALL: u8 = 29;
pub const OPCODE_FADD: u8 = 30;
pub const OPCODE_FSUB: u8 = 31;
pub const OPCODE_FMUL: u8 = 32;
pub const OPCODE_FDIV: u8 = 33;
pub const OPCODE_FCMP: u8 = 34;
pub const OPCODE_AND: u8 = 35;
pub const OPCODE_OR: u8 = 36;
pub const OPCODE_XOR: u8 = 37;
pub const OPCODE_SHL: u8 = 38;
pub const OPCODE_SHR: u8 = 39;
pub const OPCODE_CVTI2F: u8 = 40;
pub const OPCODE_NOP: u8 = 63;

/*
  Syscall constants
*/
pub const SYSCALL_SYSTEM_EXIT: u8 = 0;
pub const SYSCALL_SYSTEM_MALLOC: u8 = 1;
pub const SYSCALL_STRING_EQUALS: u8 = 10;
pub const SYSCALL_STRING_LEN: u8 = 11;
pub const SYSCALL_STRING_SUBSTRING: u8 = 12;
pub const SYSCALL_ARRAY_ALLOC: u8 = 13;
pub const SYSCALL_ARRAY_GET: u8 = 14;
pub const SYSCALL_ARRAY_PUT: u8 = 15;
pub const SYSCALL_IO_IN_STRING: u8 = 16;
pub const SYSCALL_IO_OUT_STRING: u8 = 17;
pub const SYSCALL_IO_OUT_INT: u8 = 18;
pub const SYSCALL_IO_OUT_FLOAT: u8 = 19;
pub const SYSCALL_IO_OUT_NL: u8 = 20;
pub const SYSCALL_ABORT: u8 = 21;
pub const SYSCALL_BREAKPOINT: u8 = 22;
pub const SYSCALL_CANVAS_INIT: u8 = 23;
pub const SYSCALL_CANVAS_UPDATE: u8 = 24;

pub fn syscall_number(syscall_name: &str) -> u8 {
    match syscall_name {
        "System::exit" => SYSCALL_SYSTEM_EXIT,
        "String::length" => SYSCALL_STRING_LEN,
        "String::substring" => SYSCALL_STRING_SUBSTRING,
        "String::equals" => SYSCALL_STRING_EQUALS,
        "Array::alloc" => SYSCALL_ARRAY_ALLOC,
        "Array::get" => SYSCALL_ARRAY_GET,
        "Array::put" => SYSCALL_ARRAY_PUT,
        "Any::in_string" => SYSCALL_IO_IN_STRING,
        "Any::out_string" => SYSCALL_IO_OUT_STRING,
        "Any::out_int" => SYSCALL_IO_OUT_INT,
        "Any::out_float" => SYSCALL_IO_OUT_FLOAT,
        "Any::out_nl" => SYSCALL_IO_OUT_NL,
        "Any::abort" => SYSCALL_ABORT,
        "Any::breakpoint" => SYSCALL_BREAKPOINT,
        "Canvas::init" => SYSCALL_CANVAS_INIT,
        "Canvas::update" => SYSCALL_CANVAS_UPDATE,
        _ => panic!("Unsupported syscall {syscall_name}"),
    }
}

/*
  Executable flags
*/
pub const EXEC_DEBUG_FLAG: u8 = 1;

pub const OBJECT_HEADER_SIZE: u16 = 4; // 4-byte vtable pointer
