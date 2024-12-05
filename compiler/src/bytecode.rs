//!
//! Bytecode internal representation (IR) type definitions and helpers
//!
use crate::bytecode::OpCode::{Add, Cmp, Div, Mov, Mul, Nop, Not, Pop, Push, Sub};
use crate::bytecode::Operand::{
    DataAddress, MemoryAtRegister, MemoryAtRegisterOffset, RegisterValue, ThisFieldOffset,
};
use crate::bytecode::Register::SB;
use crate::scope::Scope;
use common::{CR_REGISTER_ID, DS_REGISTER_ID, SB_REGISTER_ID, SP_REGISTER_ID};

pub type Label = String;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum Register {
    GP(usize),      // General Purpose, limited to 16 slots
    SP,             // Stack Pointer
    SB,             // Stack Base
    CR,             // Call result
    Virtual(usize), // Virtual Register, unlimited slots, only for the internal representation
    DS,             // Data segment
}

impl Register {
    pub fn code(&self) -> u8 {
        match self {
            Register::GP(i) => *i as u8,
            Register::SP => SP_REGISTER_ID,
            Register::SB => SB_REGISTER_ID,
            Register::CR => CR_REGISTER_ID,
            Register::Virtual(i) => *i as u8,
            Register::DS => DS_REGISTER_ID,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum Operand {
    ImmediateValue(i32),                                      // ex: 1
    ImmediateFloatValue(u32), // ex: f32 casted as u32 (f32 cannot be hashed nor has equality)
    RegisterValue(Register),  // ex: %r1
    MemoryAtRegister(Register), // ex: (%r1)
    MemoryAtRegisterOffset(Register, i16), // ex: -2(%r1)
    MemoryAtRegisterOffsetScale(Register, i16, Register, u8), // ex: 8(%r1, %r2, 4]
    DataAddress(Label),       // ex: label_0

    // below values are temporary and must be removed from final bytecode
    ThisFieldOffset(u16), // offset
    BoolCondition(u8),    // bool operator code
}

impl Operand {
    pub fn is_immediate(&self) -> bool {
        match self {
            Operand::ImmediateValue(_) => true,
            Operand::ImmediateFloatValue(_) => true,
            _ => false,
        }
    }

    pub fn is_memory_access(&self) -> bool {
        match self {
            MemoryAtRegister(_)
            | MemoryAtRegisterOffset(_, _)
            | Operand::MemoryAtRegisterOffsetScale(_, _, _, _) => true,
            DataAddress(_) => true,
            ThisFieldOffset(_) => true,
            _ => false,
        }
    }

    pub fn is_addr(&self) -> bool {
        match self {
            DataAddress(_) => true,
            _ => false,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum OpCode {
    // arithmetic on integers
    Add(Operand, Operand), // dst, src
    Sub(Operand, Operand),
    Mul(Operand, Operand),
    Div(Operand, Operand),
    Cmp(Operand, Operand),

    // arithmetic on floats
    FAdd(Operand, Operand), // dst, src
    FSub(Operand, Operand),
    FMul(Operand, Operand),
    FDiv(Operand, Operand),
    FCmp(Operand, Operand),

    // float conversion
    Cvti2f(Operand),

    // bit-wise
    Not(Operand),
    And(Operand, Operand), // dst, src
    Or(Operand, Operand),
    Xor(Operand, Operand),
    Shl(Operand, Operand),
    Shr(Operand, Operand),

    // (un)conditional jumps
    Jg(Label),
    Jge(Label),
    Jl(Label),
    Jle(Label),
    Je(Label),
    Jne(Label),
    Jmp(Label),

    // set 0 or 1 value based on condition flags
    SetG(Operand),
    SetGe(Operand),
    SetL(Operand),
    SetLe(Operand),
    SetE(Operand),
    SetNe(Operand),

    // memory access
    Mov(Operand, Operand), // dst, src
    Push(Operand),
    Pop(Operand),

    Call(Label),
    DynamicCall(Operand), // call with dynamic operand

    Syscall(u8), // system call
    Ret,         // return from call

    Nop,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Instr {
    pub label: Option<String>,
    pub op_code: OpCode,
    pub descr: Option<String>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum DataValue {
    Ascii(String),
    //Byte(u8),
    DataPtr(Label),
    TextPtr(Label),
    Skip(u32),
}

#[derive(PartialEq, Debug, Clone)]
pub struct DataItem {
    pub label: Option<String>,
    pub value: DataValue,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Bytecode {
    pub text: Vec<Instr>,
    pub data: Vec<DataItem>,

    label_next: Option<Label>,
}

impl Bytecode {
    pub fn new() -> Bytecode {
        Bytecode {
            text: vec![],
            data: vec![],
            label_next: None,
        }
    }

    pub fn set_label_next(&mut self, label: Label) {
        self.flush_instr();
        self.label_next = Some(label);
    }

    pub fn emit(&mut self, op_code: OpCode, descr: Option<&str>) {
        let label = self.label_next.take();
        self.text.push(Instr {
            op_code,
            label,
            descr: descr.map(|d| String::from(d)),
        });
    }

    pub fn emit_first(&mut self, op_code: OpCode, descr: Option<&str>) {
        let label = self.label_next.take();
        self.text.insert(
            0,
            Instr {
                op_code,
                label,
                descr: descr.map(|d| String::from(d)),
            },
        );
    }

    pub fn flush_instr(&mut self) {
        if self.label_next.is_some() {
            self.emit(Nop, None); // write last label
        }
    }

    pub fn emit_data(&mut self, label: Label, value: DataValue) -> Operand {
        self.data.push(DataItem {
            label: Some(label.clone()),
            value,
        });
        DataAddress(label)
    }

    pub fn emit_data_without_label(&mut self, value: DataValue) {
        self.data.push(DataItem { label: None, value });
    }

    pub fn append(&mut self, other: Bytecode) {
        for instr in other.text {
            self.text.push(instr.clone());
        }
        for item in other.data {
            self.data.push(item.clone());
        }
    }

    fn expand_this_operand(&mut self, op: &Operand, scope: &mut Scope) -> Operand {
        match &op {
            ThisFieldOffset(offset) => {
                let virtual_reg = scope.alloc_virtual_register_from(op);
                self.emit(
                    Mov(RegisterValue(virtual_reg.clone()), MemoryAtRegister(SB)),
                    Some("load 'this' base address"),
                );
                MemoryAtRegisterOffset(virtual_reg, *offset as i16)
            }
            _ => op.clone(),
        }
    }

    fn expand_read_operand(&mut self, read_operand: &Operand, scope: &mut Scope) -> Operand {
        let op = self.expand_this_operand(read_operand, scope);
        match &op {
            MemoryAtRegister(_)
            | MemoryAtRegisterOffset(_, _)
            | Operand::MemoryAtRegisterOffsetScale(_, _, _, _) => {
                let virtual_reg = scope.alloc_virtual_register_from(&op);
                self.emit(
                    Mov(RegisterValue(virtual_reg.clone()), op.clone()),
                    Some("load temp value"),
                );
                RegisterValue(virtual_reg)
            }
            _ => op.clone(),
        }
    }

    fn expand_instruction(&mut self, instr: &Instr, scope: &mut Scope) {
        if instr.label.is_some() {
            self.set_label_next(instr.label.clone().unwrap());
        }
        match &instr.op_code {
            Add(dst, src) => {
                let mut src_exp = self.expand_this_operand(src, scope);
                let dst_exp = self.expand_this_operand(dst, scope);
                if src_exp.is_memory_access() && dst_exp.is_memory_access() {
                    src_exp = self.expand_read_operand(&src_exp, scope);
                }
                self.emit(Add(dst_exp, src_exp), instr.descr.as_deref().clone());
            }
            Sub(dst, src) => {
                let mut src_exp = self.expand_this_operand(src, scope);
                let dst_exp = self.expand_this_operand(dst, scope);
                if src_exp.is_memory_access() && dst_exp.is_memory_access() {
                    src_exp = self.expand_read_operand(&src_exp, scope);
                }
                self.emit(Sub(dst_exp, src_exp), instr.descr.as_deref().clone());
            }
            Mul(dst, src) => {
                let mut src_exp = self.expand_this_operand(src, scope);
                let dst_exp = self.expand_this_operand(dst, scope);
                if src_exp.is_memory_access() && dst_exp.is_memory_access() {
                    src_exp = self.expand_read_operand(&src_exp, scope);
                }
                self.emit(Mul(dst_exp, src_exp), instr.descr.as_deref().clone());
            }
            Div(dst, src) => {
                let mut src_exp = self.expand_this_operand(src, scope);
                let dst_exp = self.expand_this_operand(dst, scope);
                if src_exp.is_memory_access() && dst_exp.is_memory_access() {
                    src_exp = self.expand_read_operand(&src_exp, scope);
                }
                self.emit(Div(dst_exp, src_exp), instr.descr.as_deref().clone());
            }
            Not(op) => {
                let op_exp = self.expand_this_operand(op, scope);
                self.emit(Not(op_exp), instr.descr.as_deref().clone());
            }
            Cmp(op1, op2) => {
                let mut op1_exp = self.expand_this_operand(op1, scope);
                let op2_exp = self.expand_this_operand(op2, scope);
                if op1_exp.is_memory_access() && op2_exp.is_memory_access() {
                    op1_exp = self.expand_read_operand(&op1_exp, scope);
                }
                self.emit(Cmp(op1_exp, op2_exp), instr.descr.as_deref().clone());
            }
            Mov(dst, src) => {
                let mut src_exp = self.expand_this_operand(src, scope);
                let dst_exp = self.expand_this_operand(dst, scope);
                if src_exp.is_memory_access() && dst_exp.is_memory_access() {
                    src_exp = self.expand_read_operand(&src_exp, scope);
                }
                self.emit(Mov(dst_exp, src_exp), instr.descr.as_deref().clone());
            }
            Push(op) => {
                let op_exp = self.expand_this_operand(op, scope);
                self.emit(Push(op_exp), instr.descr.as_deref().clone());
            }
            Pop(op) => {
                let op_exp = self.expand_this_operand(op, scope);
                self.emit(Pop(op_exp), instr.descr.as_deref().clone());
            }
            _ => self.emit(instr.op_code.clone(), instr.descr.as_deref().clone()),
        }
    }

    pub fn expand(&mut self, scope: &mut Scope) {
        let mut expanded = Bytecode::new();
        for instr in &self.text {
            expanded.expand_instruction(instr, scope);
        }
        self.text = expanded.text;
    }

    pub fn print_text_assembly(&self, entry: &str) -> String {
        let mut out = String::new();
        out.push_str(format!(".entry {entry}\n").as_str());
        out.push_str(".data\n");
        for item in &self.data {
            out.push_str("  ");
            match &item.label {
                Some(label) => out.push_str(label.as_str()),
                None => {}
            }
            match &item.value {
                DataValue::Ascii(s) => {
                    out.push_str(" .ascii \"");
                    out.push_str(s);
                    out.push_str("\"");
                }
                DataValue::DataPtr(label) => {
                    out.push_str(" .dataptr $");
                    out.push_str(label);
                }
                DataValue::TextPtr(label) => {
                    out.push_str(" .textptr $");
                    out.push_str(label);
                }
                DataValue::Skip(size) => {
                    out.push_str(" .skip ");
                    out.push_str(size.to_string().as_str());
                }
            }
            out.push_str("\n");
        }
        out.push_str(".text\n");
        for instr in &self.text {
            self.print_instr(instr, &mut out);
        }
        out
    }

    pub fn print_instr(&self, instr: &Instr, out: &mut String) {
        match &instr.label {
            Some(label) => {
                out.push_str("  ");
                out.push_str(label);
                out.push_str(":\n");
            }
            None => {}
        }
        out.push_str("      ");
        let mut op_code = String::new();
        let mut operands = String::new();
        match &instr.op_code {
            Add(dst, src) => {
                op_code.push_str("add");
                self.print_operand(dst, &mut operands);
                operands.push_str(", ");
                self.print_operand(src, &mut operands);
            }
            Sub(dst, src) => {
                op_code.push_str("sub");
                self.print_operand(dst, &mut operands);
                operands.push_str(", ");
                self.print_operand(src, &mut operands);
            }
            Mul(dst, src) => {
                op_code.push_str("mul");
                self.print_operand(dst, &mut operands);
                operands.push_str(", ");
                self.print_operand(src, &mut operands);
            }
            Div(dst, src) => {
                op_code.push_str("div");
                self.print_operand(dst, &mut operands);
                operands.push_str(", ");
                self.print_operand(src, &mut operands);
            }
            Cmp(a, b) => {
                op_code.push_str("cmp");
                self.print_operand(a, &mut operands);
                operands.push_str(", ");
                self.print_operand(b, &mut operands);
            }
            OpCode::FAdd(dst, src) => {
                op_code.push_str("fadd");
                self.print_operand(dst, &mut operands);
                operands.push_str(", ");
                self.print_operand(src, &mut operands);
            }
            OpCode::FSub(dst, src) => {
                op_code.push_str("fsub");
                self.print_operand(dst, &mut operands);
                operands.push_str(", ");
                self.print_operand(src, &mut operands);
            }
            OpCode::FMul(dst, src) => {
                op_code.push_str("fmul");
                self.print_operand(dst, &mut operands);
                operands.push_str(", ");
                self.print_operand(src, &mut operands);
            }
            OpCode::FDiv(dst, src) => {
                op_code.push_str("fdiv");
                self.print_operand(dst, &mut operands);
                operands.push_str(", ");
                self.print_operand(src, &mut operands);
            }
            OpCode::FCmp(a, b) => {
                op_code.push_str("fcmp");
                self.print_operand(a, &mut operands);
                operands.push_str(", ");
                self.print_operand(b, &mut operands);
            }
            OpCode::Cvti2f(value) => {
                op_code.push_str("cvti2f");
                self.print_operand(value, &mut operands);
            }
            OpCode::Jg(label) => {
                op_code.push_str("jg");
                operands.push_str(label);
            }
            OpCode::Jge(label) => {
                op_code.push_str("jge");
                operands.push_str(label);
            }
            OpCode::Jl(label) => {
                op_code.push_str("jl");
                operands.push_str(label);
            }
            OpCode::Jle(label) => {
                op_code.push_str("jle");
                operands.push_str(label);
            }
            OpCode::Je(label) => {
                op_code.push_str("je");
                operands.push_str(label);
            }
            OpCode::Jne(label) => {
                op_code.push_str("jne");
                operands.push_str(label);
            }
            OpCode::Jmp(label) => {
                op_code.push_str("jmp");
                operands.push_str(label);
            }
            Mov(dst, src) => {
                op_code.push_str("mov");
                self.print_operand(dst, &mut operands);
                operands.push_str(", ");
                self.print_operand(src, &mut operands);
            }
            Push(value) => {
                op_code.push_str("push");
                self.print_operand(value, &mut operands);
            }
            Pop(value) => {
                op_code.push_str("pop");
                self.print_operand(value, &mut operands);
            }
            OpCode::Call(value) => {
                op_code.push_str("call ");
                operands.push_str(value);
            }
            OpCode::DynamicCall(op) => {
                op_code.push_str("dyncall ");
                self.print_operand(op, &mut operands);
            }
            Not(value) => {
                op_code.push_str("not");
                self.print_operand(value, &mut operands);
            }
            OpCode::Syscall(number) => {
                op_code.push_str("syscall");
                operands.push_str(number.to_string().as_str());
            }
            OpCode::Ret => {
                op_code.push_str("ret");
            }
            Nop => op_code.push_str("nop"),
            OpCode::SetG(value) => {
                op_code.push_str("setg");
                self.print_operand(value, &mut operands);
            }
            OpCode::SetGe(value) => {
                op_code.push_str("setge");
                self.print_operand(value, &mut operands);
            }
            OpCode::SetL(value) => {
                op_code.push_str("setl");
                self.print_operand(value, &mut operands);
            }
            OpCode::SetLe(value) => {
                op_code.push_str("setle");
                self.print_operand(value, &mut operands);
            }
            OpCode::SetE(value) => {
                op_code.push_str("sete");
                self.print_operand(value, &mut operands);
            }
            OpCode::SetNe(value) => {
                op_code.push_str("setne");
                self.print_operand(value, &mut operands);
            }
            OpCode::And(dst, src) => {
                op_code.push_str("and");
                self.print_operand(dst, &mut operands);
                operands.push_str(", ");
                self.print_operand(src, &mut operands);
            }
            OpCode::Or(dst, src) => {
                op_code.push_str("or");
                self.print_operand(dst, &mut operands);
                operands.push_str(", ");
                self.print_operand(src, &mut operands);
            }
            OpCode::Xor(dst, src) => {
                op_code.push_str("xor");
                self.print_operand(dst, &mut operands);
                operands.push_str(", ");
                self.print_operand(src, &mut operands);
            }
            OpCode::Shl(dst, src) => {
                op_code.push_str("shl");
                self.print_operand(dst, &mut operands);
                operands.push_str(", ");
                self.print_operand(src, &mut operands);
            }
            OpCode::Shr(dst, src) => {
                op_code.push_str("shr");
                self.print_operand(dst, &mut operands);
                operands.push_str(", ");
                self.print_operand(src, &mut operands);
            }
        }
        let mut comment = String::new();
        if instr.descr.is_some() {
            comment.push_str("; ");
            comment.push_str(instr.descr.as_ref().unwrap().as_str());
        }
        out.push_str(&*format!("{:8}{:20}{}\n", op_code, operands, comment));
    }

    fn print_operand(&self, operand: &Operand, out: &mut String) {
        match operand {
            Operand::ImmediateValue(i) => {
                out.push_str(i.to_string().as_str());
            }
            Operand::ImmediateFloatValue(raw) => {
                out.push_str(f32::from_bits(*raw).to_string().as_str());
                out.push('f');
            }
            RegisterValue(r) => self.print_register(r, out),
            MemoryAtRegister(r) => {
                out.push_str("[");
                self.print_register(r, out);
                out.push_str("]");
            }
            MemoryAtRegisterOffset(r, o) => {
                out.push_str("[");
                self.print_register(r, out);
                if *o > 0 {
                    out.push_str(" + ");
                    out.push_str(o.to_string().as_str());
                } else {
                    out.push_str(" - ");
                    out.push_str((-o).to_string().as_str());
                }
                out.push_str("]");
            }
            Operand::MemoryAtRegisterOffsetScale(r1, o, r2, s) => {
                out.push_str("[");
                self.print_register(r1, out);
                out.push_str(" + ");
                out.push_str(o.to_string().as_str());
                out.push_str(" + ");
                self.print_register(r2, out);
                out.push_str(" * ");
                out.push_str(s.to_string().as_str());
                out.push_str("]");
            }
            DataAddress(lbl) => out.push_str(lbl),
            ThisFieldOffset(_) => panic!("Illegal 'this' operand"),
            Operand::BoolCondition(_) => panic!("Illegal comparison operand"),
        }
    }

    fn print_register(&self, reg: &Register, out: &mut String) {
        match reg {
            Register::GP(idx) => {
                out.push_str("r");
                out.push_str(idx.to_string().as_str());
            }
            Register::SB => out.push_str("sb"),
            Register::SP => out.push_str("sp"),
            Register::CR => out.push_str("cr"),
            Register::Virtual(idx) => {
                out.push('#');
                out.push_str(idx.to_string().as_str());
            }
            Register::DS => out.push_str("ds"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::bytecode::OpCode::{Add, Call, Mov, Push, Ret, Sub};
    use crate::bytecode::Operand::{ImmediateValue, MemoryAtRegisterOffset, RegisterValue};
    use crate::bytecode::Register::{SB, SP};
    use crate::bytecode::{Bytecode, Label};
    use crate::scope::Scope;

    #[test]
    fn test_expand() {
        let mut bytecode = Bytecode::new();
        bytecode.emit(
            Mov(
                MemoryAtRegisterOffset(SB, 4),
                MemoryAtRegisterOffset(SB, -4),
            ),
            None,
        );
        bytecode.emit(
            Mov(MemoryAtRegisterOffset(SB, 8), ImmediateValue(123)),
            None,
        );
        bytecode.emit(
            Add(MemoryAtRegisterOffset(SB, 4), MemoryAtRegisterOffset(SB, 8)),
            None,
        );
        bytecode.emit(Push(MemoryAtRegisterOffset(SB, 4)), None);
        bytecode.emit(Mov(RegisterValue(SB), RegisterValue(SP)), None);
        bytecode.emit(Call(Label::from("f")), None);
        bytecode.emit(Sub(RegisterValue(SB), ImmediateValue(4)), None);
        bytecode.emit(Ret, None);

        let mut scope = Scope::new();
        scope.push_scope();
        bytecode.expand(&mut scope);

        assert_eq!(bytecode.text.len(), 10);
    }
}
