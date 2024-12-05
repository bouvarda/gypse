//!
//! Apply generic transformer to update Bytecode
//!
use crate::bytecode::OpCode::{
    Add, And, Call, Cmp, Cvti2f, Div, DynamicCall, FAdd, FCmp, FDiv, FMul, FSub, Mov, Mul, Nop,
    Not, Or, Pop, Push, SetE, SetG, SetGe, SetL, SetLe, SetNe, Shl, Shr, Sub, Xor,
};
use crate::bytecode::{Bytecode, Label, OpCode, Operand};

pub trait BytecodeTransformer {
    fn transform_read(&mut self, op: Operand, bytecode: &mut Bytecode) -> Operand;
    fn transform_write(&mut self, op: Operand, bytecode: &mut Bytecode) -> Operand;
    fn transform_pre_call(&mut self, bytecode: &mut Bytecode);
    fn transform_post_call(&mut self, bytecode: &mut Bytecode);
    fn transform_return(&mut self, label: Option<Label>, bytecode: &mut Bytecode);
}

pub fn bytecode_transform<T>(bytecode: Bytecode, transformer: &mut T) -> Bytecode
where
    T: BytecodeTransformer,
{
    let mut tranformed = Bytecode::new();
    tranformed.data = bytecode.data;
    for mut instr in bytecode.text {
        let mut saved_label: Option<Label> = None;
        if instr.label.is_some() {
            //tranformed.set_label_next(instr.label.unwrap());
            saved_label = instr.label.take();
        }
        let new_instr = match instr.op_code {
            Add(dst, src) => {
                let src2 = transformer.transform_read(src, &mut tranformed);
                let dst2 = transformer.transform_read(dst, &mut tranformed);
                let dst3 = transformer.transform_write(dst2, &mut tranformed);
                Add(dst3, src2)
            }
            Sub(dst, src) => {
                let src2 = transformer.transform_read(src, &mut tranformed);
                let dst2 = transformer.transform_read(dst, &mut tranformed);
                let dst3 = transformer.transform_write(dst2, &mut tranformed);
                Sub(dst3, src2)
            }
            Mul(dst, src) => {
                let src2 = transformer.transform_read(src, &mut tranformed);
                let dst2 = transformer.transform_read(dst, &mut tranformed);
                let dst3 = transformer.transform_write(dst2, &mut tranformed);
                Mul(dst3, src2)
            }
            Div(dst, src) => {
                let src2 = transformer.transform_read(src, &mut tranformed);
                let dst2 = transformer.transform_read(dst, &mut tranformed);
                let dst3 = transformer.transform_write(dst2, &mut tranformed);
                Div(dst3, src2)
            }
            Cmp(op1, op2) => {
                let op1_2 = transformer.transform_read(op1, &mut tranformed);
                let op2_2 = transformer.transform_read(op2, &mut tranformed);
                Cmp(op1_2, op2_2)
            }

            FAdd(dst, src) => {
                let src2 = transformer.transform_read(src, &mut tranformed);
                let dst2 = transformer.transform_read(dst, &mut tranformed);
                let dst3 = transformer.transform_write(dst2, &mut tranformed);
                FAdd(dst3, src2)
            }
            FSub(dst, src) => {
                let src2 = transformer.transform_read(src, &mut tranformed);
                let dst2 = transformer.transform_read(dst, &mut tranformed);
                let dst3 = transformer.transform_write(dst2, &mut tranformed);
                FSub(dst3, src2)
            }
            FMul(dst, src) => {
                let src2 = transformer.transform_read(src, &mut tranformed);
                let dst2 = transformer.transform_read(dst, &mut tranformed);
                let dst3 = transformer.transform_write(dst2, &mut tranformed);
                FMul(dst3, src2)
            }
            FDiv(dst, src) => {
                let src2 = transformer.transform_read(src, &mut tranformed);
                let dst2 = transformer.transform_read(dst, &mut tranformed);
                let dst3 = transformer.transform_write(dst2, &mut tranformed);
                FDiv(dst3, src2)
            }
            FCmp(op1, op2) => {
                let op1_2 = transformer.transform_read(op1, &mut tranformed);
                let op2_2 = transformer.transform_read(op2, &mut tranformed);
                FCmp(op1_2, op2_2)
            }

            And(dst, src) => {
                let src2 = transformer.transform_read(src, &mut tranformed);
                let dst2 = transformer.transform_read(dst, &mut tranformed);
                let dst3 = transformer.transform_write(dst2, &mut tranformed);
                And(dst3, src2)
            }

            Or(dst, src) => {
                let src2 = transformer.transform_read(src, &mut tranformed);
                let dst2 = transformer.transform_read(dst, &mut tranformed);
                let dst3 = transformer.transform_write(dst2, &mut tranformed);
                Or(dst3, src2)
            }

            Xor(dst, src) => {
                let src2 = transformer.transform_read(src, &mut tranformed);
                let dst2 = transformer.transform_read(dst, &mut tranformed);
                let dst3 = transformer.transform_write(dst2, &mut tranformed);
                Xor(dst3, src2)
            }

            Shl(dst, src) => {
                let src2 = transformer.transform_read(src, &mut tranformed);
                let dst2 = transformer.transform_read(dst, &mut tranformed);
                let dst3 = transformer.transform_write(dst2, &mut tranformed);
                Shl(dst3, src2)
            }

            Shr(dst, src) => {
                let src2 = transformer.transform_read(src, &mut tranformed);
                let dst2 = transformer.transform_read(dst, &mut tranformed);
                let dst3 = transformer.transform_write(dst2, &mut tranformed);
                Shr(dst3, src2)
            }

            Mov(dst, src) => {
                let src2 = transformer.transform_read(src, &mut tranformed);
                let dst2 = transformer.transform_write(dst, &mut tranformed);
                Mov(dst2, src2)
            }

            Push(op) => Push(transformer.transform_read(op, &mut tranformed)),
            Not(op) => Not(transformer.transform_read(op, &mut tranformed)),

            SetG(op) => SetG(transformer.transform_write(op, &mut tranformed)),
            SetGe(op) => SetGe(transformer.transform_write(op, &mut tranformed)),
            SetL(op) => SetL(transformer.transform_write(op, &mut tranformed)),
            SetLe(op) => SetLe(transformer.transform_write(op, &mut tranformed)),
            SetE(op) => SetE(transformer.transform_write(op, &mut tranformed)),
            SetNe(op) => SetNe(transformer.transform_write(op, &mut tranformed)),

            Pop(op) => {
                let op2 = transformer.transform_write(op, &mut tranformed);
                Pop(op2)
            }

            Cvti2f(op) => {
                let op2 = transformer.transform_read(op, &mut tranformed);
                let op3 = transformer.transform_write(op2, &mut tranformed);
                Cvti2f(op3)
            }

            Call(name) => {
                transformer.transform_pre_call(&mut tranformed);
                tranformed.emit(Call(name), instr.descr.take().as_deref());
                transformer.transform_post_call(&mut tranformed);
                Nop
            }

            DynamicCall(op) => {
                let new_op = transformer.transform_read(op, &mut tranformed);
                transformer.transform_pre_call(&mut tranformed);
                tranformed.emit(DynamicCall(new_op), instr.descr.take().as_deref());
                transformer.transform_post_call(&mut tranformed);
                Nop
            }

            OpCode::Ret => {
                // pass the ret label to the transformer, it will handle it
                let ret_label = saved_label.take();
                transformer.transform_return(ret_label, &mut tranformed);
                OpCode::Ret
            }

            _ => instr.op_code,
        };

        if saved_label.is_some() {
            tranformed.set_label_next(saved_label.unwrap());
            if new_instr == Nop {
                tranformed.emit(Nop, Some("restore label"));
            }
        }
        if new_instr != Nop {
            tranformed.emit(new_instr, instr.descr.take().as_deref());
        }
    }
    tranformed
}
