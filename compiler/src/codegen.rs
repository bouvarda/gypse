//!
//! Code generation: from AST to Bytecode
//!
use crate::ast::{Block, Class, Expr, MatchPattern, Method, Op, Program, Statement};
use crate::bytecode::DataValue::{DataPtr, Skip, TextPtr};
use crate::bytecode::OpCode::{
    Add, And, Call, Cmp, Cvti2f, Div, DynamicCall, FAdd, FCmp, FDiv, FMul, FSub, Je, Jg, Jge, Jl,
    Jle, Jmp, Jne, Mov, Mul, Not, Or, Pop, Push, Ret, SetE, SetG, SetGe, SetL, SetLe, SetNe, Shl,
    Shr, Sub, Syscall, Xor,
};
use crate::bytecode::Operand::{
    BoolCondition, DataAddress, ImmediateFloatValue, ImmediateValue, MemoryAtRegister,
    MemoryAtRegisterOffset, MemoryAtRegisterOffsetScale, RegisterValue, ThisFieldOffset,
};
use crate::bytecode::Register;
use crate::bytecode::Register::{CR, SB, SP};
use crate::bytecode::{Bytecode, DataValue, Label, Operand};
use crate::inference::Inference;
use crate::naive_allocator::NaiveAllocator;
use crate::scope::Scope;
use crate::types::{
    ANY_TYPE, CTOR_NAME, FLOAT_TYPE, INT_TYPE, MAIN_CLASS, PRIMITIVE_TYPES, UNIT_TYPE, VTABLE_NAME,
};
use crate::utils::{sizeof_class_object, sizeof_class_object_field};

use common::{
    syscall_number, GP_REGISTER_COUNT, OBJECT_HEADER_SIZE, OPCODE_JE, OPCODE_JG, OPCODE_JGE,
    OPCODE_JL, OPCODE_JLE, OPCODE_JNE, SYSCALL_SYSTEM_MALLOC,
};

const THIS_VAR: &str = "this";
const SUPER_VAR: &str = "super";

pub struct CodeGen<'a> {
    program: &'a Program,
    entry_label: Label,
}

impl CodeGen<'_> {
    pub fn new(program: &Program) -> CodeGen {
        CodeGen {
            program,
            entry_label: Scope::gen_class_label(MAIN_CLASS, CTOR_NAME),
        }
    }

    fn gen_call(
        &mut self,
        class_type: &str,
        method_name: &str,
        this: Operand,
        args: Vec<Expr>,
        is_super_call: bool,
        bytecode: &mut Bytecode,
        scope: &mut Scope,
    ) -> Option<Operand> {
        // backup SB register
        bytecode.emit(Push(RegisterValue(SB)), Some("backup previous stack frame"));

        // push args in reverse order
        let mut args_size: i32 = 0;
        for arg_expr in args.iter().rev() {
            let arg_location = self.gen_expr(arg_expr, false, bytecode, scope).unwrap();
            bytecode.emit(Push(arg_location), Some("method argument"));
            args_size += 4;
        }
        // push "this" argument
        bytecode.emit(Push(this), Some("'this' pointer"));
        args_size += 4;

        // update SB to the stack top
        bytecode.emit(
            Mov(RegisterValue(SB), RegisterValue(SP)),
            Some("new stack frame"),
        );

        let class = self.program.find_class(class_type);
        let (_, method) = class
            .lookup_method(method_name, self.program)
            .expect(format!("Cannot find method {method_name}").as_str());
        if method.is_virtual {
            // virtual method call
            let virtual_index = 0; // TODO compute index
            let vtable_index = 4 + virtual_index * 4;
            let reg: Register = scope.alloc_virtual_register();
            bytecode.emit(Mov(RegisterValue(CR), MemoryAtRegister(SB)), None);
            bytecode.emit(
                Mov(RegisterValue(reg.clone()), MemoryAtRegister(CR)),
                Some("load vtable pointer"),
            );
            if is_super_call {
                bytecode.emit(
                    Mov(RegisterValue(CR), MemoryAtRegister(reg.clone())),
                    Some("load parent vtable pointer"),
                );
                bytecode.emit(
                    DynamicCall(MemoryAtRegisterOffsetScale(
                        CR,
                        vtable_index,
                        Register::DS,
                        1,
                    )),
                    Some("parent call virtual method"),
                );
            } else {
                bytecode.emit(
                    DynamicCall(MemoryAtRegisterOffset(reg, vtable_index)),
                    Some("call virtual method"),
                );
            }
        } else {
            // non virtual method call
            let label = Scope::gen_class_label(&class_type, method_name);
            bytecode.emit(Call(label), None);
        }

        if args_size > 0 {
            // unwind the stack
            bytecode.emit(
                Add(RegisterValue(SP), ImmediateValue(args_size)),
                Some("unwind stack frame"),
            );
        }
        // restore SB pointer
        bytecode.emit(Pop(RegisterValue(SB)), Some("restore previous SB"));

        // retrieve return value
        if method.return_type != UNIT_TYPE {
            let ret = scope.alloc_virtual_register();
            let result = RegisterValue(ret.clone());
            bytecode.emit(
                Mov(RegisterValue(ret), RegisterValue(CR)),
                Some("retrieve call result"),
            );
            Some(result)
        } else {
            None
        }
    }

    fn gen_local_var_decl(
        &mut self,
        var_name: &str,
        var_type: &str,
        bytecode: &mut Bytecode,
        scope: &mut Scope,
    ) -> Operand {
        let offset = scope.alloc_stack(4);
        let location = MemoryAtRegisterOffset(SB, offset);
        scope.declare_var(var_name, var_type, location.clone());
        bytecode.emit(
            Sub(RegisterValue(SP), ImmediateValue(4)),
            Some(&*format!("alloc variable {var_name}")),
        );
        location
    }

    fn gen_var_assign(&mut self, src: Operand, dst: Operand, bytecode: &mut Bytecode) {
        match (&src, &dst) {
            (_, ThisFieldOffset(_)) => {
                bytecode.emit(Mov(dst, src), Some("assign 'this' field"));
            }
            _ => {
                bytecode.emit(Mov(dst, src), Some("assign local variable"));
            }
        }
    }

    fn gen_block(
        &mut self,
        block: &Block,
        bytecode: &mut Bytecode,
        scope: &mut Scope,
    ) -> Option<Operand> {
        scope.push_scope();
        for stmt in &block.stmts {
            match stmt {
                Statement::VarDeclStmt(var_name, var_type, expr) => {
                    let src_location = self.gen_expr(expr, false, bytecode, scope).unwrap();
                    let dst_location = self.gen_local_var_decl(var_name, var_type, bytecode, scope);
                    self.gen_var_assign(src_location, dst_location.clone(), bytecode);
                }
                Statement::ExprStmt(expr) => {
                    self.gen_expr(expr, false, bytecode, scope);
                }
            }
        }
        let result = self.gen_expr(&block.return_expr, false, bytecode, scope);

        // unwind local variables
        let allocated_size = scope.allocated_size();
        if allocated_size > 0 {
            bytecode.emit(
                Add(RegisterValue(SP), ImmediateValue(allocated_size as i32)),
                Some("unwind local variables"),
            )
        }
        scope.pop_scope(true);
        result
    }

    fn gen_conditional_jump(
        &mut self,
        condition: &Expr,
        label_if_true: Label,
        invert: bool,
        bytecode: &mut Bytecode,
        scope: &mut Scope,
    ) {
        let cond_location = self.gen_expr(condition, true, bytecode, scope).unwrap();
        if invert {
            match cond_location {
                BoolCondition(OPCODE_JG) => bytecode.emit(Jle(label_if_true), None),
                BoolCondition(OPCODE_JGE) => bytecode.emit(Jl(label_if_true), None),
                BoolCondition(OPCODE_JL) => bytecode.emit(Jge(label_if_true), None),
                BoolCondition(OPCODE_JLE) => bytecode.emit(Jg(label_if_true), None),
                BoolCondition(OPCODE_JE) => bytecode.emit(Jne(label_if_true), None),
                BoolCondition(OPCODE_JNE) => bytecode.emit(Je(label_if_true), None),
                ImmediateValue(0) => bytecode.emit(Jmp(label_if_true), None),
                _ => panic!("Unsupported condition"),
            }
        } else {
            match cond_location {
                BoolCondition(OPCODE_JG) => bytecode.emit(Jg(label_if_true), None),
                BoolCondition(OPCODE_JGE) => bytecode.emit(Jge(label_if_true), None),
                BoolCondition(OPCODE_JL) => bytecode.emit(Jl(label_if_true), None),
                BoolCondition(OPCODE_JLE) => bytecode.emit(Jle(label_if_true), None),
                BoolCondition(OPCODE_JE) => bytecode.emit(Je(label_if_true), None),
                BoolCondition(OPCODE_JNE) => bytecode.emit(Jne(label_if_true), None),
                ImmediateValue(1) => bytecode.emit(Jmp(label_if_true), None),
                _ => panic!("Unsupported condition"),
            }
        }
    }

    fn gen_conditional_set(
        &mut self,
        cond_operand: &Operand,
        bytecode: &mut Bytecode,
        scope: &mut Scope,
    ) -> Operand {
        let reg = scope.alloc_virtual_register();
        let reg_operand = RegisterValue(reg.clone());
        match cond_operand {
            BoolCondition(OPCODE_JG) => bytecode.emit(SetG(reg_operand), None),
            BoolCondition(OPCODE_JGE) => bytecode.emit(SetGe(reg_operand), None),
            BoolCondition(OPCODE_JL) => bytecode.emit(SetL(reg_operand), None),
            BoolCondition(OPCODE_JLE) => bytecode.emit(SetLe(reg_operand), None),
            BoolCondition(OPCODE_JE) => bytecode.emit(SetE(reg_operand), None),
            BoolCondition(OPCODE_JNE) => bytecode.emit(SetNe(reg_operand), None),
            _ => panic!("Unsupported condition"),
        }
        RegisterValue(reg.clone())
    }

    fn gen_if_else(
        &mut self,
        cond: &Expr,
        then_expr: &Expr,
        else_expr: &Expr,
        bytecode: &mut Bytecode,
        scope: &mut Scope,
    ) -> Option<Operand> {
        let mut result_register: Option<Register> = None;
        let label_then = scope.gen_label("if_then");
        let label_end = scope.gen_label("if_end");

        // test if condition
        self.gen_conditional_jump(cond, label_then.clone(), false, bytecode, scope);

        // else block
        let else_location = self.gen_expr(else_expr, false, bytecode, scope);
        if else_location.is_some() {
            let reg = scope.alloc_virtual_register();
            result_register = Some(reg.clone());
            bytecode.emit(Mov(RegisterValue(reg), else_location.unwrap()), None);
        }
        bytecode.emit(Jmp(label_end.clone()), None);

        // then block
        bytecode.set_label_next(label_then);
        let then_location = self.gen_expr(then_expr, false, bytecode, scope);
        if then_location.is_some() {
            match &result_register {
                Some(reg) => {
                    bytecode.emit(
                        Mov(RegisterValue(reg.clone()), then_location.unwrap()),
                        None,
                    );
                }
                None => panic!("If-else is not consistent"),
            }
        }

        // if end
        bytecode.set_label_next(label_end);
        match result_register {
            Some(reg) => Some(RegisterValue(reg)),
            None => None,
        }
    }

    fn gen_if_without_else(
        &mut self,
        cond: &Expr,
        then_expr: &Expr,
        bytecode: &mut Bytecode,
        scope: &mut Scope,
    ) {
        let label_end = scope.gen_label("if_end");

        // test inverted if condition
        self.gen_conditional_jump(cond, label_end.clone(), true, bytecode, scope);

        // then block
        self.gen_expr(then_expr, false, bytecode, scope);
        bytecode.set_label_next(label_end);
    }

    fn gen_logical_and_or(
        &mut self,
        left_expr: &Expr,
        right_expr: &Expr,
        is_and: bool,
        is_condition: bool,
        bytecode: &mut Bytecode,
        scope: &mut Scope,
    ) -> Operand {
        let reg = scope.alloc_virtual_register();
        let label_end = if is_and {
            scope.gen_label("and_end")
        } else {
            scope.gen_label("or_end")
        };
        let left_location = self.gen_expr(left_expr, false, bytecode, scope).unwrap();
        bytecode.emit(Mov(RegisterValue(reg.clone()), left_location), None);
        bytecode.emit(Cmp(RegisterValue(reg.clone()), ImmediateValue(0)), None);
        if is_and {
            bytecode.emit(Je(label_end.clone()), None);
        } else {
            bytecode.emit(Jne(label_end.clone()), None);
        }
        let right_location = self.gen_expr(right_expr, false, bytecode, scope).unwrap();
        bytecode.emit(Mov(RegisterValue(reg.clone()), right_location), None);
        bytecode.emit(Cmp(RegisterValue(reg.clone()), ImmediateValue(0)), None);
        bytecode.set_label_next(label_end);
        if is_condition {
            BoolCondition(OPCODE_JNE) // truthy
        } else {
            RegisterValue(reg)
        }
    }

    fn gen_arithmetic_operator(
        &mut self,
        operator: &Op,
        left_expr: &Expr,
        right_expr: &Expr,
        left_location: Operand,
        right_location: Operand,
        bytecode: &mut Bytecode,
        scope: &mut Scope,
    ) -> Option<Operand> {
        let result_register = scope.alloc_virtual_register();

        let inference = Inference::new(self.program, scope);
        let left_type = inference.infer_static_type(&left_expr);
        let right_type = inference.infer_static_type(&right_expr);

        let op_code = match (left_type.as_str(), right_type.as_str()) {
            (INT_TYPE, INT_TYPE) => {
                match operator {
                    Op::Add => Add(RegisterValue(result_register.clone()), right_location), // result += right
                    Op::Sub => Sub(RegisterValue(result_register.clone()), right_location), // result -= right
                    Op::Mul => Mul(RegisterValue(result_register.clone()), right_location), // result *= right
                    Op::Div => Div(RegisterValue(result_register.clone()), right_location), // result /= right
                    _ => panic!("Unsupported arithmetic integer operator {:?}", operator),
                }
            }
            (FLOAT_TYPE, FLOAT_TYPE) => match operator {
                Op::Add => FAdd(RegisterValue(result_register.clone()), right_location),
                Op::Sub => FSub(RegisterValue(result_register.clone()), right_location),
                Op::Mul => FMul(RegisterValue(result_register.clone()), right_location),
                Op::Div => FDiv(RegisterValue(result_register.clone()), right_location),
                _ => panic!("Unsupported arithmetic float operator {:?}", operator),
            },
            (FLOAT_TYPE, INT_TYPE) | (INT_TYPE, FLOAT_TYPE) => {
                let integer_op = if left_type.as_str() == INT_TYPE {
                    &left_location
                } else {
                    &right_location
                };
                match integer_op {
                    ImmediateValue(i) => {
                        let casted_operand = ImmediateFloatValue(f32::to_bits(*i as f32));
                        match operator {
                            Op::Add => FAdd(RegisterValue(result_register.clone()), casted_operand),
                            Op::Sub => FSub(RegisterValue(result_register.clone()), casted_operand),
                            Op::Mul => FMul(RegisterValue(result_register.clone()), casted_operand),
                            Op::Div => FDiv(RegisterValue(result_register.clone()), casted_operand),
                            _ => panic!("Unsupported arithmetic float operator {:?}", operator),
                        }
                    }
                    _ => {
                        let temp_operand = RegisterValue(scope.alloc_virtual_register());
                        bytecode.emit(Mov(temp_operand.clone(), integer_op.clone()), None);
                        bytecode.emit(Cvti2f(temp_operand.clone()), Some("convert to float"));
                        match operator {
                            Op::Add => {
                                FAdd(RegisterValue(result_register.clone()), temp_operand.clone())
                            }
                            Op::Sub => {
                                FSub(RegisterValue(result_register.clone()), temp_operand.clone())
                            }
                            Op::Mul => {
                                FMul(RegisterValue(result_register.clone()), temp_operand.clone())
                            }
                            Op::Div => {
                                FDiv(RegisterValue(result_register.clone()), temp_operand.clone())
                            }
                            _ => panic!("Unsupported arithmetic float operator {:?}", operator),
                        }
                    }
                }
            }
            _ => panic!(
                "Incompatible types for arithmetic operator between {:?} and {:?}",
                left_type, right_type
            ),
        };
        // result = left
        bytecode.emit(
            Mov(RegisterValue(result_register.clone()), left_location),
            None,
        );
        bytecode.emit(op_code, None);
        Some(RegisterValue(result_register))
    }

    fn gen_bitwise_operator(
        &mut self,
        operator: &Op,
        left_location: Operand,
        right_location: Operand,
        bytecode: &mut Bytecode,
        scope: &mut Scope,
    ) -> Option<Operand> {
        let result_register = scope.alloc_virtual_register();
        let op_code = match operator {
            Op::BitwiseAnd => And(RegisterValue(result_register.clone()), right_location), // result &= right
            Op::BitwiseOr => Or(RegisterValue(result_register.clone()), right_location), // result |= right
            Op::BitwiseXOr => Xor(RegisterValue(result_register.clone()), right_location), // result ^= right
            Op::LeftShift => Shl(RegisterValue(result_register.clone()), right_location), // result <<= right
            Op::RightShift => Shr(RegisterValue(result_register.clone()), right_location), // result >>= right
            _ => panic!("Unsupported bitwise operator {:?}", operator),
        };
        // result = left
        bytecode.emit(
            Mov(RegisterValue(result_register.clone()), left_location),
            None,
        );
        bytecode.emit(op_code, None);
        Some(RegisterValue(result_register))
    }

    fn gen_expr(
        &mut self,
        expr: &Expr,
        is_condition: bool,
        bytecode: &mut Bytecode,
        scope: &mut Scope,
    ) -> Option<Operand> {
        match expr {
            Expr::True => Some(ImmediateValue(1)),
            Expr::False | Expr::Null => Some(ImmediateValue(0)),
            Expr::Unit => None,
            Expr::IntegerLiteral(i) => Some(ImmediateValue(*i)),
            Expr::FloatLiteral(f) => Some(ImmediateFloatValue(f32::to_bits(*f))),
            Expr::StringLiteral(string_value) => {
                let label = scope.gen_label("str");
                Some(bytecode.emit_data(label, DataValue::Ascii(string_value.clone())))
            }
            Expr::New(class_type, args) => {
                // alloc memory on heap using syscall
                let size = sizeof_class_object(class_type, self.program);
                bytecode.emit(Push(ImmediateValue(size)), Some("push object size"));
                bytecode.emit(
                    Syscall(SYSCALL_SYSTEM_MALLOC),
                    Some("malloc object on heap"),
                );
                bytecode.emit(
                    Mov(
                        MemoryAtRegister(CR),
                        DataAddress(Scope::gen_class_label(class_type, VTABLE_NAME)),
                    ),
                    Some("write object vtable pointer"),
                );
                bytecode.emit(Add(RegisterValue(SP), ImmediateValue(4)), None);

                // store the new object address in 'this'
                let reg = scope.alloc_virtual_register();
                bytecode.emit(Mov(RegisterValue(reg.clone()), RegisterValue(CR)), None);

                self.gen_call(
                    class_type,
                    CTOR_NAME,
                    RegisterValue(reg),
                    args.to_vec(),
                    false,
                    bytecode,
                    scope,
                )
            }
            Expr::MethodCall(receiver, method_name, args) => {
                let location = self
                    .gen_expr(receiver, false, bytecode, scope)
                    .expect("Cannot call method on NULL");
                let inference = Inference::new(self.program, scope);
                let receiver_type = inference.infer_static_type(receiver);
                let receiver_class = self.program.find_class(receiver_type.as_str());
                let (base_class, _) = receiver_class
                    .lookup_method(method_name, self.program)
                    .expect(format!("Cannot find method {}", method_name).as_str());
                let is_super_call = match receiver.as_ref() {
                    Expr::Super => true,
                    _ => false,
                };
                self.gen_call(
                    base_class.class_type.as_str(),
                    method_name,
                    location,
                    args.to_vec(),
                    is_super_call,
                    bytecode,
                    scope,
                )
            }
            Expr::VarRef(var_name) => match scope.lookup(var_name) {
                Some(var) => Some(var.location.clone()),
                None => panic!("Cannot find variable {var_name}"),
            },
            Expr::VarAccess(receiver, var_name) => {
                let receiver_location = self
                    .gen_expr(receiver, false, bytecode, scope)
                    .expect("Cannot get attribute on NULL");
                let inference = Inference::new(self.program, scope);
                let receiver_type = inference.infer_static_type(receiver);
                let receiver_class = self.program.find_class(receiver_type.as_str());
                match scope.lookup_layout_index(receiver_class, var_name) {
                    Some(index) => {
                        let reg = scope.alloc_virtual_register();
                        bytecode.emit(
                            Mov(RegisterValue(reg.clone()), receiver_location),
                            Some("load receiver"),
                        );
                        Some(MemoryAtRegisterOffset(reg, index))
                    }
                    None => panic!("Cannot find attribute {var_name}"),
                }
            }
            Expr::This | Expr::Super => match scope.lookup(THIS_VAR) {
                Some(var) => Some(var.location.clone()),
                None => panic!("Cannot find 'this' variable"),
            },
            Expr::BlockRef(block) => self.gen_block(block, bytecode, scope),
            Expr::BinaryOp(dst_expr, Op::Assign, src_expr) => {
                let src_location = self.gen_expr(src_expr, false, bytecode, scope).unwrap();
                let dst_location = self.gen_expr(dst_expr, false, bytecode, scope).unwrap();
                self.gen_var_assign(src_location, dst_location.clone(), bytecode);
                Some(dst_location)
            }
            // a && b => eval a then b if a is truthy, if a is falsy skip b eval
            Expr::BinaryOp(left_expr, Op::And, right_expr) => Some(self.gen_logical_and_or(
                left_expr,
                right_expr,
                true,
                is_condition,
                bytecode,
                scope,
            )),
            // a || b => eval a then b if a is falsy, if a is truthy skip b eval
            Expr::BinaryOp(left_expr, Op::Or, right_expr) => Some(self.gen_logical_and_or(
                left_expr,
                right_expr,
                false,
                is_condition,
                bytecode,
                scope,
            )),
            Expr::BinaryOp(left_expr, operator, right_expr) => {
                let left_location = self.gen_expr(left_expr, false, bytecode, scope).unwrap();
                let right_location = self.gen_expr(right_expr, false, bytecode, scope).unwrap();

                if operator.is_arithmetic() {
                    self.gen_arithmetic_operator(
                        operator,
                        &left_expr,
                        &right_expr,
                        left_location,
                        right_location,
                        bytecode,
                        scope,
                    )
                } else if operator.is_bitwise() {
                    self.gen_bitwise_operator(
                        operator,
                        left_location,
                        right_location,
                        bytecode,
                        scope,
                    )
                } else {
                    let inference = Inference::new(self.program, scope);
                    let left_type = inference.infer_static_type(&left_expr);
                    let right_type = inference.infer_static_type(&right_expr);
                    match (left_type.as_str(), right_type.as_str()) {
                        (FLOAT_TYPE, FLOAT_TYPE) => {
                            bytecode.emit(FCmp(left_location, right_location), None);
                        }
                        (_, _) => {
                            bytecode.emit(Cmp(left_location, right_location), None);
                        } // _ => panic!("Incompatible comparison {:?} between {:?} and {:?}", operator, left_type, right_type)
                    };
                    let op_code = match operator {
                        Op::Gt => OPCODE_JG,
                        Op::Ge => OPCODE_JGE,
                        Op::Lt => OPCODE_JL,
                        Op::Le => OPCODE_JLE,
                        Op::Eq => OPCODE_JE,
                        Op::Ne => OPCODE_JNE,
                        _ => panic!("Unsupported comparison operator {:?}", operator),
                    };
                    if is_condition {
                        Some(BoolCondition(op_code)) // used later in if conditions
                    } else {
                        Some(self.gen_conditional_set(&BoolCondition(op_code), bytecode, scope))
                    }
                }
            }
            Expr::If(cond, then_expr) => {
                self.gen_if_without_else(cond, then_expr, bytecode, scope);
                None
            }
            Expr::IfElse(cond, then_expr, else_expr) => {
                self.gen_if_else(cond, then_expr, else_expr, bytecode, scope)
            }
            Expr::While(cond, body) => {
                let label_cond = scope.gen_label("while_cond");
                let label_body = scope.gen_label("while_body");
                let label_end = scope.gen_label("while_end");

                // while condition test
                bytecode.set_label_next(label_cond.clone());
                self.gen_conditional_jump(cond, label_body.clone(), false, bytecode, scope);

                // condition is false, jump out of loop
                bytecode.emit(Jmp(label_end.clone()), None);

                // while body
                bytecode.set_label_next(label_body);
                self.gen_expr(body, false, bytecode, scope);
                bytecode.emit(Jmp(label_cond), None);

                // end while
                bytecode.set_label_next(label_end);
                None
            }
            Expr::Not(e) | Expr::Minus(e) => {
                let location = self.gen_expr(e, false, bytecode, scope).unwrap();
                match expr {
                    Expr::Not(_) => {
                        bytecode.emit(Not(location.clone()), None);
                        Some(location)
                    }
                    Expr::Minus(e) => {
                        let result_reg = scope.alloc_virtual_register();
                        let inference = Inference::new(self.program, scope);
                        let t = inference.infer_static_type(e.as_ref());
                        // convert as (0 - x) to negate
                        if t == FLOAT_TYPE {
                            bytecode.emit(
                                Mov(
                                    RegisterValue(result_reg.clone()),
                                    ImmediateFloatValue(f32::to_bits(0.0)),
                                ),
                                None,
                            );
                            bytecode.emit(
                                FSub(RegisterValue(result_reg.clone()), location.clone()),
                                Some("negate float value"),
                            );
                        } else {
                            bytecode.emit(
                                Mov(RegisterValue(result_reg.clone()), ImmediateValue(0)),
                                None,
                            );
                            bytecode.emit(
                                Sub(RegisterValue(result_reg.clone()), location.clone()),
                                Some("negate integer value"),
                            )
                        }
                        Some(RegisterValue(result_reg))
                    }
                    _ => panic!(),
                }
            }
            Expr::Syscall(syscall_name) => {
                bytecode.emit(
                    Syscall(syscall_number(syscall_name)),
                    Some(&*format!("system call {syscall_name}")),
                );
                Some(RegisterValue(CR))
            }
            Expr::Match(cond_expr, cases) => {
                let cond_operand = self
                    .gen_expr(cond_expr, false, bytecode, scope)
                    .expect("Match expression must return a value");
                let label_match_end = scope.gen_label("match_end");
                let mut case_labels: Vec<Label> = vec![];

                // copy the condition operand in a virtual register to be able to dereference it
                let cond_reg = scope.alloc_virtual_register();
                bytecode.emit(
                    Mov(RegisterValue(cond_reg.clone()), cond_operand.clone()),
                    None,
                );

                // case pattern checks
                for (idx, case) in cases.iter().enumerate() {
                    let case_label = scope.gen_label(format!("case{idx}").as_str());
                    case_labels.push(case_label.clone());
                    match &case.pattern {
                        MatchPattern::NullPattern => {
                            bytecode.emit(
                                Cmp(cond_operand.clone(), ImmediateValue(0)),
                                Some("check match null"),
                            );
                            bytecode.emit(Je(case_label), None);
                        }
                        MatchPattern::TypePattern(_, class_type) if class_type == ANY_TYPE => {
                            bytecode.emit(Jmp(case_label), Some("default case"));
                        }
                        // TODO handle String type matching
                        MatchPattern::TypePattern(_, class_type)
                            if PRIMITIVE_TYPES.contains(&class_type.as_str()) =>
                        {
                            panic!("Cannot match on primitive type {class_type}");
                        }
                        MatchPattern::TypePattern(_, class_type) => {
                            bytecode.emit(
                                Cmp(
                                    MemoryAtRegister(cond_reg.clone()),
                                    DataAddress(Scope::gen_class_label(
                                        class_type.as_str(),
                                        VTABLE_NAME,
                                    )),
                                ),
                                Some(format!("check match {class_type}").as_str()),
                            );
                            bytecode.emit(Je(case_label), None);
                        }
                    }
                }
                // TODO no match exception
                bytecode.emit(Syscall(syscall_number("System::exit")), None);

                // case blocks
                let mut result_register: Option<Register> = None;
                for (idx, case) in cases.iter().enumerate() {
                    let label = case_labels.get(idx).unwrap();
                    bytecode.set_label_next(label.clone());
                    match &case.pattern {
                        MatchPattern::TypePattern(var_name, class_type) => {
                            scope.push_scope();
                            scope.declare_var(var_name, class_type, cond_operand.clone())
                        }
                        _ => {}
                    }
                    let case_location = self.gen_block(&case.block, bytecode, scope);
                    match &case.pattern {
                        MatchPattern::TypePattern(_, _) => {
                            scope.pop_scope(true);
                        }
                        _ => {}
                    }
                    // case return value
                    if case_location.is_some() {
                        if result_register.is_none() {
                            result_register = Some(scope.alloc_virtual_register());
                        }
                        bytecode.emit(
                            Mov(
                                RegisterValue(result_register.clone().unwrap()),
                                case_location.unwrap(),
                            ),
                            None,
                        );
                    }
                    bytecode.emit(Jmp(label_match_end.clone()), None);
                }
                bytecode.set_label_next(label_match_end);
                result_register.map(|r| RegisterValue(r))
            }
        }
    }

    fn gen_method(
        &mut self,
        super_class_type: &str,
        class_type: &str,
        method: &Method,
        scope: &mut Scope,
    ) -> Bytecode {
        // register method args scope and the "this" argument
        scope.push_scope();
        scope.declare_var(THIS_VAR, class_type, MemoryAtRegisterOffset(SB, 0));
        scope.declare_var(SUPER_VAR, super_class_type, MemoryAtRegisterOffset(SB, 0));

        // declare the arguments
        let mut offset: i16 = 4; // first argument is always 'this'
        for arg in &method.args {
            scope.declare_var(
                arg.name.as_str(),
                &arg.arg_type,
                MemoryAtRegisterOffset(SB, offset),
            );
            offset += 4; // TODO size
        }

        let mut bytecode = Bytecode::new();

        // create routine label
        let routine_label = Scope::gen_class_label(class_type, method.name.as_str());
        let is_entry_routine = routine_label == self.entry_label;
        bytecode.set_label_next(routine_label);

        // special case for the main object, it is allocated statically on data section
        if is_entry_routine {
            let size = sizeof_class_object(&class_type, self.program);
            let object = bytecode.emit_data(
                Label::from("main_obj"),
                DataPtr(Scope::gen_class_label(MAIN_CLASS, VTABLE_NAME)),
            );
            let remaining = size - OBJECT_HEADER_SIZE as i32;
            if remaining > 0 {
                bytecode.emit_data(Label::from(""), Skip(remaining as u32));
            }
            bytecode.emit(
                Mov(MemoryAtRegister(SB), object),
                Some("store 'this' main object at [sb]"),
            );
            bytecode.emit(
                Sub(RegisterValue(SP), ImmediateValue(4)),
                Some("reserved for ip"),
            );
        }
        let return_location = self.gen_expr(&method.body, false, &mut bytecode, scope);
        if is_entry_routine {
            // exit syscall the main entry routine, not a regular method return
            bytecode.emit(Syscall(0), None);
        } else {
            // store return value in CR and emit ret
            if method.return_type != UNIT_TYPE && return_location.is_some() {
                match return_location.unwrap() {
                    RegisterValue(CR) => {} // the value is already setup in CR
                    operand => bytecode.emit(
                        Mov(RegisterValue(CR), operand),
                        Some("store the return value"),
                    ),
                }
            }
            bytecode.emit(Ret, None);
        }
        bytecode.flush_instr();
        bytecode.expand(scope);
        let start_spill_addr = scope.start_spill_addr();
        scope.pop_scope(false);

        // allocate physical registers in the expanded bytecode, the spill location is at the end of the current stack frame
        let mut allocator = NaiveAllocator::new(GP_REGISTER_COUNT, start_spill_addr);
        allocator.allocate(bytecode)
        // bytecode
    }

    fn gen_class(&mut self, class: &Class, bytecode: &mut Bytecode, scope: &mut Scope) {
        // register class scope
        scope.set_class(class.class_type.as_str());
        scope.push_scope();
        let mut offset: u16 = OBJECT_HEADER_SIZE; // skip object header
        for field in &class.fields {
            let field_size = sizeof_class_object_field(&field.field_type) as u16;
            scope.declare_var(
                field.name.as_str(),
                &field.field_type,
                ThisFieldOffset(offset),
            );
            offset += field_size;
        }
        let super_class_name = match &class.extends {
            Some(parent) => parent.clone(),
            None => class.class_type.clone(),
        };
        // generate class methods
        for method in &class.methods {
            bytecode.append(self.gen_method(
                super_class_name.as_str(),
                &class.class_type,
                method,
                scope,
            ));
        }
        scope.pop_scope(false);

        // generate class vtable
        self.gen_class_vtable(class, bytecode, scope);
    }

    fn gen_class_vtable(&mut self, class: &Class, bytecode: &mut Bytecode, scope: &mut Scope) {
        // vtable parent pointer (4 bytes), its address is important for pattern matching
        if class.class_type == ANY_TYPE {
            return; // vtable not needed for Any (abstract class without virtual methods)
        }
        let parent_pointer: DataValue = if class.extends.as_ref().unwrap() == ANY_TYPE {
            DataValue::Skip(4) // no parent (!= from Any), NULL pointer
        } else {
            DataValue::DataPtr(Scope::gen_class_label(
                class.extends.as_ref().unwrap(),
                VTABLE_NAME,
            ))
        };
        bytecode.emit_data(
            Scope::gen_class_label(class.class_type.as_str(), VTABLE_NAME),
            parent_pointer,
        );

        let mut vtable_indexes: Vec<(String, String)> = vec![];
        self.rec_build_vtable_indexes(class, &mut vtable_indexes);

        // emit one text pointer (4 bytes) for each virtual method, in the class hierarchy declaration order, this will be used for polymorphic calls
        for (vclass, vmethod) in vtable_indexes {
            let virtual_label = Scope::gen_class_label(vclass.as_str(), vmethod.as_str());
            bytecode.emit_data_without_label(TextPtr(virtual_label));
        }
    }

    fn rec_build_vtable_indexes(
        &mut self,
        class: &Class,
        vtable_indexes: &mut Vec<(String, String)>,
    ) {
        match &class.extends {
            Some(parent_name) if parent_name != ANY_TYPE => {
                let parent_class = self.program.find_class(parent_name);
                self.rec_build_vtable_indexes(parent_class, vtable_indexes);
            }
            _ => {}
        }
        for method in &class.methods {
            let index: Option<usize> = vtable_indexes
                .iter()
                .enumerate()
                .find(|(_, (_, m))| *m == method.name)
                .map(|(i, _)| i);
            match index {
                Some(i) => {
                    // update the existing index with the new class method
                    vtable_indexes.remove(i);
                    vtable_indexes.insert(i, (class.class_type.clone(), method.name.clone()));
                }
                None if method.is_virtual => {
                    // new virtual method in the table, takes next index
                    vtable_indexes.push((class.class_type.clone(), method.name.clone()))
                }
                None => {}
            }
        }
    }

    pub fn gen_program(&mut self) -> (Bytecode, Label) {
        let mut scope = Scope::new();
        let mut bytecode = Bytecode::new();

        let classes = &self.program.classes;
        for class in classes {
            self.gen_class(class, &mut bytecode, &mut scope);
        }
        (bytecode, self.entry_label.clone())
    }
}
