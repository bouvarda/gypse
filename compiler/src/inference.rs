//!
//! Type inference helpers
//!
use crate::ast::Expr::{BinaryOp, MethodCall, Super, This, VarAccess, VarRef};
use crate::ast::{Class, Expr, Program};
use crate::scope::Scope;
use crate::types::{BOOL_TYPE, FLOAT_TYPE, INT_TYPE, NULL_TYPE, STRING_TYPE, UNIT_TYPE};

pub struct Inference<'a> {
    program: &'a Program,
    scope: &'a Scope,
}

impl Inference<'_> {
    pub fn new<'a>(program: &'a Program, scope: &'a Scope) -> Inference<'a> {
        Inference { program, scope }
    }

    fn current_class(&self) -> &Class {
        let class_name = self.scope.current_class();
        self.program.find_class(class_name.as_str())
    }

    pub fn infer_static_type(&self, expr: &Expr) -> String {
        match expr {
            Expr::True | Expr::False => String::from(BOOL_TYPE),
            Expr::Unit => String::from(UNIT_TYPE),
            Expr::Null => String::from(NULL_TYPE),
            This => self.scope.current_class().clone(),
            Super => self.current_class().super_class_name_or_this().clone(),
            Expr::New(class_type, _) => class_type.clone(),
            VarAccess(receiver, var_name) => {
                let receiver_type = self.infer_static_type(receiver.as_ref());
                let receiver_class = self.program.find_class(receiver_type.as_str());
                receiver_class.find_field(var_name).field_type.clone()
            }
            VarRef(var_name) => self.scope.lookup(var_name).unwrap().var_type.clone(),
            Expr::StringLiteral(_) => String::from(STRING_TYPE),
            Expr::IntegerLiteral(_) => String::from(INT_TYPE),
            Expr::FloatLiteral(_) => String::from(FLOAT_TYPE),
            Expr::Not(_) => String::from(BOOL_TYPE),
            Expr::Minus(e) => {
                let t = self.infer_static_type(e);
                if t == FLOAT_TYPE {
                    String::from(FLOAT_TYPE)
                } else {
                    String::from(INT_TYPE)
                }
            }
            Expr::If(_, _) => String::from(UNIT_TYPE),
            Expr::IfElse(_, then, _) => self.infer_static_type(then),
            Expr::While(_, _) => String::from(UNIT_TYPE),
            Expr::BlockRef(block) => self.infer_static_type(&block.return_expr),
            BinaryOp(left, op, right) => {
                if op.is_arithmetic() {
                    let left_type = self.infer_static_type(left);
                    let right_type = self.infer_static_type(right);
                    if left_type == FLOAT_TYPE || right_type == FLOAT_TYPE {
                        String::from(FLOAT_TYPE)
                    } else {
                        String::from(INT_TYPE)
                    }
                } else if op.is_bitwise() {
                    String::from(INT_TYPE)
                } else {
                    String::from(BOOL_TYPE)
                }
            }
            MethodCall(receiver, method_name, _) => {
                let receiver_type = self.infer_static_type(receiver.as_ref());
                let receiver_class = self.program.find_class(receiver_type.as_str());
                let (_, method) = receiver_class
                    .lookup_method(method_name, self.program)
                    .expect("Cannot find method");
                method.return_type.clone()
            }
            Expr::Match(_, cases) => {
                self.infer_static_type(&cases.first().unwrap().block.return_expr)
            }
            Expr::Syscall(_) => String::from(INT_TYPE),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Class, Expr, Field, Method, Program};
    use crate::bytecode::Operand::ImmediateValue;
    use crate::inference::Inference;
    use crate::parser::Parser;
    use crate::scope::Scope;

    #[test]
    fn test_infer_static() {
        let program = Program {
            imports: vec![],
            classes: vec![Class {
                class_type: String::from("A"),
                ctor_args: vec![],
                extends: Some("Any".to_string()),
                extends_args: vec![],
                fields: vec![Field {
                    name: "a".to_string(),
                    field_type: String::from("String"),
                }],
                methods: vec![Method {
                    is_virtual: false,
                    is_private: false,
                    name: "test_method".to_string(),
                    args: vec![],
                    return_type: String::from("Bool"),
                    body: Expr::True,
                }],
                init_blocks: vec![],
            }],
        };
        let mut scope = Scope::new();
        scope.set_class("A");
        scope.push_scope();
        scope.declare_var("a", &String::from("String"), ImmediateValue(0));
        scope.push_scope();
        scope.declare_var("b", &String::from("Bool"), ImmediateValue(0));
        let inference = Inference::new(&program, &scope);

        assert_eq!(
            inference.infer_static_type(&Parser::new(None, String::from("42")).read_expr()),
            String::from("Int")
        );
        assert_eq!(
            inference.infer_static_type(&Parser::new(None, String::from("a")).read_expr()),
            String::from("String")
        );
        assert_eq!(
            inference.infer_static_type(&Parser::new(None, String::from("b")).read_expr()),
            String::from("Bool")
        );
        assert_eq!(
            inference
                .infer_static_type(&Parser::new(None, String::from("if (b) 1 else 2")).read_expr()),
            String::from("Int")
        );
        assert_eq!(
            inference.infer_static_type(&Parser::new(None, String::from("this")).read_expr()),
            String::from("A")
        );
        assert_eq!(
            inference.infer_static_type(&Parser::new(None, String::from("super")).read_expr()),
            String::from("Any")
        );
        assert_eq!(
            inference.infer_static_type(
                &Parser::new(None, String::from("this.test_method(0)")).read_expr()
            ),
            String::from("Bool")
        );
    }
}
