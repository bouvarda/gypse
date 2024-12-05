//!
//! Abstract Syntax Tree type definitions and helpers
//!
use crate::ast::Expr::{BinaryOp, MethodCall, Super, This, VarAccess, VarRef};
use crate::ast::Op::Assign;
use crate::ast::Statement::ExprStmt;
use crate::types::{ANY_TYPE, CTOR_NAME, SOURCE_FILE_EXTENSION};
use common::OBJECT_HEADER_SIZE;

use std::fmt::Debug;

#[derive(PartialEq, Debug, Clone)]
pub struct Program {
    pub imports: Vec<Import>,
    pub classes: Vec<Class>,
}

impl Program {
    pub fn find_class(&self, name: &str) -> &Class {
        for c in &self.classes {
            if c.class_type == name {
                return c;
            }
        }
        panic!("Class not found: {name}")
    }
}

#[derive(PartialEq, Debug, Clone, Eq, Hash)]
pub struct Import {
    pub parts: Vec<String>,
}

impl Import {
    pub fn new(init: &str) -> Import {
        if init.ends_with(SOURCE_FILE_EXTENSION) {
            Import {
                parts: vec![String::from(
                    init.strip_suffix(SOURCE_FILE_EXTENSION).unwrap(),
                )],
            }
        } else {
            Import {
                parts: vec![String::from(init)],
            }
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Class {
    pub class_type: String,
    pub ctor_args: Vec<String>,
    pub extends: Option<String>,
    pub extends_args: Vec<Expr>,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
    pub init_blocks: Vec<Block>,
}

impl Class {
    #[allow(dead_code)] // used in tests
    pub fn empty(name: String) -> Class {
        Class {
            class_type: name.clone(),
            ctor_args: vec![],
            extends: Some(ANY_TYPE.to_string()),
            extends_args: vec![],
            fields: vec![],
            methods: vec![Method {
                is_virtual: false,
                is_private: false,
                name: CTOR_NAME.to_string(),
                args: vec![],
                body: Expr::BlockRef(Block {
                    stmts: vec![],
                    return_expr: Box::from(This),
                }),
                return_type: name.clone(),
            }],
            init_blocks: vec![],
        }
    }

    pub fn new(
        class_type: String,
        ctor_args: Vec<String>,
        extends: Option<String>,
        extends_args: Vec<Expr>,
        fields: Vec<Field>,
        methods: Vec<Method>,
        init_blocks: Vec<Block>,
    ) -> Class {
        let mut result = Class {
            class_type,
            ctor_args,
            extends,
            extends_args,
            fields,
            methods,
            init_blocks,
        };
        if result.class_type != ANY_TYPE {
            result.methods.push(result.gen_ctor()); // TODO refactor gen_ctor
        }
        result
    }

    pub fn super_class_name_or_this(&self) -> String {
        match &self.extends {
            Some(e) => e.clone(),
            None => self.class_type.clone(),
        }
    }

    pub fn find_field(&self, name: &str) -> &Field {
        for f in &self.fields {
            if f.name == name {
                return f;
            }
        }
        panic!("Field not found: {name}")
    }

    pub fn find_field_index(&self, name: &str) -> Option<i16> {
        let mut idx: i16 = OBJECT_HEADER_SIZE as i16; // skip header bytes
        for f in &self.fields {
            if f.name == name {
                return Some(idx);
            }
            idx += 4;
        }
        None
    }

    pub fn lookup_method<'a>(
        &'a self,
        method_name: &str,
        program: &'a Program,
    ) -> Option<(&Class, &Method)> {
        for m in &self.methods {
            if m.name == method_name {
                return Some((self, m));
            }
        }
        match &self.extends {
            Some(extends) => {
                let parent = program.find_class(extends);
                parent.lookup_method(method_name, program)
            }
            None => None,
        }
    }

    fn gen_ctor(&self) -> Method {
        let mut args: Vec<MethodArg> = vec![];
        let mut stmts: Vec<Statement> = vec![];
        // call super ctor
        match &self.extends {
            Some(c) if c != ANY_TYPE => {
                let mut super_args: Vec<Expr> = vec![];
                for super_arg in &self.extends_args {
                    super_args.push(super_arg.clone());
                }
                stmts.push(ExprStmt(MethodCall(
                    Box::new(Super),
                    "__init__".to_owned(),
                    Box::new(super_args),
                )));
            }
            _ => {}
        }
        // assign fields with underscore prefix to avoid name shadowing, ex: this.a = _a;  instead of this.a = a;
        for arg_name in &self.ctor_args {
            let field = self.fields.iter().find(|f| f.name == *arg_name).unwrap();

            let mut arg_name_with_prefix = String::new();
            arg_name_with_prefix.push('_');
            arg_name_with_prefix.push_str(arg_name);

            args.push(MethodArg {
                name: arg_name_with_prefix.clone(),
                arg_type: field.field_type.clone(),
            });
            // "this.a = _a;"
            stmts.push(ExprStmt(BinaryOp(
                Box::new(VarAccess(Box::new(This), arg_name.clone())),
                Assign,
                Box::new(VarRef(arg_name_with_prefix)),
            )));
        }
        // inject init block in the constructor
        for block in &self.init_blocks {
            for s in &block.stmts {
                stmts.push(s.clone());
            }
            let return_expr = block.return_expr.as_ref();
            if *return_expr != Expr::Unit {
                // do not retrieve Unit return
                stmts.push(ExprStmt(return_expr.clone()));
            }
        }
        Method {
            is_virtual: false,
            is_private: false,
            name: "__init__".to_owned(),
            args,
            return_type: self.class_type.clone(),
            body: Expr::BlockRef(Block {
                stmts,
                return_expr: Box::new(This),
            }),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Field {
    pub name: String,
    pub field_type: String,
}

#[derive(PartialEq, Debug, Clone)]
pub struct MethodArg {
    pub name: String,
    pub arg_type: String,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Method {
    pub is_virtual: bool,
    pub is_private: bool,
    pub name: String,
    pub args: Vec<MethodArg>,
    pub return_type: String,
    pub body: Expr,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    VarDeclStmt(String, String, Expr),
    ExprStmt(Expr),
}

#[derive(PartialEq, Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Statement>,
    pub return_expr: Box<Expr>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum MatchPattern {
    NullPattern,
    TypePattern(String, String),
}

#[derive(PartialEq, Debug, Clone)]
pub struct MatchCase {
    pub pattern: MatchPattern,
    pub block: Block,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Gt,
    Ge,
    Lt,
    Le,
    Eq,
    Ne,
    Assign,
    And,
    Or,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXOr,
    LeftShift,
    RightShift,
}

impl Op {
    pub fn is_arithmetic(&self) -> bool {
        *self == Op::Add || *self == Op::Sub || *self == Op::Mul || *self == Op::Div
    }

    pub fn is_bitwise(&self) -> bool {
        *self == Op::BitwiseAnd
            || *self == Op::BitwiseOr
            || *self == Op::BitwiseXOr
            || *self == Op::LeftShift
            || *self == Op::RightShift
    }

    #[allow(dead_code)]
    pub fn is_compare(&self) -> bool {
        *self == Op::Gt
            || *self == Op::Ge
            || *self == Op::Lt
            || *self == Op::Le
            || *self == Op::Eq
            || *self == Op::Ne
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    True,
    False,
    Unit,
    Null,
    This,
    Super,
    New(String, Box<Vec<Expr>>),  // new A(v1, v2)
    VarAccess(Box<Expr>, String), // this.a
    VarRef(String),               // a
    StringLiteral(String),
    IntegerLiteral(i32),
    FloatLiteral(f32),
    Not(Box<Expr>),
    Minus(Box<Expr>),
    If(Box<Expr>, Box<Expr>),
    IfElse(Box<Expr>, Box<Expr>, Box<Expr>),
    While(Box<Expr>, Box<Expr>),
    BlockRef(Block),
    BinaryOp(Box<Expr>, Op, Box<Expr>),
    MethodCall(Box<Expr>, String, Box<Vec<Expr>>),
    Match(Box<Expr>, Vec<MatchCase>),
    Syscall(String),
}
