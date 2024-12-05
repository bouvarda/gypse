//!
//! Parse source file to AST
//!
use crate::ast::Expr::{
    BinaryOp, BlockRef, False, FloatLiteral, If, IfElse, IntegerLiteral, Match, MethodCall, Minus,
    New, Not, Null, StringLiteral, Super, Syscall, This, True, Unit, VarAccess, VarRef, While,
};
use crate::ast::MatchPattern::{NullPattern, TypePattern};
use crate::ast::Op::{Add, And, Assign, Div, Eq, Ge, Gt, Le, Lt, Mul, Ne, Or, Sub};
use crate::ast::Statement::{ExprStmt, VarDeclStmt};
use crate::ast::{
    Block, Class, Expr, Field, Import, MatchCase, Method, MethodArg, Op, Program, Statement,
};
use crate::types::{ANY_TYPE, CTOR_NAME, STDLIB_IMPLICIT_IMPORTS, STDLIB_ROOT};
use std::path::Path;
use std::str::FromStr;
use std::{fs, vec};

pub struct Parser {
    filename: Option<String>,
    buf: String,
    pos: usize,
    line: usize,
}

impl Parser {
    pub fn new(filename: Option<&str>, buf: String) -> Parser {
        Parser {
            filename: filename.map(|f| String::from(f)),
            buf,
            pos: 0,
            line: 1,
        }
    }

    #[allow(dead_code)] // used in tests
    fn new_from_str(s: &str) -> Parser {
        Parser {
            filename: None,
            buf: String::from(s),
            pos: 0,
            line: 1,
        }
    }

    fn fmt_error(&self, msg: &str) -> String {
        match &self.filename {
            Some(f) => format!("Parse ERROR in {} at line {}: {}", f, self.line, msg),
            None => format!("ERROR: {}", msg),
        }
    }

    fn error(&self, msg: &str) {
        panic!("{}", self.fmt_error(msg))
    }

    fn char_at(&self, index: usize) -> char {
        self.buf
            .chars()
            .nth(index)
            .expect(&*self.fmt_error("Premature end of file"))
    }

    fn reset(&mut self, p: usize) {
        self.pos = p
    }

    fn read_eol(&mut self) {
        while self.pos < self.buf.len() && self.char_at(self.pos) != '\n' {
            self.pos += 1;
        }
        self.pos += 1;
        self.line += 1;
    }

    fn read_space(&mut self) {
        while self.pos < self.buf.len() {
            let c = self.char_at(self.pos);
            match c {
                ' ' | '\t' | '\r' | '\n' => {
                    if c == '\n' {
                        self.line += 1;
                    }
                    self.pos += 1
                }
                '/' => {
                    // read comment
                    if self.pos + 1 < self.buf.len() && self.char_at(self.pos + 1) == '/' {
                        self.read_eol();
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
    }

    fn read_char(&mut self, expected_char: char) -> bool {
        self.read_space();
        if self.pos < self.buf.len() {
            let c = self.char_at(self.pos);
            if c == expected_char {
                self.pos += 1;
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn read_mandatory_char(&mut self, expected_char: char) {
        if !self.read_char(expected_char) {
            let line = self.line;
            self.error(
                format!("Expected character '{expected_char}' not found at line {line}").as_str(),
            )
        }
    }

    fn read_word(&mut self) -> &str {
        self.read_space();
        let start = self.pos;
        while self.pos < self.buf.len()
            && (self.char_at(self.pos).is_alphabetic() || self.char_at(self.pos) == '_')
        {
            self.pos += 1
        }
        if start == self.pos {
            return "";
        }
        while self.pos < self.buf.len() && self.char_at(self.pos).is_alphanumeric() {
            self.pos += 1
        }
        if self.pos > 0 && self.pos > self.buf.len() {
            self.pos = self.buf.len();
        }
        &self.buf[start..self.pos]
    }

    fn read_field(&mut self) -> Option<Field> {
        let backup_pos = self.pos;
        match self.read_word() {
            "var" => {
                let name = self.read_word().to_owned();
                self.read_mandatory_char(':');
                let field_type = self.read_type();
                Some(Field {
                    name: name,
                    field_type: field_type,
                })
            }
            _ => {
                self.reset(backup_pos);
                None
            }
        }
    }

    fn read_integer(&mut self) -> Option<i32> {
        self.read_space();
        let start = self.pos;
        while self.pos < self.buf.len() && self.char_at(self.pos).is_numeric() {
            self.pos += 1
        }
        if self.pos - start > 0 {
            i32::from_str(&self.buf[start..self.pos]).ok()
        } else {
            None
        }
    }

    fn read_string(&mut self) -> Option<String> {
        if self.read_char('"') {
            let mut result = String::new();
            while self.pos < self.buf.len() {
                let c = self.char_at(self.pos);
                if c != '"' {
                    result.push(c);
                    self.pos += 1;
                } else {
                    break;
                }
            }
            self.read_mandatory_char('"');
            Some(result)
        } else {
            None
        }
    }

    fn read_operator(&mut self) -> Option<Op> {
        self.read_space();
        if self.pos >= self.buf.len() {
            return None;
        }
        let c = self.char_at(self.pos);
        self.pos += 1;
        match c {
            '+' => Some(Add),
            '-' => Some(Sub),
            '*' => Some(Mul),
            '/' => Some(Div),
            '<' => {
                if self.pos < self.buf.len() && self.char_at(self.pos) == '=' {
                    self.pos += 1;
                    Some(Le)
                } else if self.pos < self.buf.len() && self.char_at(self.pos) == '<' {
                    self.pos += 1;
                    Some(Op::LeftShift)
                } else {
                    Some(Lt)
                }
            }
            '>' => {
                if self.pos < self.buf.len() && self.char_at(self.pos) == '=' {
                    self.pos += 1;
                    Some(Ge)
                } else if self.pos < self.buf.len() && self.char_at(self.pos) == '>' {
                    self.pos += 1;
                    Some(Op::RightShift)
                } else {
                    Some(Gt)
                }
            }
            '=' => {
                if self.pos < self.buf.len() && self.char_at(self.pos) == '=' {
                    self.pos += 1;
                    Some(Eq)
                } else {
                    Some(Assign)
                }
            }
            '&' => {
                if self.pos < self.buf.len() && self.char_at(self.pos) == '&' {
                    self.pos += 1;
                    Some(And)
                } else {
                    Some(Op::BitwiseAnd)
                }
            }
            '|' => {
                if self.pos < self.buf.len() && self.char_at(self.pos) == '|' {
                    self.pos += 1;
                    Some(Or)
                } else {
                    Some(Op::BitwiseOr)
                }
            }
            '!' => {
                if self.pos < self.buf.len() && self.char_at(self.pos) == '=' {
                    self.pos += 1;
                    Some(Ne)
                } else {
                    None
                }
            }
            '^' => Some(Op::BitwiseXOr),
            _ => {
                self.pos -= 1;
                None
            }
        }
    }

    pub fn read_type(&mut self) -> String {
        String::from(self.read_word())
    }

    pub fn read_expr(&mut self) -> Expr {
        let mut primary = self.read_expr_primary();
        loop {
            let infix = self.read_expr_infix_op();
            if infix.is_some() {
                primary = match infix.unwrap() {
                    BinaryOp(_, op, right_expr) => BinaryOp(Box::new(primary), op, right_expr),
                    MethodCall(_, name, args) => MethodCall(Box::new(primary), name, args),
                    VarAccess(_, var_name) => VarAccess(Box::new(primary), var_name),
                    Match(_, cases) => Match(Box::new(primary), cases),
                    _ => Null,
                }
            } else {
                break;
            }
        }
        primary
    }

    fn read_expr_primary(&mut self) -> Expr {
        self.read_space();
        let c = self.char_at(self.pos);
        if c == '{' {
            self.pos += 1;
            let block = self.read_block();
            self.read_mandatory_char('}');
            BlockRef(block)
        } else if c == '(' {
            self.pos += 1;
            self.read_space();
            let c2 = self.char_at(self.pos);
            if c2 == ')' {
                self.pos += 1;
                Unit
            } else {
                let expr = self.read_expr();
                self.read_mandatory_char(')');
                expr
            }
        } else if c == '!' {
            self.pos += 1;
            let e = self.read_expr();
            Not(Box::new(e))
        } else if c == '-' {
            self.pos += 1;
            let e = self.read_expr();
            Minus(Box::new(e))
        } else {
            let w = self.read_word().to_owned();
            let mut e = match w.as_str() {
                "" => None,
                "null" => Some(Null),
                "true" => Some(True),
                "false" => Some(False),
                "this" | "super" => {
                    let keyword = if w.as_str() == "this" { This } else { Super };
                    self.read_space();
                    let backup = self.pos;
                    if self.pos < self.buf.len() && self.char_at(self.pos) == '.' {
                        self.pos += 1;
                        let attr = self.read_word().to_owned();
                        self.read_space();
                        if self.pos < self.buf.len() && self.char_at(self.pos) != '(' {
                            Some(VarAccess(Box::new(keyword), attr.to_string()))
                        } else {
                            self.pos = backup;
                            Some(keyword)
                        }
                    } else {
                        Some(keyword)
                    }
                }
                "new" => {
                    let class_type = self.read_type();
                    Some(New(class_type, Box::new(self.read_call_args())))
                }
                "if" => {
                    self.read_mandatory_char('(');
                    let cond = self.read_condition();
                    self.read_mandatory_char(')');
                    let then_expr = self.read_expr();

                    let backup_pos = self.pos;
                    if self.read_word() == "else" {
                        let else_expr = self.read_expr();
                        Some(IfElse(
                            Box::new(cond),
                            Box::new(then_expr),
                            Box::new(else_expr),
                        ))
                    } else {
                        self.reset(backup_pos);
                        Some(If(Box::new(cond), Box::new(then_expr)))
                    }
                }
                "while" => {
                    self.read_mandatory_char('(');
                    let cond = self.read_condition();
                    self.read_mandatory_char(')');
                    let body = self.read_expr();
                    Some(While(Box::new(cond), Box::new(body)))
                }
                id => {
                    self.read_space();
                    if self.pos < self.buf.len() && self.char_at(self.pos) == '(' {
                        Some(MethodCall(
                            Box::new(This),
                            w,
                            Box::new(self.read_call_args()),
                        ))
                    } else {
                        Some(VarRef(id.to_string()))
                    }
                }
            };
            if e.is_none() {
                let s = self.read_string();
                if s.is_some() {
                    e = Some(StringLiteral(s.unwrap()));
                }
                let i = self.read_integer();
                if i.is_some() {
                    if self.pos < self.buf.len() && self.char_at(self.pos) == '.' {
                        self.pos += 1;
                        let j = self.read_integer();
                        if j.is_none() {
                            self.error("Cannot read float literal");
                        }
                        let mut float_string = i.unwrap().to_string();
                        float_string.push('.');
                        float_string.push_str(j.unwrap().to_string().as_str());
                        e = Some(FloatLiteral(f32::from_str(float_string.as_str()).unwrap()));
                    } else {
                        e = Some(IntegerLiteral(i.unwrap()));
                    }
                }
            }
            e.unwrap_or(Unit)
        }
    }

    fn read_condition(&mut self) -> Expr {
        match self.read_expr() {
            // truthy expr: automatically inject "== true"
            MethodCall(left, name, args) => {
                BinaryOp(Box::from(MethodCall(left, name, args)), Eq, Box::from(True))
            }
            VarRef(name) => BinaryOp(Box::from(VarRef(name)), Eq, Box::from(True)),
            Not(expr) => {
                BinaryOp(expr, Eq, Box::from(False)) // !<expr> becomes <expr> == false
            }
            expr => expr,
        }
    }

    fn read_match(&mut self) -> Option<Expr> {
        let start = self.pos;
        if self.read_word() == "match" {
            let mut cases: Vec<MatchCase> = vec![];
            self.read_mandatory_char('{');
            loop {
                let start = self.pos;
                if self.read_word() != "case" {
                    self.pos = start;
                    break;
                }
                let w = self.read_word().to_owned();
                let pattern = if w == "null" {
                    NullPattern
                } else {
                    self.read_mandatory_char(':');
                    let pattern_type = self.read_word().to_owned();
                    TypePattern(w.to_owned(), pattern_type)
                };
                self.read_space();
                if !(self.pos + 2 < self.buf.len()
                    && self.char_at(self.pos) == '='
                    && self.char_at(self.pos + 1) == '>')
                {
                    self.error("Incorrect match case, expected '=>' syntax");
                }
                self.pos += 2;
                self.read_space();
                let bracket = self.read_char('{');
                let block = self.read_block();
                if bracket {
                    self.read_mandatory_char('}');
                }
                cases.push(MatchCase { pattern, block });
            }
            self.read_mandatory_char('}');
            Some(Match(Box::new(Null), cases))
        } else {
            self.pos = start;
            None
        }
    }

    fn read_expr_infix_op(&mut self) -> Option<Expr> {
        if let Some(op) = self.read_operator() {
            let right = self.read_expr();
            Some(BinaryOp(Box::new(Null), op, Box::new(right)))
        } else {
            if let Some(m) = self.read_match() {
                Some(m)
            } else {
                if self.pos < self.buf.len() && self.char_at(self.pos) == '.' {
                    self.pos += 1;
                    let method_name = self.read_word().to_owned();
                    self.read_space();
                    if self.pos < self.buf.len() && self.char_at(self.pos) == '(' {
                        let call_args = self.read_call_args();
                        Some(MethodCall(Box::new(Null), method_name, Box::new(call_args)))
                    } else {
                        Some(VarAccess(Box::new(Null), method_name))
                    }
                } else {
                    None
                }
            }
        }
    }

    fn read_field_with_expr(&mut self) -> Option<(Field, Expr)> {
        let field = self.read_field();
        if field.is_some() {
            self.read_mandatory_char('=');
            let expr = self.read_expr();
            Some((field.unwrap(), expr))
        } else {
            None
        }
    }

    fn read_method(&mut self, class_name: &str) -> Option<Method> {
        let backup_pos = self.pos;
        let mut token = self.read_word();
        let mut is_virtual = false;
        let mut is_private = false;
        if token == "private" {
            is_private = true;
            token = self.read_word();
        }
        if token == "virtual" {
            is_virtual = true;
            token = self.read_word();
        }
        if token == "def" {
            let name = self.read_word().to_owned();
            self.read_mandatory_char('(');
            let mut args: Vec<MethodArg> = vec![];
            loop {
                let arg_name = self.read_word().to_owned();
                if arg_name.is_empty() {
                    break;
                }
                self.read_mandatory_char(':');
                let field_type = self.read_type();
                args.push(MethodArg {
                    name: arg_name,
                    arg_type: field_type,
                });

                if !self.read_char(',') {
                    break;
                }
            }
            self.read_mandatory_char(')');
            self.read_mandatory_char(':');
            let return_type = self.read_type();
            self.read_mandatory_char('=');

            let backup_body = self.pos;
            let w = self.read_word();
            let expr = if w == "syscall" {
                let mut syscall_name = String::from(class_name);
                syscall_name.push_str("::");
                syscall_name.push_str(name.as_str());
                Syscall(syscall_name)
            } else {
                self.pos = backup_body;
                self.read_expr()
            };
            Some(Method {
                is_virtual,
                is_private,
                name,
                args,
                return_type,
                body: expr,
            })
        } else {
            self.reset(backup_pos);
            None
        }
    }

    fn read_block(&mut self) -> Block {
        let mut stmts: Vec<Statement> = vec![];
        loop {
            let backup_index = self.pos;
            let w = self.read_word();
            if w == "var" {
                let var_name = self.read_word().to_owned();
                self.read_mandatory_char(':');
                let var_type = self.read_type();
                self.read_mandatory_char('=');
                let e = self.read_expr();
                stmts.push(VarDeclStmt(var_name, var_type, e));
                if !self.read_char(';') {
                    return Block {
                        stmts,
                        return_expr: Box::new(Unit),
                    };
                }
            } else {
                self.pos = backup_index;
                let e = self.read_expr();
                if !self.read_char(';') {
                    return Block {
                        stmts,
                        return_expr: Box::new(e),
                    };
                }
                stmts.push(ExprStmt(e));
            }
        }
    }

    fn read_call_args(&mut self) -> Vec<Expr> {
        let mut args: Vec<Expr> = vec![];
        self.read_mandatory_char('(');
        let mut first: bool = true;
        loop {
            self.read_space();
            let c = self.char_at(self.pos);
            if c == ')' || (!first && c != ',') {
                break;
            }
            if !first {
                self.read_mandatory_char(',');
            }
            args.push(self.read_expr());
            if first {
                first = false;
            }
        }
        self.read_mandatory_char(')');
        args
    }

    fn read_class(&mut self) -> Option<Class> {
        match self.read_word() {
            "class" => {
                let class_type = self.read_type();
                if class_type.len() > 0 {
                    // read fields
                    self.read_mandatory_char('(');
                    let mut ctor_args: Vec<String> = vec![];
                    let mut fields: Vec<Field> = vec![];
                    while let Some(f) = self.read_field() {
                        ctor_args.push(f.name.clone());
                        fields.push(f);
                        if !self.read_char(',') {
                            break;
                        }
                    }
                    self.read_mandatory_char(')');

                    // read inheritance
                    let mut parent_class: Option<String> = None;
                    let mut parent_call_args: Vec<Expr> = vec![];
                    let extends = self.read_word();
                    if extends == "extends" {
                        parent_class = Some(self.read_word().to_owned());
                        parent_call_args = self.read_call_args();
                    } else if class_type != ANY_TYPE {
                        parent_class = Some(String::from(ANY_TYPE)) // default inheritance for all classes (except Any)
                    }

                    // read class body
                    self.read_mandatory_char('{');
                    let mut methods: Vec<Method> = vec![];
                    let mut init_blocks: Vec<Block> = vec![];
                    loop {
                        if let Some(m) = self.read_method(class_type.as_str()) {
                            methods.push(m);
                        } else if let Some((f, e)) = self.read_field_with_expr() {
                            init_blocks.push(Block {
                                stmts: vec![ExprStmt(BinaryOp(
                                    Box::new(VarRef(f.name.clone())),
                                    Assign,
                                    Box::new(e),
                                ))],
                                return_expr: Box::new(Unit),
                            });
                            fields.push(f);
                        } else if self.read_char('{') {
                            init_blocks.push(self.read_block());
                            self.read_mandatory_char('}');
                        } else {
                            break;
                        }
                        self.read_mandatory_char(';');
                    }
                    self.read_mandatory_char('}');
                    Some(Class::new(
                        class_type,
                        ctor_args,
                        parent_class,
                        parent_call_args,
                        fields,
                        methods,
                        init_blocks,
                    ))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn read_imports(&mut self) -> Vec<Import> {
        let mut imports: Vec<Import> = vec![];

        // implicit imports
        for implicit_import in STDLIB_IMPLICIT_IMPORTS {
            imports.push(Import {
                parts: vec![String::from(STDLIB_ROOT), String::from(implicit_import)],
            });
        }

        loop {
            let backup = self.pos;
            if self.read_word() == "import" {
                let mut parts: Vec<String> = vec![];
                let mut value = self.read_word();
                while value.len() > 0 {
                    parts.push(String::from(value));
                    if self.pos < self.buf.len() && self.char_at(self.pos) == '.' {
                        self.pos += 1;
                        value = self.read_word();
                    } else {
                        break;
                    }
                }
                self.read_mandatory_char(';');
                imports.push(Import { parts });
            } else {
                self.reset(backup);
                break;
            }
        }
        imports
    }

    fn read_program(&mut self) -> Program {
        let imports = self.read_imports();
        let mut classes: Vec<Class> = vec![];
        while let Some(class) = self.read_class() {
            classes.push(class);
        }
        Program { imports, classes }
    }
}

pub fn parse_file(file_path: &Path) -> Program {
    let buf = fs::read_to_string(file_path).expect("Cannot read file {file_path}");

    let mut parser = Parser::new(file_path.file_name().unwrap().to_str(), buf);
    parser.read_program()
}

#[test]
fn test_read_word() {
    assert_eq!(Parser::new_from_str("").read_word(), "");
    assert_eq!(Parser::new_from_str("good123").read_word(), "good123");
    assert_eq!(Parser::new_from_str("123bad").read_word(), "");
    assert_eq!(Parser::new_from_str("{word}").read_word(), "");
    assert_eq!(
        Parser::new_from_str("  \t class A() { }").read_word(),
        "class"
    );
    assert_eq!(
        Parser::new_from_str("//   class A() { }\n word").read_word(),
        "word"
    );
}

#[test]
fn test_read_integer() {
    assert_eq!(Parser::new_from_str("0").read_integer(), Some(0));
    assert_eq!(Parser::new_from_str(" 1234").read_integer(), Some(1234));
}

#[test]
fn test_read_string() {
    assert_eq!(
        Parser::new_from_str("\"toto à la plage\"").read_string(),
        Some("toto à la plage".to_string())
    );
    assert_eq!(
        Parser::new_from_str("\"str\"").read_string(),
        Some("str".to_string())
    );
    assert_eq!(
        Parser::new_from_str("\"\"").read_string(),
        Some("".to_string())
    );
}

#[test]
fn test_read_operator() {
    assert_eq!(Parser::new_from_str("").read_operator(), None);
    assert_eq!(Parser::new_from_str("=").read_operator(), Some(Assign));
    assert_eq!(Parser::new_from_str("==").read_operator(), Some(Eq));
    assert_eq!(Parser::new_from_str("<").read_operator(), Some(Lt));
    assert_eq!(Parser::new_from_str("<=").read_operator(), Some(Le));
    assert_eq!(Parser::new_from_str(" * ").read_operator(), Some(Mul));
    assert_eq!(Parser::new_from_str("/").read_operator(), Some(Div));

    assert_eq!(
        Parser::new_from_str("&").read_operator(),
        Some(Op::BitwiseAnd)
    );
    assert_eq!(
        Parser::new_from_str("|").read_operator(),
        Some(Op::BitwiseOr)
    );
    assert_eq!(
        Parser::new_from_str("^").read_operator(),
        Some(Op::BitwiseXOr)
    );
    assert_eq!(
        Parser::new_from_str("<<").read_operator(),
        Some(Op::LeftShift)
    );
    assert_eq!(
        Parser::new_from_str(">>").read_operator(),
        Some(Op::RightShift)
    );
}

#[test]
fn test_read_type() {
    assert_eq!(Parser::new_from_str("Int").read_type(), "Int".to_string());
    assert_eq!(
        Parser::new_from_str("Array").read_type(),
        "Array".to_string()
    );
}

#[test]
fn test_read_class() {
    assert_eq!(
        Parser::new_from_str(" class A() { }").read_class(),
        Some(Class::empty("A".to_string()))
    );
    assert_eq!(Parser::new_from_str("// end").read_class(), None);
    assert_eq!(
        Parser::new_from_str("class A(var a: Int) extends B(a) {}").read_class(),
        Some(Class {
            class_type: String::from("A"),
            ctor_args: vec!["a".to_string()],
            fields: vec![Field {
                name: "a".to_string(),
                field_type: String::from("Int")
            }],
            extends: Some("B".to_string()),
            extends_args: vec![VarRef("a".to_string())],
            methods: vec![Method {
                is_virtual: false,
                is_private: false,
                name: CTOR_NAME.to_string(),
                args: vec![MethodArg {
                    name: "_a".to_string(),
                    arg_type: "Int".to_string()
                }],
                return_type: "A".to_string(),
                body: BlockRef(Block {
                    stmts: vec![
                        ExprStmt(MethodCall(
                            Box::from(Super),
                            "__init__".to_string(),
                            Box::from(vec![VarRef("a".to_string())])
                        )),
                        ExprStmt(BinaryOp(
                            Box::from(VarAccess(Box::from(This), "a".to_string())),
                            Assign,
                            Box::from(VarRef("_a".to_string()))
                        ))
                    ],
                    return_expr: Box::from(This)
                })
            }],
            init_blocks: vec![]
        })
    );
}

#[test]
fn test_read_method() {
    assert_eq!(
        Parser::new_from_str("virtual def get_int(): Int = {}").read_method("A"),
        Some(Method {
            is_virtual: true,
            is_private: false,
            name: "get_int".to_string(),
            args: vec![],
            return_type: String::from("Int"),
            body: BlockRef(Block {
                stmts: vec![],
                return_expr: Box::new(Unit)
            }),
        })
    );
    assert_eq!(
        Parser::new_from_str("def with_args(a: Int, b: Bool): Int = {}").read_method("A"),
        Some(Method {
            is_virtual: false,
            is_private: false,
            name: "with_args".to_string(),
            args: vec![
                MethodArg {
                    name: "a".to_string(),
                    arg_type: String::from("Int")
                },
                MethodArg {
                    name: "b".to_string(),
                    arg_type: String::from("Bool")
                }
            ],
            return_type: String::from("Int"),
            body: BlockRef(Block {
                stmts: vec![],
                return_expr: Box::new(Unit)
            }),
        })
    );
    assert_eq!(
        Parser::new_from_str("private def private_method(): Unit = {}").read_method("A"),
        Some(Method {
            is_virtual: false,
            is_private: true,
            name: "private_method".to_string(),
            args: vec![],
            return_type: String::from("Unit"),
            body: BlockRef(Block {
                stmts: vec![],
                return_expr: Box::new(Unit)
            }),
        })
    );
    assert_eq!(Parser::new_from_str("// end").read_class(), None);
}

#[test]
fn test_read_expr_primary() {
    assert_eq!(Parser::new_from_str("12").read_expr(), IntegerLiteral(12));
    assert_eq!(
        Parser::new_from_str("\"str\"").read_expr(),
        StringLiteral("str".to_string())
    );
    assert_eq!(Parser::new_from_str("true").read_expr(), True);
    assert_eq!(Parser::new_from_str("this").read_expr(), This);
    assert_eq!(Parser::new_from_str("null").read_expr(), Null);
    assert_eq!(
        Parser::new_from_str("my_variable").read_expr(),
        VarRef("my_variable".to_string())
    );
    assert_eq!(Parser::new_from_str("((0))").read_expr(), IntegerLiteral(0));
    assert_eq!(Parser::new_from_str("()").read_expr(), Unit);
    assert_eq!(
        Parser::new_from_str("new MyClass()").read_expr(),
        New(String::from("MyClass"), Box::from(vec![]))
    );
    assert_eq!(
        Parser::new_from_str("if (a != b) 1 else 0").read_expr(),
        IfElse(
            Box::from(BinaryOp(
                Box::from(VarRef("a".to_string())),
                Ne,
                Box::from(VarRef("b".to_string()))
            )),
            Box::from(IntegerLiteral(1)),
            Box::from(IntegerLiteral(0))
        )
    );
    assert_eq!(
        Parser::new_from_str("while (true) { }").read_expr(),
        While(
            Box::from(True),
            Box::from(BlockRef(Block {
                stmts: vec![],
                return_expr: Box::from(Unit)
            }))
        )
    );
    assert_eq!(
        Parser::new_from_str("! true").read_expr(),
        Not(Box::from(True))
    );
    assert_eq!(
        Parser::new_from_str("-10").read_expr(),
        Minus(Box::from(IntegerLiteral(10)))
    );
    assert_eq!(
        Parser::new_from_str("out.print(\"hello\")").read_expr(),
        MethodCall(
            Box::from(VarRef("out".to_string())),
            "print".to_string(),
            Box::from(vec![StringLiteral("hello".to_string())])
        )
    );
    assert_eq!(
        Parser::new_from_str("this.my_func(0)").read_expr(),
        MethodCall(
            Box::from(This),
            "my_func".to_string(),
            Box::from(vec![IntegerLiteral(0)])
        )
    );
    assert_eq!(
        Parser::new_from_str("my_func(0)").read_expr(),
        MethodCall(
            Box::from(This),
            "my_func".to_string(),
            Box::from(vec![IntegerLiteral(0)])
        )
    );
    assert_eq!(
        Parser::new_from_str("this.get().func()").read_expr(),
        MethodCall(
            Box::from(MethodCall(
                Box::from(This),
                "get".to_string(),
                Box::from(vec![])
            )),
            "func".to_string(),
            Box::from(vec![])
        )
    );
    assert_eq!(
        Parser::new_from_str("if (stop) exit()").read_expr(),
        If(
            Box::from(BinaryOp(
                Box::from(VarRef("stop".to_string())),
                Eq,
                Box::from(True)
            )),
            Box::from(MethodCall(
                Box::from(This),
                "exit".to_string(),
                Box::from(vec![])
            ))
        )
    );
}

#[test]
fn test_read_expr_block() {
    assert_eq!(
        Parser::new_from_str("{ 12 }").read_expr(),
        BlockRef(Block {
            stmts: vec![],
            return_expr: Box::from(IntegerLiteral(12))
        })
    );
    assert_eq!(
        Parser::new_from_str("{ var x: Int = 1; x }").read_expr(),
        BlockRef(Block {
            stmts: vec![VarDeclStmt(
                "x".to_string(),
                String::from("Int"),
                IntegerLiteral(1)
            )],
            return_expr: Box::from(VarRef("x".to_string()))
        })
    );
}

#[test]
fn test_read_expr_infix() {
    assert_eq!(
        Parser::new_from_str("a + b").read_expr(),
        BinaryOp(
            Box::from(VarRef("a".to_string())),
            Add,
            Box::from(VarRef("b".to_string()))
        )
    );
    assert_eq!(
        Parser::new_from_str("a <= b").read_expr(),
        BinaryOp(
            Box::from(VarRef("a".to_string())),
            Le,
            Box::from(VarRef("b".to_string()))
        )
    );
    assert_eq!(
        Parser::new_from_str("a + b + 1").read_expr(),
        BinaryOp(
            Box::from(VarRef("a".to_string())),
            Add,
            Box::from(BinaryOp(
                Box::from(VarRef("b".to_string())),
                Add,
                Box::from(IntegerLiteral(1))
            ))
        )
    );
    assert_eq!(
        Parser::new_from_str("a + b > 0").read_expr(),
        BinaryOp(
            Box::from(VarRef("a".to_string())),
            Add,
            Box::from(BinaryOp(
                Box::from(VarRef("b".to_string())),
                Gt,
                Box::from(IntegerLiteral(0))
            ))
        )
    );
    assert_eq!(
        Parser::new_from_str("a = b - 10").read_expr(),
        BinaryOp(
            Box::from(VarRef("a".to_string())),
            Assign,
            Box::from(BinaryOp(
                Box::from(VarRef("b".to_string())),
                Sub,
                Box::from(IntegerLiteral(10))
            ))
        )
    );
    assert_eq!(
        Parser::new_from_str("a match { case i: Int => i }").read_expr(),
        Match(
            Box::from(VarRef("a".to_string())),
            vec![MatchCase {
                pattern: TypePattern("i".to_string(), "Int".to_string()),
                block: Block {
                    stmts: vec![],
                    return_expr: Box::new(VarRef("i".to_string()))
                }
            }]
        )
    );
    assert_eq!(
        Parser::new_from_str("(a.get(0)) match { case i: Int => i }").read_expr(),
        Match(
            Box::from(MethodCall(
                Box::from(VarRef("a".to_string())),
                "get".to_string(),
                Box::new(vec![IntegerLiteral(0)])
            )),
            vec![MatchCase {
                pattern: TypePattern("i".to_string(), "Int".to_string()),
                block: Block {
                    stmts: vec![],
                    return_expr: Box::new(VarRef("i".to_string()))
                }
            }]
        )
    );
    assert_eq!(
        Parser::new_from_str("this.a = a").read_expr(),
        BinaryOp(
            Box::from(VarAccess(Box::from(This), "a".to_string())),
            Assign,
            Box::from(VarRef("a".to_string()))
        )
    );
    assert_eq!(
        Parser::new_from_str("a.b").read_expr(),
        VarAccess(Box::from(VarRef("a".to_string())), "b".to_string())
    );
    assert_eq!(
        Parser::new_from_str("a && b").read_expr(),
        BinaryOp(
            Box::from(VarRef("a".to_string())),
            And,
            Box::from(VarRef("b".to_string()))
        )
    );
    assert_eq!(
        Parser::new_from_str("a || b").read_expr(),
        BinaryOp(
            Box::from(VarRef("a".to_string())),
            Or,
            Box::from(VarRef("b".to_string()))
        )
    );
}

#[test]
fn test_read_call_args() {
    assert_eq!(Parser::new_from_str("()").read_call_args(), vec![]);
    assert_eq!(
        Parser::new_from_str("(1)").read_call_args(),
        vec![IntegerLiteral(1)]
    );
    assert_eq!(
        Parser::new_from_str("(a, b, \"c\")").read_call_args(),
        vec![
            VarRef("a".to_string()),
            VarRef("b".to_string()),
            StringLiteral("c".to_string())
        ]
    );
}

#[test]
fn test_match() {
    assert_eq!(Parser::new_from_str("").read_match(), None);
    assert_eq!(
        Parser::new_from_str("match { case i: Int => true case null => false }").read_match(),
        Some(Match(
            Box::from(Null),
            vec![
                MatchCase {
                    pattern: TypePattern("i".to_string(), "Int".to_string()),
                    block: Block {
                        stmts: vec![],
                        return_expr: Box::from(True)
                    }
                },
                MatchCase {
                    pattern: NullPattern,
                    block: Block {
                        stmts: vec![],
                        return_expr: Box::from(False)
                    }
                }
            ]
        ))
    );
    assert_eq!(
        Parser::new_from_str("match { case s: String => {s} }").read_match(),
        Some(Match(
            Box::from(Null),
            vec![MatchCase {
                pattern: TypePattern("s".to_string(), "String".to_string()),
                block: Block {
                    stmts: vec![],
                    return_expr: Box::from(VarRef("s".to_string()))
                }
            }]
        ))
    );
}

#[test]
fn test_read_truthy_condition() {
    assert_eq!(
        Parser::new_from_str("if (has_next()) 1 else -1").read_expr(),
        IfElse(
            Box::from(BinaryOp(
                Box::new(MethodCall(
                    Box::from(This),
                    "has_next".to_string(),
                    Box::from(vec![])
                )),
                Eq,
                Box::from(True)
            )),
            Box::from(IntegerLiteral(1)),
            Box::from(Minus(Box::new(IntegerLiteral(1))))
        )
    );
    assert_eq!(
        Parser::new_from_str("toto").read_condition(),
        BinaryOp(Box::new(VarRef("toto".to_string())), Eq, Box::from(True))
    );
}

#[test]
fn test_read_falsy_condition() {
    assert_eq!(
        Parser::new_from_str("!stop").read_condition(),
        BinaryOp(Box::new(VarRef("stop".to_string())), Eq, Box::from(False))
    );
}

#[test]
fn test_read_complex_and_or_condition() {
    assert_eq!(
        Parser::new_from_str("((n > 0) && (n < 10)) || (n == -1)").read_condition(),
        BinaryOp(
            Box::from(BinaryOp(
                Box::from(BinaryOp(
                    Box::from(VarRef("n".to_string())),
                    Gt,
                    Box::from(IntegerLiteral(0))
                )),
                And,
                Box::from(BinaryOp(
                    Box::from(VarRef("n".to_string())),
                    Lt,
                    Box::from(IntegerLiteral(10))
                ))
            )),
            Or,
            Box::from(BinaryOp(
                Box::from(VarRef("n".to_string())),
                Eq,
                Box::from(Minus(Box::from(IntegerLiteral(1))))
            ))
        )
    );
}

#[test]
fn test_read_imports() {
    assert_eq!(
        Parser::new_from_str("import std.collection.List;").read_imports(),
        vec![
            Import {
                parts: vec!["std".to_string(), "Any".to_string()]
            },
            Import {
                parts: vec!["std".to_string(), "String".to_string()]
            },
            Import {
                parts: vec![
                    "std".to_string(),
                    "collection".to_string(),
                    "List".to_string()
                ]
            }
        ]
    )
}
