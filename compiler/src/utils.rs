use crate::ast::Statement::{ExprStmt, VarDeclStmt};
use crate::ast::{Block, Expr, MatchPattern, Method, Op, Program};
use common::OBJECT_HEADER_SIZE;

pub fn sizeof_class_object(class_type: &str, program: &Program) -> i32 {
    let class = program.find_class(class_type);

    let mut cur_size: i32 = OBJECT_HEADER_SIZE as i32; // size for object header
                                                       // TODO add size from inheritance
    for field in &class.fields {
        cur_size += sizeof_class_object_field(&field.field_type);
    }
    cur_size
}

pub fn sizeof_class_object_field(field_type: &str) -> i32 {
    match field_type {
        "Bool" => 1,
        "Int" => 4,
        _ => 4, // non-primitive object reference have 4 bytes for the address
    }
}

pub fn print_ast(program: &Program) {
    let mut out = String::new();
    for class in &program.classes {
        out.push_str("class ");
        out.push_str(class.class_type.as_str());
        out.push_str("(");
        for arg in &class.ctor_args {
            let field = class.fields.iter().find(|f| f.name == *arg).unwrap();
            out.push_str("var ");
            out.push_str(arg.as_str());
            out.push_str(": ");
            print_type(&mut out, &field.field_type);
            out.push_str(", ");
        }
        out.push_str(") ");
        if class.extends.is_some() {
            out.push_str("extends ");
            out.push_str(class.extends.as_ref().unwrap().as_str());
            out.push_str("(");
            for expr in &class.extends_args {
                print_expr(&mut out, &expr);
            }
            out.push_str(")");
        }
        out.push_str(" {\n");
        for field in &class.fields {
            out.push_str("var ");
            out.push_str(field.name.as_str());
            out.push_str(": ");
            print_type(&mut out, &field.field_type);
            out.push_str(";\n");
        }
        for method in &class.methods {
            print_method(&mut out, &method);
        }
        out.push_str("}\n");
    }
    println!("{out}");
}

fn print_method(out: &mut String, method: &Method) {
    if method.is_virtual {
        out.push_str("virtual ");
    }
    out.push_str("def ");
    out.push_str(method.name.as_str());
    out.push_str("(");
    for arg in &method.args {
        out.push_str(arg.name.as_str());
        out.push_str(": ");
        print_type(out, &arg.arg_type);
        out.push_str(", ");
    }
    out.push_str(") = \n");
    print_expr(out, &method.body);
    out.push_str("\n");
}

fn print_type(out: &mut String, class_type: &str) {
    out.push_str(class_type);
}

fn print_expr(out: &mut String, expr: &Expr) {
    match expr {
        Expr::True => out.push_str("true"),
        Expr::False => out.push_str("false"),
        Expr::Unit => out.push_str("Unit"),
        Expr::Null => out.push_str("null"),
        Expr::This => out.push_str("this"),
        Expr::Super => out.push_str("super"),
        Expr::New(cls, args) => {
            out.push_str("new ");
            print_type(out, cls);
            out.push_str("(");
            for arg in args.as_ref() {
                print_expr(out, arg);
                out.push_str(", ");
            }
            out.push_str(")");
        }
        Expr::VarAccess(left, name) => {
            print_expr(out, left);
            out.push('.');
            out.push_str(name);
        }
        Expr::VarRef(name) => out.push_str(name),
        Expr::StringLiteral(s) => {
            out.push_str("\"");
            out.push_str(s);
            out.push_str("\"");
        }
        Expr::IntegerLiteral(i) => out.push_str(i.to_string().as_str()),
        Expr::FloatLiteral(f) => out.push_str(f.to_string().as_str()),
        Expr::Not(e) => {
            out.push_str("!");
            print_expr(out, e);
        }
        Expr::Minus(e) => {
            out.push_str("-");
            print_expr(out, e);
        }
        Expr::If(cond, then_expr) => {
            out.push_str("if (");
            print_expr(out, cond);
            out.push_str(") {\n");
            print_expr(out, then_expr);
            out.push_str("}\n");
        }
        Expr::IfElse(cond, then_expr, else_expr) => {
            out.push_str("if (");
            print_expr(out, cond);
            out.push_str(") {\n");
            print_expr(out, then_expr);
            out.push_str("} else {\n");
            print_expr(out, else_expr);
            out.push_str("}\n");
        }
        Expr::While(cond, body) => {
            out.push_str("while (");
            print_expr(out, cond);
            out.push_str(") {\n");
            print_expr(out, body);
            out.push_str("}\n");
        }
        Expr::BlockRef(block) => {
            print_block(out, block);
        }
        Expr::BinaryOp(left, op, right) => {
            print_expr(out, left);
            match op {
                Op::Add => out.push_str(" + "),
                Op::Sub => out.push_str(" - "),
                Op::Mul => out.push_str(" * "),
                Op::Div => out.push_str(" / "),
                Op::Gt => out.push_str(" > "),
                Op::Ge => out.push_str(" >= "),
                Op::Lt => out.push_str(" < "),
                Op::Le => out.push_str(" <= "),
                Op::Eq => out.push_str(" == "),
                Op::Ne => out.push_str(" != "),
                Op::Assign => out.push_str(" = "),
                Op::And => out.push_str(" && "),
                Op::Or => out.push_str(" || "),
                Op::BitwiseAnd => out.push_str(" & "),
                Op::BitwiseOr => out.push_str(" | "),
                Op::BitwiseXOr => out.push_str(" ^ "),
                Op::LeftShift => out.push_str(" << "),
                Op::RightShift => out.push_str(" >> "),
            }
            print_expr(out, right);
        }
        Expr::MethodCall(left, name, args) => {
            print_expr(out, left);
            out.push_str(".");
            out.push_str(name);
            out.push_str("(");
            for arg in args.as_ref() {
                print_expr(out, arg);
                out.push_str(", ");
            }
            out.push_str(")");
        }
        Expr::Match(left, cases) => {
            print_expr(out, left);
            out.push_str(" match {\n");
            for c in cases {
                match &c.pattern {
                    MatchPattern::NullPattern => out.push_str("case null => "),
                    MatchPattern::TypePattern(name, datatype) => {
                        out.push_str("case ");
                        out.push_str(name.as_str());
                        out.push_str(": ");
                        out.push_str(datatype.as_str());
                        out.push_str(" => ");
                    }
                }
                print_block(out, &c.block);
            }
            out.push_str("}");
        }
        Expr::Syscall(name) => {
            out.push_str("syscall ");
            out.push_str(name.to_string().as_str());
        }
    }
}

fn print_block(out: &mut String, block: &Block) {
    out.push_str("{");
    for stmt in &block.stmts {
        match stmt {
            VarDeclStmt(name, datatype, expr) => {
                out.push_str("var ");
                out.push_str(name.as_str());
                out.push_str(": ");
                print_type(out, datatype);
                out.push_str(" = ");
                print_expr(out, &expr);
            }
            ExprStmt(e) => print_expr(out, &e),
        }
        out.push_str(";\n");
    }
    print_expr(out, &block.return_expr);
    out.push_str("}");
}
