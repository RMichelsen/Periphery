use crate::parser::{AstNode, AstKind, AstType, NO_IDX};
use crate::typer::FnKey;
use std::collections::HashMap;

// Debug printer
fn format_type<'a>(ty: AstType<'a>) -> String {
    match ty {
        AstType::Int => "int".to_string(),
        AstType::Float => "float".to_string(),
        AstType::Bool => "bool".to_string(),
        AstType::Byte => "byte".to_string(),
        AstType::Void => "void".to_string(),
        AstType::Struct(name) => format!("struct {}", name),
        AstType::Array { base, dims } => {
            let mut depth = 0; while depth < dims.len() && dims[depth] != usize::MAX { depth += 1; }
            let mut s = String::new();
            use std::fmt::Write;
            for i in 0..depth { write!(s, "[{} x ", dims[i]).unwrap(); }
            match base {
                "int" => s.push_str("int"),
                "float" => s.push_str("float"),
                "bool" => s.push_str("bool"),
                "byte" => s.push_str("byte"),
                "void" => s.push_str("void"),
                other => { write!(s, "struct {}", other).unwrap(); }
            }
            for _ in 0..depth { s.push(']'); }
            s
        }
        AstType::Ptr(base) => format!("ptr {}", base),
        AstType::Return(_) => "_unresolvedreturn".to_string(),
        AstType::Unknown => "unknown".to_string(),
    }
}

fn print_ast_inner<'a>(ast: &[AstNode<'a>], root: usize) {
    fn walk_list<'a>(ast: &[AstNode<'a>], mut cur: usize, indent: usize, f: &dyn Fn(&[AstNode<'a>], usize, usize)) {
        while cur != NO_IDX {
            f(ast, cur, indent);
            cur = ast[cur].next;
        }
    }

    fn rec<'a>(ast: &[AstNode<'a>], idx: usize, indent: usize) {
        let pad = "  ".repeat(indent);
        let node = &ast[idx];
        let ty_s = format_type(node.ty);
        match &node.kind {
            AstKind::Integer { value } => println!("{}Integer value={} type={} @{}:{}", pad, value, ty_s, node.line, node.column),
            AstKind::Float { value }   => println!("{}Float value={} type={} @{}:{}", pad, value, ty_s, node.line, node.column),
            AstKind::Bool { value }    => println!("{}Bool value={} type={} @{}:{}", pad, value, ty_s, node.line, node.column),
            AstKind::String { value, cooked_len }  => println!("{}String \"{}\" len={} type={} @{}:{}", pad, value, cooked_len, ty_s, node.line, node.column),
            AstKind::Void {}           => println!("{}Void type={} @{}:{}", pad, ty_s, node.line, node.column),
            AstKind::Name { name }     => println!("{}Name {} type={} @{}:{}", pad, name, ty_s, node.line, node.column),
            AstKind::Assignment { name, expr } => {
                println!("{}Assignment {} type={} @{}:{}", pad, name, ty_s, node.line, node.column);
                rec(ast, *expr, indent + 1);
            }
            AstKind::Unary { op, expr } => {
                println!("{}Unary {:?} type={} @{}:{}", pad, op, ty_s, node.line, node.column);
                rec(ast, *expr, indent + 1);
            }
            AstKind::Binary { op, left, right } => {
                println!("{}Binary {:?} type={} @{}:{}", pad, op, ty_s, node.line, node.column);
                rec(ast, *left, indent + 1);
                rec(ast, *right, indent + 1);
            }
            AstKind::Conditional { cond, true_expr, false_expr } => {
                println!("{}Conditional type={} @{}:{}", pad, ty_s, node.line, node.column);
                // Label each branch for clearer debugging
                println!("{}  If:", pad);
                rec(ast, *cond, indent + 2);
                println!("{}  True:", pad);
                rec(ast, *true_expr, indent + 2);
                println!("{}  False:", pad);
                rec(ast, *false_expr, indent + 2);
            }
            AstKind::Block { exprs, expr_count } => {
                println!("{}Block count={} type={} @{}:{}", pad, expr_count, ty_s, node.line, node.column);
                walk_list(ast, *exprs, indent + 1, &rec);
            }
            AstKind::Call { name, args, arg_count } => {
                // Collect argument types for display
                let mut cur = *args;
                let mut arg_types: Vec<String> = Vec::new();
                while cur != NO_IDX {
                    arg_types.push(format_type(ast[cur].ty));
                    cur = ast[cur].next;
                }
                println!("{}Call to {} args={} types=[{}] ret_type={} @{}:{}", pad, name, arg_count, arg_types.join(", "), ty_s, node.line, node.column);
                walk_list(ast, *args, indent + 1, &rec);
            }
            AstKind::StructInit { name, fields, field_count } => {
                println!("{}StructInit {} fields={} type={} @{}:{}", pad, name, field_count, ty_s, node.line, node.column);
                walk_list(ast, *fields, indent + 1, &rec);
            }
            AstKind::ArrayInit { elems, elem_count } => {
                println!("{}ArrayInit count={} type={} @{}:{}", pad, elem_count, ty_s, node.line, node.column);
                walk_list(ast, *elems, indent + 1, &rec);
            }
            AstKind::ArrayAccess { expr, index_expr } => {
                println!("{}ArrayAccess type={} @{}:{}", pad, ty_s, node.line, node.column);
                rec(ast, *expr, indent + 1);
                rec(ast, *index_expr, indent + 1);
            }
            AstKind::FieldInit { name, expr } => {
                println!("{}FieldInit {} type={} @{}:{}", pad, name, ty_s, node.line, node.column);
                rec(ast, *expr, indent + 1);
            }
            AstKind::FieldAccess { expr, name } => {
                println!("{}FieldAccess {} type={} @{}:{}", pad, name, ty_s, node.line, node.column);
                rec(ast, *expr, indent + 1);
            }
            AstKind::Unknown => println!("{}Unknown type={} @{}:{}", pad, ty_s, node.line, node.column),
        }
    }

    rec(ast, root, 0);
}

pub fn print_ast<'a>(ast: &[AstNode<'a>], root: usize, fn_instances: &HashMap<FnKey<'a>, usize>) {
    print_ast_inner(ast, root);
    println!();
    for (fn_key, root_idx) in fn_instances.iter() {
        print!("Function Instance: {}", fn_key.name);
        for ty in fn_key.param_types.iter() {
            if *ty == AstType::Unknown { break; }
            match ty {
                AstType::Int => print!("_int"),
                AstType::Float => print!("_float"),
                AstType::Bool => print!("_bool"),
                AstType::Byte => print!("_byte"),
                AstType::Void => print!("_void"),
                AstType::Struct(name) => print!("_struct({})", name),
                AstType::Array { base, dims } => {
                    let mut depth = 0; while depth < dims.len() && dims[depth] != usize::MAX { depth += 1; }
                    print!("_array({};", base);
                    for i in 0..depth { print!("{}{}", dims[i], if i+1<depth { "x" } else { "" }); }
                    print!(")");
                }
                AstType::Return(_) => print!("_unresolvedreturn"),
                    AstType::Ptr(base) => print!("_ptr({})", base),
                AstType::Unknown => {},
            }
        }
        println!(" @{}:{}", ast[*root_idx].line, ast[*root_idx].column);
        print_ast_inner(ast, *root_idx);
        println!();
    }
}
