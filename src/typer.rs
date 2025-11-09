use crate::parser::{AstKind, AstNode, AstType, StructDecl, FunctionDecl, ExternFunctionDecl, AstValue, NO_IDX};
use crate::lexer::TokenKind;

use std::collections::HashMap;

#[derive(Debug, PartialEq, Hash, Eq, Clone, Copy)]
pub struct FnKey<'a> {
    pub name: &'a str,
    pub param_types: [AstType<'a>; 32],
}

struct Typer<'a> {
    ast: Vec<AstNode<'a>>,
    struct_decls: &'a HashMap<&'a str, StructDecl<'a>>,
    fn_decls: &'a HashMap<&'a str, FunctionDecl<'a>>,
    extern_fns: &'a HashMap<&'a str, ExternFunctionDecl<'a>>,
    fn_instances: HashMap<FnKey<'a>, usize>,
    fn_return_indices: HashMap<FnKey<'a>, usize>,
    fn_return_types: Vec<AstType<'a>>,
    deferred_type_checks: Vec<(AstType<'a>, AstType<'a>, usize)>,
}

impl<'a> Typer<'a> {
    fn push_copy(&mut self, line: u32, column: u32, kind: AstKind<'a>) -> usize {
        let idx = self.ast.len();
        self.ast.push(AstNode {
            kind: kind,
            ty: AstType::Unknown,
            line,
            column,
            next: NO_IDX,
            ssa: AstValue::Void,
        });
        idx
    }

    fn clone_subtree(&mut self, root: usize) -> usize {
        let (line, col) = (self.ast[root].line, self.ast[root].column);
        match self.ast[root].kind {
            AstKind::Integer { value } => {
                self.push_copy(line, col, AstKind::Integer { value })
            }
            AstKind::Float { value } => {
                self.push_copy(line, col, AstKind::Float { value })
            }
            AstKind::Bool { value } => {
                self.push_copy(line, col, AstKind::Bool { value })
            }
            AstKind::String { value, cooked_len } => {
                self.push_copy(line, col, AstKind::String { value, cooked_len })
            }
            AstKind::Void { } => {
                self.push_copy(line, col, AstKind::Void {})
            }
            AstKind::Unary { op, expr } => {
                let expr_idx = self.clone_subtree(expr);
                self.push_copy(line, col, AstKind::Unary { op, expr: expr_idx })
            }
            AstKind::Binary { op, left, right } => {
                let left_idx = self.clone_subtree(left);
                let right_idx = self.clone_subtree(right);
                self.push_copy(line, col, AstKind::Binary { op, left: left_idx, right: right_idx })
            }
            AstKind::Assignment { name, expr } => {
                let expr_idx = self.clone_subtree(expr);
                self.push_copy(line, col, AstKind::Assignment { name, expr: expr_idx })
            }
            AstKind::ArrayInit { elems, elem_count } => {
                if elem_count == 0 || elems == NO_IDX {
                    return self.push_copy(line, col, AstKind::ArrayInit { elems: NO_IDX, elem_count: 0 });
                }
                let mut elem = elems;
                let first_elem_idx = self.clone_subtree(elem);
                let mut last_elem_idx = first_elem_idx;
                elem = self.ast[elem].next;
                for _ in 1..elem_count {
                    let new_elem_idx = self.clone_subtree(elem);
                    self.ast[last_elem_idx].next = new_elem_idx;
                    last_elem_idx = new_elem_idx;
                    elem = self.ast[elem].next;
                }
                self.push_copy(line, col, AstKind::ArrayInit { elems: first_elem_idx, elem_count })
            }
            AstKind::ArrayAccess { expr, index_expr } => {
                let expr_idx = self.clone_subtree(expr);
                let index_expr_idx = self.clone_subtree(index_expr);
                self.push_copy(line, col, AstKind::ArrayAccess { expr: expr_idx, index_expr: index_expr_idx })
            }
            AstKind::StructInit { name, fields, field_count } => {
                if field_count == 0 || fields == NO_IDX {
                    return self.push_copy(line, col, AstKind::StructInit { name, fields: NO_IDX, field_count: 0 });
                }
                let mut field = fields;
                let first_field_idx = self.clone_subtree(field);
                let mut last_field_idx = first_field_idx;
                field = self.ast[field].next;
                for _ in 1..field_count {
                    let new_field_idx = self.clone_subtree(field);
                    self.ast[last_field_idx].next = new_field_idx;
                    last_field_idx = new_field_idx;
                    field = self.ast[field].next;
                }
                self.push_copy(line, col, AstKind::StructInit { name, fields: first_field_idx, field_count })
            }
            AstKind::Conditional { cond, true_expr, false_expr } => {
                let cond_idx = self.clone_subtree(cond);
                let true_idx = self.clone_subtree(true_expr);
                let false_idx = self.clone_subtree(false_expr);
                self.push_copy(line, col, AstKind::Conditional { cond: cond_idx, true_expr: true_idx, false_expr: false_idx })
            }
            AstKind::Block { exprs, expr_count } => {
                if expr_count == 0 || exprs == NO_IDX {
                    return self.push_copy(line, col, AstKind::Block { exprs: NO_IDX, expr_count: 0 });
                }
                let mut expr = exprs;
                let first_expr_idx = self.clone_subtree(expr);
                let mut last_expr_idx = first_expr_idx;
                expr = self.ast[expr].next;
                for _ in 1..expr_count {
                    let new_expr_idx = self.clone_subtree(expr);
                    self.ast[last_expr_idx].next = new_expr_idx;
                    last_expr_idx = new_expr_idx;
                    expr = self.ast[expr].next;
                }
                self.push_copy(line, col, AstKind::Block { exprs: first_expr_idx, expr_count })
            }
            AstKind::Name { name } => {
                self.push_copy(line, col, AstKind::Name { name })
            }
            AstKind::Call { name, args, arg_count } => {
                if arg_count == 0 || args == NO_IDX {
                    return self.push_copy(line, col, AstKind::Call { name, args: NO_IDX, arg_count: 0 });
                }
                let mut arg = args;
                let first_arg_idx = self.clone_subtree(arg);
                let mut last_arg_idx = first_arg_idx;
                arg = self.ast[arg].next;
                for _ in 1..arg_count {
                    let new_arg_idx = self.clone_subtree(arg);
                    self.ast[last_arg_idx].next = new_arg_idx;
                    last_arg_idx = new_arg_idx;
                    arg = self.ast[arg].next;
                }
                self.push_copy(line, col, AstKind::Call { name, args: first_arg_idx, arg_count })
            }
            AstKind::FieldInit { name, expr } => {
                let expr_idx = self.clone_subtree(expr);
                self.push_copy(line, col, AstKind::FieldInit { name, expr: expr_idx })
            }
            AstKind::FieldAccess { expr, name } => {
                let expr_idx = self.clone_subtree(expr);
                self.push_copy(line, col, AstKind::FieldAccess { expr: expr_idx, name })
            }
            AstKind::Unknown => {
                self.push_copy(line, col, AstKind::Unknown)
            }
        }
    }

    fn flatten_type(&self, ty: AstType<'a>) -> AstType<'a> {
        match ty {
            AstType::Return(i) => self.flatten_type(self.fn_return_types[i]),
            _ => ty,
        }
    }

    fn report_error(&self, site: usize, msg: impl std::fmt::Display) {
        let node = &self.ast[site];
        eprintln!("Type error at line {}, column {}: {}", node.line, node.column, msg);
    }

    fn expect_same_type(&mut self, a: AstType<'a>, b: AstType<'a>, site: usize) {
        // Defer type mismatch errors if either type is unresolved return
        if matches!(a, AstType::Return(_)) || matches!(b, AstType::Return(_)) {
            self.deferred_type_checks.push((a, b, site));
            return;
        }
        let (af, bf) = (self.flatten_type(a), self.flatten_type(b));
        if af != bf {
            self.report_error(site, format!("type mismatch: expected {:?}, found {:?}", af, bf));
        }
    }

    fn infer(&mut self, index: usize, env: &mut HashMap<&'a str, AstType<'a>>) {
        match self.ast[index].kind {
            AstKind::Integer { .. } => {
                self.ast[index].ty = AstType::Int;
            }
            AstKind::Float { .. } => {
                self.ast[index].ty = AstType::Float;
            }
            AstKind::Bool { .. } => {
                self.ast[index].ty = AstType::Bool;
            }
            AstKind::String { cooked_len, .. } => {
                let mut dims = [usize::MAX; 8];
                dims[0] = cooked_len + 1;
                self.ast[index].ty = AstType::Array { base: "byte", dims };
            }
            AstKind::Void {} => {
                self.ast[index].ty = AstType::Void;
            }
            AstKind::Unary { op: _, expr } => {
                self.infer(expr, env);
                self.ast[index].ty = self.ast[expr].ty;
            }
            AstKind::Binary { op, left, right } => {
                self.infer(left, env);
                self.infer(right, env);
                match op {
                    TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash => {
                        self.expect_same_type(self.ast[left].ty, self.ast[right].ty, index);
                        self.ast[index].ty = self.ast[left].ty;
                    }
                    TokenKind::DoubleEqual | TokenKind::NotEqual | TokenKind::Less | TokenKind::Greater | TokenKind::LessEqual | TokenKind::GreaterEqual => {
                        self.expect_same_type(self.ast[left].ty, self.ast[right].ty, index);
                        self.ast[index].ty = AstType::Bool;
                    }
                    _ => {
                        self.report_error(index, format!("unknown binary operator: {:?}", op));
                        self.ast[index].ty = AstType::Unknown;
                    }
                }
            }
            AstKind::Assignment { name, expr } => {
                self.infer(expr, env);
                let expr_type = self.ast[expr].ty;
                env.insert(name, expr_type);
                self.ast[index].ty = expr_type;
            }
            AstKind::ArrayInit { elems, elem_count } => {
                if elem_count == 0 || elems == NO_IDX {
                    self.report_error(index, "cannot infer type of empty array");
                    self.ast[index].ty = AstType::Unknown;
                    return;
                }

                let mut elem = elems;
                self.infer(elem, env);
                let mut first_ty = self.flatten_type(self.ast[elem].ty);
                for _ in 1..elem_count {
                    elem = self.ast[elem].next;
                    self.infer(elem, env);
                    self.expect_same_type(first_ty, self.ast[elem].ty, elem);
                    first_ty = self.flatten_type(self.ast[elem].ty);
                }

                let mut dims = [usize::MAX; 8];
                dims[0] = elem_count;

                self.ast[index].ty = match first_ty {
                    // For nested array initialization, propagate inner dimensions
                    AstType::Array { base, dims: inner } => {
                        let mut di = 1;
                        for &d in inner.iter() {
                            if d == usize::MAX || di >= dims.len() { break; }
                            dims[di] = d;
                            di += 1;
                        }
                        AstType::Array { base, dims }
                    }
                    AstType::Int => { AstType::Array { base: "int", dims } }
                    AstType::Float => { AstType::Array { base: "float", dims } }
                    AstType::Bool => { AstType::Array { base: "bool", dims } }
                    AstType::Byte => { AstType::Array { base: "byte", dims } }
                    AstType::Void => { AstType::Array { base: "void", dims } }
                    AstType::Struct(name) => { AstType::Array { base: name, dims } }
                    _ => {
                        self.report_error(index, format!("invalid array element type: {:?}", first_ty));
                        AstType::Unknown
                    }
                };
            }
            AstKind::ArrayAccess { expr, index_expr } => {
                self.infer(expr, env);
                self.infer(index_expr, env);
                self.expect_same_type(self.ast[index_expr].ty, AstType::Int, index_expr);
                let expr_type = self.flatten_type(self.ast[expr].ty);

                match expr_type {
                    AstType::Array { base, dims } => {
                        if dims[0] == usize::MAX {
                            self.report_error(index, "array has no first dimension");
                            self.ast[index].ty = AstType::Unknown;
                        } else if dims[1] == usize::MAX {
                            self.ast[index].ty = match base {
                                "int" => AstType::Int,
                                "float" => AstType::Float,
                                "bool" => AstType::Bool,
                                "byte" => AstType::Byte,
                                "void" => AstType::Void,
                                other => AstType::Struct(other),
                            };
                        } else {
                            let mut new_dims = [usize::MAX; 8];
                            let mut di = 0;
                            for i in 1..dims.len() {
                                if dims[i] == usize::MAX { break; }
                                new_dims[di] = dims[i];
                                di += 1;
                            }
                            self.ast[index].ty = AstType::Array { base, dims: new_dims };
                        }
                    }
                    _ => {
                        self.report_error(index, format!("attempted index access on non-array type: {:?}", expr_type));
                        self.ast[index].ty = AstType::Unknown;
                    }
                }
            }
            AstKind::StructInit { name, fields, field_count } => {
                if let Some(struct_decl) = self.struct_decls.get(name) {
                    let mut field = fields;

                    if field_count != struct_decl.fields.len() {
                        self.report_error(index, format!("field count mismatch in struct {}: expected {}, found {}", name, struct_decl.fields.len(), field_count));
                        return;
                    }

                    for i in 0..field_count {
                        self.infer(field, env);

                        let AstKind::FieldInit { name: field_name, .. } = self.ast[field].kind else { unreachable!() };
                        let decl_field_name = struct_decl.fields[i as usize].0;

                        if field_name != "" && field_name != decl_field_name {
                            self.report_error(field, format!("unexpected field '{}' in struct '{}'", field_name, name));
                        }

                        field = self.ast[field].next;
                    }
                    self.ast[index].ty = AstType::Struct(name);
                }
                else {
                    self.report_error(index, format!("undefined struct: {}", name));
                    self.ast[index].ty = AstType::Unknown;
                }
            }
            AstKind::Conditional { cond, true_expr, false_expr } => {
                self.infer(cond, env);
                self.expect_same_type(self.ast[cond].ty, AstType::Bool, cond);

                self.infer(true_expr, env);
                self.infer(false_expr, env);
                self.expect_same_type(self.ast[true_expr].ty, self.ast[false_expr].ty, true_expr);
                self.ast[index].ty = self.ast[true_expr].ty;
            }
            AstKind::Block { exprs, expr_count } => {
                let mut scoped_env = env.clone();
                let mut expr = exprs;
                let mut last_type = AstType::Unknown;
                for _ in 0..expr_count {
                    self.infer(expr, &mut scoped_env);
                    last_type = self.ast[expr].ty;
                    expr = self.ast[expr].next;
                }
                self.ast[index].ty = last_type;
            }
            AstKind::Name { name } => {
                if let Some(var_type) = env.get(name) {
                    self.ast[index].ty = *var_type;
                } else {
                    self.ast[index].ty = AstType::Unknown;
                    self.report_error(index, format!("undefined variable: {}", name));
                }
            }
            AstKind::Call { name, args, arg_count } => {
                let fn_decl = self.fn_decls.get(name);

                if let Some(FunctionDecl { name, params, body }) = fn_decl {
                    if params.len() != arg_count {
                        self.report_error(index, format!("argument count mismatch in function {}: expected {}, found {}", name, params.len(), arg_count));
                        self.ast[index].ty = AstType::Unknown;
                        return;
                    }

                    let mut fn_env = env.clone();
                    let mut arg = args;
                    let mut mono_arg_types = [AstType::Unknown; 32];
                    for i in 0..arg_count {
                        self.infer(arg, &mut fn_env);
                        let arg_type = self.flatten_type(self.ast[arg].ty);

                        fn_env.insert(params[i], arg_type);

                        mono_arg_types[i as usize] = arg_type;
                        arg = self.ast[arg].next;
                    }

                    let instantiation_key = FnKey {
                        name,
                        param_types: mono_arg_types,
                    };

                    // Check if we already have a monomorphized instance, functions are uniquely identified by their name and argument types
                    if self.fn_instances.contains_key(&instantiation_key) {
                        let return_idx = self.fn_return_indices[&instantiation_key];
                        self.ast[index].ty = AstType::Return(return_idx);
                    }
                    else {
                        // Important to insert before inferring to handle recursion
                        let mono_idx = self.clone_subtree(*body);
                        let return_idx = self.fn_return_types.len();
                        self.fn_return_types.push(AstType::Unknown);

                        self.fn_instances.insert(instantiation_key, mono_idx);
                        self.fn_return_indices.insert(instantiation_key, return_idx);
                        self.ast[index].ty = AstType::Return(return_idx);

                        self.infer(mono_idx, &mut fn_env);

                        self.fn_return_types[return_idx] = self.flatten_type(self.ast[mono_idx].ty);
                    }
                }
                else if let Some(ext) = self.extern_fns.get(name) {
                    if ext.param_types.len() != arg_count {
                        self.report_error(index, format!("argument count mismatch in extern function {}: expected {}, found {}", name, ext.param_types.len(), arg_count));
                        self.ast[index].ty = AstType::Unknown;
                        return;
                    }
                    let mut arg = args;
                    for expected in ext.param_types.iter() {
                        self.infer(arg, env);
                        let actual = self.ast[arg].ty;
                        match (actual, *expected) {
                            // Allow decay from array to pointer
                            (AstType::Array { base, .. }, AstType::Ptr(exp_base)) if base == exp_base => {}
                            _ => self.expect_same_type(actual, *expected, arg),
                        }
                        arg = self.ast[arg].next;
                    }
                    self.ast[index].ty = ext.return_type;
                }
                else {
                    self.report_error(index, format!("undefined function: {}", name));
                    self.ast[index].ty = AstType::Unknown;
                }
            }
            AstKind::FieldInit { name: _, expr } => {
                self.infer(expr, env);
                self.ast[index].ty = self.flatten_type(self.ast[expr].ty);
            }
            AstKind::FieldAccess { expr, name } => {
                self.infer(expr, env);
                let expr_type = self.flatten_type(self.ast[expr].ty);
                if let AstType::Struct(struct_name) = expr_type {
                    if let Some(struct_decl) = self.struct_decls.get(struct_name) {
                        if let Some((_, field_type)) = struct_decl.fields.iter().find(|(field_name, _)| *field_name == name) {
                            self.ast[index].ty = *field_type;
                        }
                        else {
                            self.report_error(index, format!("struct '{}' has no field named '{}'", struct_name, name));
                            self.ast[index].ty = AstType::Unknown;
                        }
                    }
                    else {
                        self.report_error(index, format!("undefined struct: {}", struct_name));
                        self.ast[index].ty = AstType::Unknown;
                    }
                }
                else {
                    self.report_error(index, format!("attempted field access on non-struct type: {:?}", expr_type));
                    self.ast[index].ty = AstType::Unknown;
                }
            }
            AstKind::Unknown => {
                self.ast[index].ty = AstType::Unknown;
            }
        }
    }
}

pub fn infer_types<'a>(ast: Vec<AstNode<'a>>, struct_decls: &'a HashMap<&'a str, StructDecl<'a>>, fn_decls: &'a HashMap<&'a str, FunctionDecl<'a>>, extern_fns: &'a HashMap<&'a str, ExternFunctionDecl<'a>>) -> (Vec<AstNode<'a>>, HashMap<FnKey<'a>, usize>) {
    let mut typer = Typer { ast, struct_decls, fn_decls, extern_fns, fn_instances: HashMap::new(), fn_return_indices: HashMap::new(), fn_return_types: Vec::new(), deferred_type_checks: Vec::new() };
    typer.infer(0, &mut HashMap::new());

    // Flatten return types, necessary because mutual recursion may leave some return types as AstType::Return
    for i in 0..typer.fn_return_types.len() {
        typer.fn_return_types[i] = typer.flatten_type( AstType::Return(i) );
    }

    // Perform deferred type checks, these are checks that were deferred due to unresolved return types
    for i in 0..typer.deferred_type_checks.len() {
        let (a, b, site) = typer.deferred_type_checks[i];
        let (a, b) = (typer.flatten_type(a), typer.flatten_type(b));
        if a != b {
            typer.report_error(site, format!("type mismatch: expected {:?}, found {:?}", a, b));
        }
    }

    // Resolve return types in the AST
    for node in &mut typer.ast {
        if let AstType::Return(ret_idx) = node.ty {
            node.ty = typer.fn_return_types[ret_idx];
        }
    }

    (typer.ast, typer.fn_instances)
}