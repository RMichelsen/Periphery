use crate::lexer::TokenKind;
use crate::parser::{
    AstKind, AstNode, AstType, AstTypeSuffix, AstValue, ExternFunctionDecl, FunctionDecl,
    StructDecl,
};
use crate::typer::FnKey;

use std::collections::HashMap;

struct LabelState {
    next_reg: u64,
    next_bb: u64,
    current_bb: u64,
}

struct StringLiteral<'a> {
    raw: &'a str,
    cooked_len: usize,
}

struct Emitter<'a> {
    ast: Vec<AstNode<'a>>,
    fn_decls: &'a HashMap<&'a str, FunctionDecl<'a>>,
    struct_decls: &'a HashMap<&'a str, StructDecl<'a>>,
    extern_fns: &'a HashMap<&'a str, ExternFunctionDecl<'a>>,
    code: String,
    labels: LabelState,
    string_literals: Vec<StringLiteral<'a>>,
}

impl<'a> Emitter<'a> {
    fn report_error(&self, site: usize, msg: impl std::fmt::Display) {
        let node = &self.ast[site];
        eprintln!(
            "Codegen error at line {}, column {}: {}",
            node.line, node.column, msg
        );
    }

    fn next_reg(&mut self) -> u64 {
        let reg = self.labels.next_reg;
        self.labels.next_reg += 1;
        reg
    }

    fn next_bb(&mut self) -> u64 {
        let bb = self.labels.next_bb;
        self.labels.next_bb += 1;
        bb
    }

    fn emit_structs(&mut self) {
        use std::fmt::Write;
        for (name, struct_decl) in self.struct_decls.iter() {
            writeln!(&mut self.code, "%{} = type {{ ", name).unwrap();
            for (i, (_, field_ty)) in struct_decl.fields.iter().enumerate() {
                write!(&mut self.code, "  {}", (field_ty)).unwrap();
                if i + 1 < struct_decl.fields.len() {
                    writeln!(&mut self.code, ",").unwrap();
                }
            }
            writeln!(&mut self.code, "}}").unwrap();
        }
    }

    fn emit_externs(&mut self) {
        use std::fmt::Write;
        for (name, ext) in self.extern_fns.iter() {
            write!(&mut self.code, "declare {} @{}(", ext.return_type, name).unwrap();
            for (i, ty) in ext.param_types.iter().enumerate() {
                write!(
                    &mut self.code,
                    "{}{}",
                    ty,
                    if i + 1 < ext.param_types.len() {
                        ", "
                    } else {
                        ""
                    }
                )
                .unwrap();
            }
            writeln!(&mut self.code, ")").unwrap();
        }
    }

    fn emit_string_literals(&mut self) {
        use std::fmt::Write;
        let literal_count = self.string_literals.len();
        for i in 0..literal_count {
            let (raw, cooked_len) = {
                let lit = &self.string_literals[i];
                (lit.raw, lit.cooked_len)
            };
            let total_len = cooked_len + 1;
            write!(
                &mut self.code,
                "@.str{} = private constant [{} x i8] c\"",
                i, total_len
            )
            .unwrap();
            Self::write_string_bytes(&mut self.code, raw);
            writeln!(&mut self.code, "\\00\"").unwrap();
        }
    }

    fn write_string_bytes(code: &mut String, raw: &str) {
        use std::fmt::Write;
        let bytes = raw.as_bytes();
        let mut i = 0usize;
        while i < bytes.len() {
            let b = if bytes[i] == b'\\' && i + 1 < bytes.len() {
                let esc = bytes[i + 1];
                i += 2;
                match esc {
                    b'n' => b'\n',
                    b'"' => b'"',
                    b'\\' => b'\\',
                    other => other,
                }
            } else {
                let plain = bytes[i];
                i += 1;
                plain
            };
            match b {
                b'\\' => write!(code, "\\5C").unwrap(),
                b'\"' => write!(code, "\\22").unwrap(),
                b'\n' => write!(code, "\\0A").unwrap(),
                0 => write!(code, "\\00").unwrap(),
                32..=126 => write!(code, "{}", b as char).unwrap(),
                _ => write!(code, "\\{:02X}", b).unwrap(),
            }
        }
    }

    fn add_string_literal(&mut self, raw: &'a str, cooked_len: usize) -> usize {
        let id = self.string_literals.len();
        self.string_literals.push(StringLiteral { raw, cooked_len });
        id
    }

    fn emit(&mut self, index: usize, env: &mut HashMap<&'a str, AstValue>) {
        use std::fmt::Write;
        match self.ast[index].kind {
            AstKind::Integer { value } => {
                self.ast[index].ssa = AstValue::Int(value);
            }
            AstKind::Float { value } => {
                self.ast[index].ssa = AstValue::Float(value);
            }
            AstKind::Bool { value } => {
                self.ast[index].ssa = AstValue::Bool(value);
            }
            AstKind::String { value, cooked_len } => {
                let id = self.add_string_literal(value, cooked_len);
                let reg = self.next_reg();
                let n = self.string_literals[id].cooked_len + 1;
                writeln!(
                    &mut self.code,
                    "%{} = getelementptr inbounds [{} x i8], [{} x i8]* @.str{}, i64 0, i64 0",
                    reg, n, n, id
                )
                .unwrap();
                self.ast[index].ssa = AstValue::Register(reg);
                self.ast[index].ty = AstType::Ptr("byte");
            }
            AstKind::Void {} => {}
            AstKind::Unary { op, expr } => {
                self.emit(expr, env);

                let reg = self.next_reg();

                if op == TokenKind::Exclamation {
                    writeln!(
                        &mut self.code,
                        "%{} = xor i1 {}, 1",
                        reg, self.ast[expr].ssa
                    )
                    .unwrap();
                } else if op == TokenKind::Minus {
                    if self.ast[expr].ty == AstType::Int {
                        writeln!(
                            &mut self.code,
                            "%{} = sub i64 0, {}",
                            reg, self.ast[expr].ssa
                        )
                        .unwrap();
                    } else if self.ast[expr].ty == AstType::Float {
                        writeln!(
                            &mut self.code,
                            "%{} = fneg double {}",
                            reg, self.ast[expr].ssa
                        )
                        .unwrap();
                    } else {
                        self.report_error(expr, format!("unsupported unary operator {:?}", op));
                        return;
                    }
                }
                self.ast[index].ssa = AstValue::Register(reg);
            }
            AstKind::Binary { op, left, right } => {
                self.emit(left, env);
                self.emit(right, env);

                let reg = self.next_reg();

                let floating = self.ast[left].ty == AstType::Float;
                let opcode = match op {
                    TokenKind::Plus => {
                        if floating {
                            "fadd"
                        } else {
                            "add"
                        }
                    }
                    TokenKind::Minus => {
                        if floating {
                            "fsub"
                        } else {
                            "sub"
                        }
                    }
                    TokenKind::Star => {
                        if floating {
                            "fmul"
                        } else {
                            "mul"
                        }
                    }
                    TokenKind::Slash => {
                        if floating {
                            "fdiv"
                        } else {
                            "sdiv"
                        }
                    }
                    TokenKind::DoubleEqual => {
                        if floating {
                            "fcmp oeq"
                        } else {
                            "icmp eq"
                        }
                    }
                    TokenKind::NotEqual => {
                        if floating {
                            "fcmp one"
                        } else {
                            "icmp ne"
                        }
                    }
                    TokenKind::Greater => {
                        if floating {
                            "fcmp ogt"
                        } else {
                            "icmp sgt"
                        }
                    }
                    TokenKind::GreaterEqual => {
                        if floating {
                            "fcmp oge"
                        } else {
                            "icmp sge"
                        }
                    }
                    TokenKind::Less => {
                        if floating {
                            "fcmp olt"
                        } else {
                            "icmp slt"
                        }
                    }
                    TokenKind::LessEqual => {
                        if floating {
                            "fcmp ole"
                        } else {
                            "icmp sle"
                        }
                    }
                    _ => {
                        self.report_error(index, format!("unsupported binary operator {:?}", op));
                        return;
                    }
                };
                writeln!(
                    &mut self.code,
                    "%{} = {} {} {}, {}",
                    reg, opcode, self.ast[left].ty, self.ast[left].ssa, self.ast[right].ssa
                )
                .unwrap();
                self.ast[index].ssa = AstValue::Register(reg);
            }
            AstKind::Assignment { name, expr } => {
                self.emit(expr, env);
                self.ast[index].ssa = self.ast[expr].ssa;
                env.insert(name, self.ast[expr].ssa);
            }
            AstKind::ArrayInit { elems, elem_count } => {
                if elem_count == 0 {
                    return;
                }

                let mut elem = elems;
                let mut elem_reg = u64::MAX;
                let mut prev_elem_reg = u64::MAX;
                for i in 0..elem_count {
                    self.emit(elem, env);
                    elem_reg = self.next_reg();
                    if i == 0 {
                        writeln!(
                            &mut self.code,
                            "%{} = insertvalue [{} x {}] undef, {} {}, {}",
                            elem_reg,
                            elem_count,
                            self.ast[elem].ty,
                            self.ast[elem].ty,
                            self.ast[elem].ssa,
                            i
                        )
                        .unwrap();
                    } else {
                        writeln!(
                            &mut self.code,
                            "%{} = insertvalue [{} x {}] %{}, {} {}, {}",
                            elem_reg,
                            elem_count,
                            self.ast[elem].ty,
                            prev_elem_reg,
                            self.ast[elem].ty,
                            self.ast[elem].ssa,
                            i
                        )
                        .unwrap();
                    }

                    if i < elem_count - 1 {
                        elem = self.ast[elem].next;
                        prev_elem_reg = elem_reg;
                        elem_reg = self.next_reg();
                    }
                }
                self.ast[index].ssa = AstValue::Register(elem_reg);
            }
            AstKind::ArrayAccess { expr, index_expr } => {
                self.emit(expr, env);
                self.emit(index_expr, env);

                let elem_reg = self.next_reg();
                writeln!(
                    &mut self.code,
                    "%{} = extractvalue {} {}, {}",
                    elem_reg, self.ast[expr].ty, self.ast[expr].ssa, self.ast[index_expr].ssa
                )
                .unwrap();
                self.ast[index].ssa = AstValue::Register(elem_reg);
            }
            AstKind::StructInit {
                name,
                fields,
                field_count,
            } => {
                if field_count == 0 {
                    return;
                }
                let mut field = fields;
                let mut field_reg = u64::MAX;
                let mut prev_field_reg = u64::MAX;
                for i in 0..field_count {
                    self.emit(field, env);
                    field_reg = self.next_reg();
                    if i == 0 {
                        writeln!(
                            &mut self.code,
                            "%{} = insertvalue %{} undef, {} {}, {}",
                            field_reg, name, self.ast[field].ty, self.ast[field].ssa, i
                        )
                        .unwrap();
                    } else {
                        writeln!(
                            &mut self.code,
                            "%{} = insertvalue %{} %{}, {} {}, {}",
                            field_reg,
                            name,
                            prev_field_reg,
                            self.ast[field].ty,
                            self.ast[field].ssa,
                            i
                        )
                        .unwrap();
                    }

                    if i < field_count - 1 {
                        field = self.ast[field].next;
                        prev_field_reg = field_reg;
                        field_reg = self.next_reg();
                    }
                }
                self.ast[index].ssa = AstValue::Register(field_reg);
            }
            AstKind::Conditional {
                cond,
                true_expr,
                false_expr,
            } => {
                let bb_true = self.next_bb();
                let bb_false = self.next_bb();
                let bb_end = self.next_bb();

                self.emit(cond, env);
                writeln!(
                    &mut self.code,
                    "br i1 {}, label %bb_{}, label %bb_{}",
                    self.ast[cond].ssa, bb_true, bb_false
                )
                .unwrap();

                self.labels.current_bb = bb_true;
                writeln!(&mut self.code, "bb_{}:", bb_true).unwrap();
                self.emit(true_expr, env);
                writeln!(&mut self.code, "br label %bb_{}", bb_end).unwrap();
                let true_exit = self.labels.current_bb;

                self.labels.current_bb = bb_false;
                writeln!(&mut self.code, "bb_{}:", bb_false).unwrap();
                self.emit(false_expr, env);
                writeln!(&mut self.code, "br label %bb_{}", bb_end).unwrap();
                let false_exit = self.labels.current_bb;

                let phi = self.next_reg();
                self.labels.current_bb = bb_end;
                writeln!(
                    &mut self.code,
                    "bb_{}:\n%{} = phi {} [ {}, %bb_{} ], [ {}, %bb_{} ]",
                    bb_end,
                    phi,
                    self.ast[true_expr].ty,
                    self.ast[true_expr].ssa,
                    true_exit,
                    self.ast[false_expr].ssa,
                    false_exit
                )
                .unwrap();
                self.ast[index].ssa = AstValue::Register(phi);
            }
            AstKind::Block { exprs, expr_count } => {
                let mut scoped_env = env.clone();
                let mut expr = exprs;
                for i in 0..expr_count {
                    self.emit(expr, &mut scoped_env);
                    if i == expr_count - 1 {
                        self.ast[index].ssa = self.ast[expr].ssa;
                    }
                    expr = self.ast[expr].next;
                }
            }
            AstKind::Call {
                name,
                args,
                arg_count,
            } => {
                if name == "main" {
                    return;
                }

                let mut arg = args;
                for _ in 0..arg_count {
                    self.emit(arg, env);
                    arg = self.ast[arg].next;
                }

                let reg = self.next_reg();

                // Extern calls use raw symbol name (no monomorphization suffixes)
                let is_extern = self.extern_fns.contains_key(name);
                write!(
                    &mut self.code,
                    "%{} = call {} @{}",
                    reg, self.ast[index].ty, name
                )
                .unwrap();
                if !is_extern {
                    arg = args;
                    for _ in 0..arg_count {
                        write!(&mut self.code, "_{}", AstTypeSuffix(self.ast[arg].ty)).unwrap();
                        arg = self.ast[arg].next;
                    }
                }

                write!(&mut self.code, "(").unwrap();
                arg = args;
                for i in 0..arg_count {
                    write!(
                        &mut self.code,
                        "{} {}{}",
                        self.ast[arg].ty,
                        self.ast[arg].ssa,
                        if i + 1 < arg_count { ", " } else { "" }
                    )
                    .unwrap();
                    arg = self.ast[arg].next;
                }
                writeln!(&mut self.code, ")").unwrap();
                self.ast[index].ssa = AstValue::Register(reg);
            }
            AstKind::Name { name } => {
                if let Some(binding) = env.get(name) {
                    self.ast[index].ssa = *binding;
                } else {
                    self.report_error(index, format!("undefined variable '{}'", name));
                }
            }
            AstKind::FieldInit { expr, .. } => {
                self.emit(expr, env);
                self.ast[index].ssa = self.ast[expr].ssa;
            }
            AstKind::FieldAccess { expr, name } => {
                self.emit(expr, env);

                let struct_type = self.ast[expr].ty;
                if let AstType::Struct(struct_name) = struct_type {
                    if let Some(struct_decl) = self.struct_decls.get(struct_name) {
                        if let Some(field_index) = struct_decl
                            .fields
                            .iter()
                            .position(|(field_name, _)| *field_name == name)
                        {
                            let field_reg = self.next_reg();
                            writeln!(
                                &mut self.code,
                                "%{} = extractvalue %{} {}, {}",
                                field_reg, struct_name, self.ast[expr].ssa, field_index
                            )
                            .unwrap();
                            self.ast[index].ssa = AstValue::Register(field_reg);
                        }
                    } else {
                        self.report_error(index, format!("struct '{}' not declared", struct_name));
                    }
                } else {
                    self.report_error(
                        expr,
                        format!(
                            "attempting to access field '{}' of non-struct type {:?}",
                            name, struct_type
                        ),
                    );
                }
            }
            _ => todo!(),
        }
    }

    fn emit_fn(&mut self, FnKey { name, param_types }: FnKey, index: usize) {
        self.labels = LabelState {
            next_reg: 0,
            next_bb: 0,
            current_bb: 0,
        };

        use std::fmt::Write;
        write!(&mut self.code, "define {} @{}", self.ast[index].ty, name).unwrap();
        for ty in param_types.iter() {
            if *ty == AstType::Unknown {
                break;
            }
            write!(&mut self.code, "_{}", AstTypeSuffix(*ty)).unwrap();
        }
        write!(&mut self.code, "(").unwrap();

        let fn_decl = self.fn_decls.get(name);

        if let Some(FunctionDecl {
            name: _, params, ..
        }) = fn_decl
        {
            let mut env: HashMap<&'a str, AstValue> = HashMap::new();
            for (i, param) in params.iter().enumerate() {
                let reg = self.next_reg();
                write!(
                    &mut self.code,
                    "{} %{}{}",
                    param_types[i as usize],
                    reg,
                    if i + 1 < params.len() { ", " } else { "" }
                )
                .unwrap();
                env.insert(*param, AstValue::Register(reg));
            }

            writeln!(&mut self.code, ") nounwind {{\nentry:").unwrap();

            self.emit(index, &mut env);

            writeln!(
                &mut self.code,
                "ret {} {}\n}}\n",
                self.ast[index].ty, self.ast[index].ssa
            )
            .unwrap();
        } else {
            self.report_error(index, format!("function '{}' not declared", name));
        }
    }
}

pub fn emit<'a>(
    ast: Vec<AstNode<'a>>,
    fn_instances: HashMap<FnKey<'a>, usize>,
    fn_decls: &'a HashMap<&'a str, FunctionDecl<'a>>,
    struct_decls: &'a HashMap<&'a str, StructDecl<'a>>,
    extern_fns: &'a HashMap<&'a str, ExternFunctionDecl<'a>>,
) -> String {
    let mut emitter = Emitter {
        ast,
        fn_decls,
        struct_decls,
        extern_fns,
        code: String::new(),
        labels: LabelState {
            next_reg: 0,
            next_bb: 0,
            current_bb: 0,
        },
        string_literals: Vec::new(),
    };
    emitter.emit_structs();
    emitter.emit_externs();
    // First walk root and functions to collect string literals before emitting their globals.
    emitter.emit(0, &mut HashMap::new());
    for (key, index) in fn_instances.iter() {
        emitter.emit_fn(*key, *index);
    }
    // Now emit string literals after collection so referenced @.strN exist.
    emitter.emit_string_literals();
    emitter.code
}
