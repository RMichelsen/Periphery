use crate::lexer::{Token, TokenKind};

use std::collections::HashMap;
use std::fmt;

pub const NO_IDX: usize = usize::MAX;

#[derive(Debug)]
pub struct StructDecl<'a> {
    pub name: &'a str,
    pub fields: Vec<(&'a str, AstType<'a>)>,
}

#[derive(Debug)]
pub struct FunctionDecl<'a> {
    pub name: &'a str,
    pub params: Vec<&'a str>,
    pub body: usize,
}

#[derive(Debug)]
pub struct ExternFunctionDecl<'a> {
    pub name: &'a str,
    pub param_types: Vec<AstType<'a>>,
    pub return_type: AstType<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub enum AstType<'a> {
    Int,
    Float,
    Bool,
    Byte,
    Void,
    Struct(&'a str),
    Array { base: &'a str, dims: [usize; 8] },
    Ptr(&'a str),
    Return(usize),
    Unknown,
}

impl<'a> fmt::Display for AstType<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AstType::Int    => fmt.write_str("i64"),
            AstType::Float  => fmt.write_str("double"),
            AstType::Bool   => fmt.write_str("i1"),
            AstType::Byte   => fmt.write_str("i8"),
            AstType::Void   => fmt.write_str("void"),
            AstType::Struct(name) => {
                fmt.write_str("%")?;
                fmt.write_str(name)
            }
            AstType::Array { base, dims } => {
                let mut depth = 0;
                while depth < dims.len() && dims[depth] != usize::MAX { depth += 1; }
                for i in 0..depth { write!(fmt, "[{} x ", dims[i])?; }
                match *base {
                    "int" => fmt.write_str("i64")?,
                    "float" => fmt.write_str("double")?,
                    "bool" => fmt.write_str("i1")?,
                    "byte" => fmt.write_str("i8")?,
                    "void" => fmt.write_str("void")?,
                    other => { write!(fmt, "%{}", other)?; }
                }
                for _ in 0..depth { fmt.write_str("]")?; }
                return Ok(());
            }
            AstType::Ptr(base) => {
                match *base {
                    "int" => write!(fmt, "i64*")?,
                    "float" => write!(fmt, "double*")?,
                    "bool" => write!(fmt, "i1*")?,
                    "byte" => write!(fmt, "i8*")?,
                    "void" => write!(fmt, "void*")?,
                    other => write!(fmt, "%{}*", other)?,
                }
                return Ok(());
            }
            AstType::Return(_) | AstType::Unknown => fmt.write_str("UnknownType")
        }
    }
}

pub struct AstTypeSuffix<'a>(pub AstType<'a>);
impl<'a> fmt::Display for AstTypeSuffix<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            AstType::Int => write!(f, "int"),
            AstType::Float => write!(f, "float"),
            AstType::Bool => write!(f, "bool"),
            AstType::Byte => write!(f, "byte"),
            AstType::Void => write!(f, "void"),
            AstType::Struct(name) => write!(f, "struct_{}", name),
            AstType::Array { base, dims } => {
                write!(f, "arr_{}", base)?;
                let mut depth = 0;
                while depth < dims.len() && dims[depth] != usize::MAX { depth += 1; }
                for i in 0..depth { write!(f, "_{}", dims[i])?; }
                Ok(())
            }
            AstType::Ptr(base) => write!(f, "ptr_{}", base),
            AstType::Return(_) | AstType::Unknown => write!(f, "unknown")
        }
    }
}

#[derive(Debug)]
pub enum AstKind<'a> {
    Integer { value: i64 },
    Float { value: f64 },
    Bool { value: bool },
    Byte { value: u8 },
    String { value: &'a str, cooked_len: usize },
    Void {},
    Unary { op: TokenKind, expr: usize },
    Binary { op: TokenKind, left: usize, right: usize },
    Assignment { name: &'a str, expr: usize },
    ArrayInit { elems: usize, elem_count: usize },
    ArrayAccess { expr: usize, index_expr: usize },
    StructInit { name: &'a str, fields: usize, field_count: usize },
    Conditional { cond: usize, true_expr: usize, false_expr: usize },
    Block { exprs: usize, expr_count: usize },
    Name { name: &'a str },
    Call { name: &'a str, args: usize, arg_count: usize },
    FieldInit { name: &'a str, expr: usize },
    FieldAccess { expr: usize, name: &'a str },
    Unknown,
}

#[derive(Debug, Copy, Clone)]
pub enum AstValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    Byte(u8),
    Register(u64),
    Void,
}

impl fmt::Display for AstValue {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AstValue::Int(value) => write!(fmt, "{}", value),
            AstValue::Float(value) => write!(fmt, "{:?}", value),
            AstValue::Bool(value) => write!(fmt, "{}", value),
            AstValue::Byte(value) => write!(fmt, "{}", value),
            AstValue::Register(reg) => write!(fmt, "%{}", reg),
            AstValue::Void => write!(fmt, "void"),
        }
    }
}

#[derive(Debug)]
pub struct AstNode<'a> {
    pub line: u32,
    pub column: u32,
    pub next: usize,
    pub kind: AstKind<'a>,
    pub ty: AstType<'a>,
    pub ssa: AstValue,
}

struct Parser<'a> {
    tokens: &'a [Token<'a>],
    index: usize,
    ast: Vec<AstNode<'a>>,
    struct_decls: HashMap<&'a str, StructDecl<'a>>,
    fn_decls: HashMap<&'a str, FunctionDecl<'a>>,
    extern_fns: HashMap<&'a str, ExternFunctionDecl<'a>>,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token<'a>]) -> Self {
        Self { tokens, index: 0, ast: Vec::new(), struct_decls: HashMap::new(), fn_decls: HashMap::new(), extern_fns: HashMap::new() }
    }

    fn current(&self) -> &Token<'a> {
        &self.tokens[self.index]
    }

    fn peek(&self, kind: TokenKind) -> bool {
        self.index < self.tokens.len() && self.tokens[self.index].kind == kind
    }

    fn peek2(&self, k1: TokenKind, k2: TokenKind) -> bool {
        if self.index + 1 >= self.tokens.len() { return false; }
        self.tokens[self.index].kind == k1 && self.tokens[self.index + 1].kind == k2
    }

    fn match_kind(&mut self, kind: TokenKind) -> bool {
        if self.peek(kind) { self.index += 1; true } else { false }
    }

    fn match_any(&mut self, kinds: &[TokenKind]) -> Option<TokenKind> {
        let token = self.current();
        for &k in kinds { if token.kind == k { self.index += 1; return Some(k); } }
        None
    }

    fn expect(&mut self, kind: TokenKind) -> &Token<'a> {
        let token = &self.tokens[self.index];
        if token.kind != kind {
            self.error_expected(kind);
        }
        self.index += 1;
        token
    }

    fn report_error(&self, line: u32, col: u32, msg: impl std::fmt::Display) {
        eprintln!("Parse error at line {}, column {}: {}", line, col, msg);
    }

    fn error_expected(&self, expected: TokenKind) {
        let token = &self.tokens[self.index];
        let (line, col) = token.line_col;
        self.report_error(line, col, format!("expected {:?}, got {:?}", expected, token.kind));
    }

    fn push(&mut self, node: AstNode<'a>) -> usize {
        let idx = self.ast.len();
        self.ast.push(node);
        idx
    }

    fn push_kind(&mut self, line: u32, column: u32, kind: AstKind<'a>) -> usize {
        self.push(AstNode { line, column, next: NO_IDX, kind, ty: AstType::Unknown, ssa: AstValue::Void })
    }

    fn pos(&self) -> (u32, u32) {
        self.tokens[self.index].line_col
    }

    pub fn parse(&mut self) {
        let (line, col) = self.pos();

        let root = AstNode { line, column: col, next: NO_IDX, kind: AstKind::Unknown, ty: AstType::Unknown, ssa: AstValue::Void };
        self.ast.push(root);

        let mut exprs = NO_IDX;
        let mut expr_count = 0;
        let mut last = NO_IDX;
        while !self.peek(TokenKind::Eof) {
            let expr = self.parse_expression();
            if last == NO_IDX {
                exprs = expr;
                last = expr;
            } else {
                self.ast[last].next = expr;
                last = expr;
            }

            expr_count += 1;
        }

        let call_to_main = self.push_kind(line, col, AstKind::Call { name: "main", args: NO_IDX, arg_count: 0 });
        if last == NO_IDX {
            exprs = call_to_main;
        } else {
            self.ast[last].next = call_to_main;
        }
        expr_count += 1;

        self.ast[0].kind = AstKind::Block { exprs, expr_count };
    }

    fn parse_expression(&mut self) -> usize {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> usize {
        // Extern function declaration: extern name(type, type, ...) -> type
        if self.match_kind(TokenKind::KwExtern) {
            let name_tok = self.expect(TokenKind::Identifier);
            let name = name_tok.lexeme;
            let (line, col) = name_tok.line_col;

            if self.extern_fns.contains_key(name) || self.fn_decls.contains_key(name) {
                self.report_error(line, col, format!("function '{}' already declared", name));
            }

            self.expect(TokenKind::LParen);
            let mut param_types = Vec::new();
            if !self.peek(TokenKind::RParen) {
                loop {
                    let param_ty = self.parse_type();
                    param_types.push(param_ty);
                    if !self.match_kind(TokenKind::Comma) { break; }
                }
            }
            self.expect(TokenKind::RParen);
            self.expect(TokenKind::Arrow);
            let return_type = self.parse_type();

            self.extern_fns.insert(name, ExternFunctionDecl { name, param_types, return_type });
            return self.push_kind(line, col, AstKind::Void {});
        }
        if self.match_kind(TokenKind::KwLet) {
            let name = self.expect(TokenKind::Identifier).lexeme;

            let (line, col) = self.pos();

            // Function definition without parameters
            if self.peek(TokenKind::LBrace) {
                let body = self.parse_expression();

                if self.fn_decls.contains_key(name) {
                    self.report_error(line, col, format!("function '{}' already declared", name));
                }
                else {
                    self.fn_decls.insert(name, FunctionDecl { name, params: Vec::new(), body });
                }
                self.push_kind(line, col, AstKind::Void {})
            }

            // Function definition with parameters
            else if self.peek(TokenKind::Identifier) {
                let mut params = Vec::new();
                while self.peek(TokenKind::Identifier) {
                    let param = self.expect(TokenKind::Identifier).lexeme;
                    params.push(param);
                }
                self.expect(TokenKind::Equal);
                let body = self.parse_expression();

                if self.fn_decls.contains_key(name) {
                    self.report_error(line, col, format!("function '{}' already declared", name));
                }
                else {
                    self.fn_decls.insert(name, FunctionDecl { name, params, body });
                }
                self.push_kind(line, col, AstKind::Void {})
            } 

            // Assignment
            else {
                self.expect(TokenKind::Equal);
                let expr = self.parse_expression();
                self.push_kind(line, col, AstKind::Assignment { name, expr })
            }
        } else {
            self.parse_condition()
        }
    }

    fn parse_type(&mut self) -> AstType<'a> {
        if self.match_kind(TokenKind::Star) {
            let base_ty = self.expect(TokenKind::Identifier).lexeme;
            return AstType::Ptr(base_ty);
        }

        let first = self.expect(TokenKind::Identifier).lexeme;
        let base_ty = first;
        let mut dims = [usize::MAX; 8];
        let mut dim_i = 0;
        while self.match_kind(TokenKind::LBracket) {
            dims[dim_i] = self.expect(TokenKind::Integer).lexeme.parse().unwrap();
            self.expect(TokenKind::RBracket);
            dim_i += 1;
        }
        if dims[0] != usize::MAX {
            AstType::Array { base: base_ty, dims }
        } else {
            match base_ty {
                "int" => AstType::Int,
                "float" => AstType::Float,
                "bool" => AstType::Bool,
                "byte" => AstType::Byte,
                "void" => AstType::Void,
                other => AstType::Struct(other)
            }
        }
    }


    fn parse_condition(&mut self) -> usize {
        let mut left = self.parse_equality();
        let (line, col) = self.pos();

        while let Some(op) = self.match_any(&[TokenKind::KwAnd, TokenKind::KwOr]) {
            if op == TokenKind::KwAnd {
                let false_idx = self.push_kind(line, col, AstKind::Bool { value: false });
                let true_expr = self.parse_condition();
                left = self.push_kind(line, col, AstKind::Conditional { cond: left, true_expr, false_expr: false_idx })
            } else if op == TokenKind::KwOr {
                let true_idx = self.push_kind(line, col, AstKind::Bool { value: true });
                let false_expr = self.parse_condition();
                left = self.push_kind(line, col, AstKind::Conditional { cond: left, true_expr: true_idx, false_expr })
            }
        }

        left
    }

    fn parse_left_assoc<F>(&mut self, sub: F, ops: &[TokenKind]) -> usize
    where
        F: Fn(&mut Parser<'a>) -> usize,
    {
        let mut left = sub(self);
        while let Some(op) = self.match_any(ops) {
            let (line, col) = self.pos();
            let right = sub(self);
            left = self.push_kind(line, col, AstKind::Binary { op, left, right });
        }
        left
    }

    fn parse_equality(&mut self) -> usize {
        self.parse_left_assoc(Self::parse_relation, &[TokenKind::DoubleEqual, TokenKind::NotEqual])
    }

    fn parse_relation(&mut self) -> usize {
        self.parse_left_assoc(Self::parse_term, &[TokenKind::Greater, TokenKind::GreaterEqual, TokenKind::Less, TokenKind::LessEqual])
    }

    fn parse_term(&mut self) -> usize {
        self.parse_left_assoc(Self::parse_factor, &[TokenKind::Plus, TokenKind::Minus])
    }

    fn parse_factor(&mut self) -> usize {
        self.parse_left_assoc(Self::parse_unary, &[TokenKind::Star, TokenKind::Slash])
    }

    fn parse_unary(&mut self) -> usize {
        if let Some(op) = self.match_any(&[TokenKind::Exclamation, TokenKind::Minus]) {
            let expr = self.parse_unary();
            let (line, col) = self.pos();
            return self.push_kind(line, col, AstKind::Unary { op, expr });
        }

        self.parse_initializer()
    }

    fn parse_initializer(&mut self) -> usize {
        // Struct initializer
        if self.peek2(TokenKind::Identifier, TokenKind::LBrace) {
            let name = self.expect(TokenKind::Identifier).lexeme;
            self.expect(TokenKind::LBrace);
            let (line, col) = self.pos();

            let mut last = NO_IDX;
            let mut fields = NO_IDX;
            let mut field_count = 0;
            if !self.peek(TokenKind::RBrace) {
                let named_initialization = self.peek2(TokenKind::Identifier, TokenKind::Colon);

                loop {
                    if self.peek(TokenKind::RBrace) {
                        let (tl, tc) = self.pos();
                        self.report_error(tl, tc, "trailing commas not allowed in struct initializers");
                    }

                    let (line, col) = self.pos();

                    let mut name = "";
                    if named_initialization {
                        name = self.expect(TokenKind::Identifier).lexeme;
                        self.expect(TokenKind::Colon);
                    }
                    let expr = self.parse_expression();
                    let field_idx = self.push_kind(line, col, AstKind::FieldInit { name, expr });

                    if last == NO_IDX {
                        fields = field_idx;
                        last = field_idx;
                    } else {
                        self.ast[last].next = field_idx;
                        last = field_idx;
                    }

                    field_count += 1;
                    if !self.match_kind(TokenKind::Comma) { break; }
                }
            }
            self.expect(TokenKind::RBrace);
            self.push_kind(line, col, AstKind::StructInit { name, fields, field_count })
        }

        // Array initializer
        else if self.match_kind(TokenKind::LBracket) {
            let (line, col) = self.pos();

            let mut elems = NO_IDX;
            let mut last  = NO_IDX;
            let mut elem_count = 0;

            if !self.peek(TokenKind::RBracket) {
                loop {
                    if self.peek(TokenKind::RBracket) {
                        let (tl, tc) = self.pos();
                        self.report_error(tl, tc, "trailing commas not allowed in array initializers");
                    }

                    let elem = self.parse_expression();

                    if last == NO_IDX {
                        elems = elem;
                        last = elem;
                    } else {
                        self.ast[last].next = elem;
                        last = elem;
                    }

                    elem_count += 1;
                    if !self.match_kind(TokenKind::Comma) { break; }
                }
            }
            self.expect(TokenKind::RBracket);
            self.push_kind(line, col, AstKind::ArrayInit { elems, elem_count })
        }

        else {
            self.parse_postfix()
        }
    }

    fn parse_postfix(&mut self) -> usize {
        let mut node = self.parse_primary();

        while let Some(kind) = self.match_any(&[TokenKind::LParen, TokenKind::Dot, TokenKind::LBracket]) {
            match kind {
                TokenKind::LParen => {
                    let (line, col) = self.pos();

                    if let AstKind::Name { name } = self.ast[node].kind {
                        let mut last = NO_IDX;
                        let mut args = NO_IDX;
                        let mut arg_count = 0;

                        if !self.peek(TokenKind::RParen) {
                            loop {
                                if self.peek(TokenKind::RParen) {
                                    let (tl, tc) = self.pos();
                                    self.report_error(tl, tc, "trailing commas not allowed in call arguments");
                                }

                                let arg = self.parse_expression();
                                if last == NO_IDX {
                                    args = arg;
                                    last = arg;
                                } else {
                                    self.ast[last].next = arg;
                                    last = arg;
                                }

                                arg_count += 1;
                                if !self.match_kind(TokenKind::Comma) { break; }
                            }
                        }

                        self.expect(TokenKind::RParen);
                        node = self.push_kind(line, col, AstKind::Call { name, args, arg_count })
                    }
                    else {
                        let (l, c) = self.pos();
                        self.report_error(l, c, "only named functions can be called");
                    }
                }
                TokenKind::Dot => {
                    let (line, col) = self.pos();
                    let field_name = self.expect(TokenKind::Identifier).lexeme;
                    node = self.push_kind(line, col, AstKind::FieldAccess { expr: node, name: field_name });
                }
                TokenKind::LBracket => {
                    let (line, col) = self.pos();
                    let index_expr = self.parse_expression();
                    node = self.push_kind(line, col, AstKind::ArrayAccess { expr: node, index_expr });
                    self.expect(TokenKind::RBracket);
                }
                _ => unreachable!(),
            }
        }

        node
    }

    fn parse_primary(&mut self) -> usize {
        if self.peek(TokenKind::Identifier) {
            let (line, col) = self.pos();
            let name = self.current().lexeme;
            self.index += 1;
            self.push_kind(line, col, AstKind::Name { name })
        }
        else if self.peek(TokenKind::Integer) {
            let (line, col) = self.pos();
            let value = self.current().lexeme;
            self.index += 1;
            self.push_kind(line, col, AstKind::Integer { value: value.parse().unwrap() })
        }
        else if self.peek(TokenKind::Float) {
            let (line, col) = self.pos();
            let value = self.current().lexeme;
            self.index += 1;
            self.push_kind(line, col, AstKind::Float { value: value.parse().unwrap() })
        }
        else if self.peek(TokenKind::Byte) {
            let (line, col) = self.pos();
            let value = self.current().lexeme;
            self.index += 1;
            self.push_kind(line, col, AstKind::Byte { value: value[..value.len() - 1].parse().unwrap() })
        }
        else if self.peek(TokenKind::String) {
            let (line, col) = self.pos();
            let tok = self.current();
            let value = tok.lexeme;
            let cooked_len = tok.cooked_len;
            self.index += 1;
            self.push_kind(line, col, AstKind::String { value, cooked_len })
        }
        else if self.match_kind(TokenKind::KwTrue) {
            let (line, col)= self.pos();
            self.push_kind(line, col, AstKind::Bool { value: true })
        }
        else if self.match_kind(TokenKind::KwFalse) {
            let (line, col)= self.pos();
            self.push_kind(line, col, AstKind::Bool { value: false })
        }
        else if self.match_kind(TokenKind::LParen) {
            let expr = self.parse_expression();
            self.expect(TokenKind::RParen);
            expr
        }
        else if self.match_kind(TokenKind::KwIf) {
            let cond = self.parse_expression();
            self.expect(TokenKind::KwThen);
            let true_expr = self.parse_expression();
            self.expect(TokenKind::KwElse);
            let false_expr = self.parse_expression();
            let (line, col) = self.pos();
            self.push_kind(line, col, AstKind::Conditional { cond, true_expr, false_expr })
        }
        else if self.match_kind(TokenKind::KwStruct) {
            let name = self.expect(TokenKind::Identifier).lexeme;
            self.expect(TokenKind::LBrace);

            let (line, col) = self.pos();
            if self.struct_decls.contains_key(name) {
                self.report_error(line, col, format!("struct '{}' already declared", name));
                return self.push_kind(line, col, AstKind::Void {});
            }

            let mut fields = Vec::new();
            if self.peek(TokenKind::Identifier) {
                loop {
                    if self.peek(TokenKind::RBrace) {
                        let (tl, tc) = self.pos();
                        self.report_error(tl, tc, "trailing commas not allowed in struct declarations");
                    }

                    let field_name = self.expect(TokenKind::Identifier).lexeme;
                    self.expect(TokenKind::Colon);
                    let field_ty = self.parse_type();
                    if let crate::parser::AstType::Ptr(_) = field_ty {
                        let (el, ec) = self.pos();
                        self.report_error(el, ec, "pointer types are only permitted in extern function signatures");
                    }

                    fields.push((field_name, field_ty));

                    if !self.match_kind(TokenKind::Comma) { break; }
                }
            }

            self.expect(TokenKind::RBrace);
            self.struct_decls.insert(name, StructDecl { name, fields });
            self.push_kind(line, col, AstKind::Void {})
        }
        else if self.match_kind(TokenKind::LBrace) {
            let (line, col) = self.pos();

            let mut exprs = NO_IDX;
            let mut expr_count = 0;
            let mut last = NO_IDX;
            while !self.peek(TokenKind::RBrace) {
                let expr = self.parse_expression();
                if last == NO_IDX {
                    exprs = expr;
                    last = expr;
                } else {
                    self.ast[last].next = expr;
                    last = expr;
                }

                expr_count += 1;
            }

            self.expect(TokenKind::RBrace);
            self.push_kind(line, col, AstKind::Block { exprs, expr_count })
        }
        else {
            let (line, col) = self.pos();
            self.report_error(line, col, format!("unexpected token: {:?}", self.current().kind));
            self.index += 1;
            self.push_kind(line, col, AstKind::Unknown)
        }
    }
}

pub fn parse<'a>(tokens: &'a [Token<'a>]) -> (Vec<AstNode<'a>>, HashMap<&'a str, StructDecl<'a>>, HashMap<&'a str, FunctionDecl<'a>>, HashMap<&'a str, ExternFunctionDecl<'a>>) {
    let mut parser = Parser::new(tokens);
    parser.parse();
    (parser.ast, parser.struct_decls, parser.fn_decls, parser.extern_fns)
}

