#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Identifier,
    Integer,
    Float,
    String,
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    DoubleEqual,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Exclamation,
    Arrow,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Colon,
    Dot,
    KwLet,
    KwExtern,
    KwAnd,
    KwOr,
    KwIf,
    KwThen,
    KwElse,
    KwTrue,
    KwFalse,
    KwStruct,
    Eof,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub lexeme: &'a str,
    pub line_col: (u32, u32),
    pub cooked_len: usize,
}

pub fn tokenize(source: &str) -> Vec<Token<'_>> {
    let mut tokens = Vec::new();

    let bytes = source.as_bytes();
    let mut pos: usize = 0;
    let mut line: u32 = 1;
    let mut column: u32 = 1;

    macro_rules! advance {
        ($n:expr) => {{
            for _ in 0..$n {
                let b = bytes[pos];
                pos += 1;
                if b == b'\n' {
                    line += 1;
                    column = 1;
                } else {
                    column += 1;
                }
            }
        }};
    }

    while pos < bytes.len() {
        while pos < bytes.len() && bytes[pos].is_ascii_whitespace() {
            advance!(1);
        }
        if pos >= bytes.len() {
            break;
        }

        let start_pos = pos;
        let slice = &source[pos..];

        macro_rules! make_token {
            ($kind:expr) => {{
                tokens.push(Token {
                    kind: $kind,
                    lexeme: &source[start_pos..pos],
                    line_col: (line, column),
                    cooked_len: 0,
                });
            }};
        }

        if bytes[pos].is_ascii_alphabetic() || bytes[pos] == b'_' {
            while pos < bytes.len() && (bytes[pos].is_ascii_alphanumeric() || bytes[pos] == b'_') {
                advance!(1);
            }
            let ident = &source[start_pos..pos];
            if ident == "let" {
                make_token!(TokenKind::KwLet);
                continue;
            }
            if ident == "extern" {
                make_token!(TokenKind::KwExtern);
                continue;
            }
            if ident == "and" {
                make_token!(TokenKind::KwAnd);
                continue;
            }
            if ident == "or" {
                make_token!(TokenKind::KwOr);
                continue;
            }
            if ident == "if" {
                make_token!(TokenKind::KwIf);
                continue;
            }
            if ident == "then" {
                make_token!(TokenKind::KwThen);
                continue;
            }
            if ident == "else" {
                make_token!(TokenKind::KwElse);
                continue;
            }
            if ident == "true" {
                make_token!(TokenKind::KwTrue);
                continue;
            }
            if ident == "false" {
                make_token!(TokenKind::KwFalse);
                continue;
            }
            if ident == "struct" {
                make_token!(TokenKind::KwStruct);
                continue;
            }
            make_token!(TokenKind::Identifier);
            continue;
        }

        if bytes[pos].is_ascii_digit() {
            while pos < bytes.len() && bytes[pos].is_ascii_digit() {
                advance!(1);
            }
            if pos < bytes.len() && bytes[pos] == b'.' {
                advance!(1);
                while bytes[pos].is_ascii_digit() {
                    advance!(1);
                }
                make_token!(TokenKind::Float);
                continue;
            } else {
                make_token!(TokenKind::Integer);
                continue;
            }
        }

        if bytes[pos] == b'"' {
            let token_line = line;
            let token_col = column;
            advance!(1);
            let start_inner = pos;
            let mut cooked_len = 0usize;
            while pos < bytes.len() {
                if bytes[pos] == b'\\' && pos + 1 < bytes.len() {
                    cooked_len += 1;
                    advance!(2);
                    continue;
                }
                if bytes[pos] == b'"' {
                    break;
                }
                cooked_len += 1;
                advance!(1);
            }
            let inner_end = pos;
            if pos < bytes.len() && bytes[pos] == b'"' {
                advance!(1);
            }
            tokens.push(Token {
                kind: TokenKind::String,
                lexeme: &source[start_inner..inner_end],
                line_col: (token_line, token_col),
                cooked_len,
            });
            continue;
        }

        if slice.starts_with("==") {
            advance!(2);
            make_token!(TokenKind::DoubleEqual);
            continue;
        }
        if slice.starts_with("!=") {
            advance!(2);
            make_token!(TokenKind::NotEqual);
            continue;
        }
        if slice.starts_with(">=") {
            advance!(2);
            make_token!(TokenKind::GreaterEqual);
            continue;
        }
        if slice.starts_with("<=") {
            advance!(2);
            make_token!(TokenKind::LessEqual);
            continue;
        }
        if slice.starts_with("->") {
            advance!(2);
            make_token!(TokenKind::Arrow);
            continue;
        }

        if bytes[pos] == b'+' {
            advance!(1);
            make_token!(TokenKind::Plus);
            continue;
        }
        if bytes[pos] == b'-' {
            advance!(1);
            make_token!(TokenKind::Minus);
            continue;
        }
        if bytes[pos] == b'*' {
            advance!(1);
            make_token!(TokenKind::Star);
            continue;
        }
        if bytes[pos] == b'/' {
            advance!(1);
            make_token!(TokenKind::Slash);
            continue;
        }
        if bytes[pos] == b'=' {
            advance!(1);
            make_token!(TokenKind::Equal);
            continue;
        }
        if bytes[pos] == b'>' {
            advance!(1);
            make_token!(TokenKind::Greater);
            continue;
        }
        if bytes[pos] == b'<' {
            advance!(1);
            make_token!(TokenKind::Less);
            continue;
        }
        if bytes[pos] == b'!' {
            advance!(1);
            make_token!(TokenKind::Exclamation);
            continue;
        }
        if bytes[pos] == b'(' {
            advance!(1);
            make_token!(TokenKind::LParen);
            continue;
        }
        if bytes[pos] == b')' {
            advance!(1);
            make_token!(TokenKind::RParen);
            continue;
        }
        if bytes[pos] == b'{' {
            advance!(1);
            make_token!(TokenKind::LBrace);
            continue;
        }
        if bytes[pos] == b'}' {
            advance!(1);
            make_token!(TokenKind::RBrace);
            continue;
        }
        if bytes[pos] == b'[' {
            advance!(1);
            make_token!(TokenKind::LBracket);
            continue;
        }
        if bytes[pos] == b']' {
            advance!(1);
            make_token!(TokenKind::RBracket);
            continue;
        }
        if bytes[pos] == b',' {
            advance!(1);
            make_token!(TokenKind::Comma);
            continue;
        }
        if bytes[pos] == b':' {
            advance!(1);
            make_token!(TokenKind::Colon);
            continue;
        }
        if bytes[pos] == b'.' {
            advance!(1);
            make_token!(TokenKind::Dot);
            continue;
        }

        advance!(1);
        make_token!(TokenKind::Unknown);
    }

    tokens.push(Token {
        kind: TokenKind::Eof,
        lexeme: "",
        line_col: (line, column),
        cooked_len: 0,
    });
    tokens
}
