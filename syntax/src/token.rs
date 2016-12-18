use position::Pos;

/// represents tokens of source code.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Token<'a> {
    pos: Pos,
    kind: TokenKind,
    literal: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(pos: Pos, kind: TokenKind, literal: &'a str) -> Self {
        Token {
            pos: pos,
            kind: kind,
            literal: literal,
        }
    }

    pub fn pos(&self) -> Pos {
        self.pos
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn has_kind(&self, kind: TokenKind) -> bool {
        self.kind == kind
    }

    pub fn literal(&self) -> &str {
        self.literal
    }
}

/// Kind of tokens.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
    Error,

    EOF,

    Int,
    True,
    False,
    Uident,
    Ident,

    Operator,

    Eq,
    Pipe,
    Pipepipe, // to distinguish `|| { 1 }`

    Arrow, // ->
    FatArrow, // =>

    // Delimiters
    Comma,
    Colon,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbrack,
    Rbrack,

    // keywords
    Def,
    Let,
    Struct,
    Enum,
    Interface,
    Implement,

    If,
    Else,
    Match,
    Return,
    For,
    In,
    Continue,
    Break,
}

impl TokenKind {
    pub fn lookup(s: &str) -> Self {
        match s {
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "def" => TokenKind::Def,
            "let" => TokenKind::Let,
            "struct" => TokenKind::Struct,
            "enum" => TokenKind::Enum,
            "interface" => TokenKind::Interface,
            "implement" => TokenKind::Implement,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "match" => TokenKind::Match,
            "return" => TokenKind::Return,
            "for" => TokenKind::For,
            "in" => TokenKind::In,
            "continue" => TokenKind::Continue,
            "break" => TokenKind::Break,
            _ => TokenKind::Ident,
        }
    }

    pub fn lookup_operator(s: &str) -> Self {
        match s {
            "=" => TokenKind::Eq,
            "|" => TokenKind::Pipe,
            "||" => TokenKind::Pipepipe,
            "->" => TokenKind::Arrow,
            "=>" => TokenKind::FatArrow,
            _ => TokenKind::Operator,
        }
    }
}
