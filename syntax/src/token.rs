use std::fmt;

use position::Pos;
use symbol::Symbol;

/// represents tokens of source code.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Token {
    pos: Pos,
    kind: TokenKind,
    symbol: Symbol,
}

impl Token {
    pub fn dummy() -> Self {
        Token {
            pos: Pos::dummy(),
            kind: TokenKind::Error,
            symbol: Symbol::intern(""),
        }
    }

    pub fn new(pos: Pos, kind: TokenKind, symbol: &str) -> Self {
        Token {
            pos: pos,
            kind: kind,
            symbol: Symbol::intern(symbol),
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

    pub fn symbol(&self) -> Symbol {
        self.symbol
    }

    pub fn is_prefix_operator(&self) -> bool {
        if self.kind != TokenKind::Operator {
            return false;
        }

        fn is_prefix_operator_char(c: char) -> bool {
            c == '-' || c == '!' || c == '@'
        }
        self.symbol.as_str().starts_with(is_prefix_operator_char)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            TokenKind::Operator | TokenKind::Uident | TokenKind::Ident => {
                fmt::Display::fmt(&*self.symbol.as_str(), f)
            }
            ref kind => fmt::Display::fmt(kind, f),
        }
    }
}

/// Kind of tokens.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
    Error,

    EOF,
    Newline,

    Int,
    True,
    False,
    String,
    Uident,
    Ident,

    Operator,

    Eq,

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
    Langle,
    Rangle,

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

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::TokenKind::*;
        let s = match *self {
            Error => "<ERROR>",
            EOF => "EOF",
            Newline => "'\\n'",
            Int => "Int",
            True => "true",
            False => "false",
            String => "String",
            Uident => "upper identifier",
            Ident => "identifier",
            Operator => "operator",
            Eq => "'='",
            Arrow => "'->'",
            FatArrow => "'=>'",
            Comma => "','",
            Colon => "':'",
            Semicolon => "';'",
            Lparen => "'('",
            Rparen => "')'",
            Lbrace => "'{'",
            Rbrace => "'}'",
            Lbrack => "'['",
            Rbrack => "']'",
            Langle => "'<'",
            Rangle => "'>'",
            Def => "def",
            Let => "let",
            Struct => "struct",
            Enum => "enum",
            Interface => "interface",
            Implement => "implement",
            If => "if",
            Else => "else",
            Match => "match",
            Return => "return",
            For => "for",
            In => "in",
            Continue => "continue",
            Break => "break",
        };
        fmt::Display::fmt(s, f)
    }
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
            "->" => TokenKind::Arrow,
            "=>" => TokenKind::FatArrow,
            "<" => TokenKind::Langle,
            ">" => TokenKind::Rangle,
            _ => TokenKind::Operator,
        }
    }
}
