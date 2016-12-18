use std::fmt;

/// `Position` represents a source position including file name, line number, column number and
/// byte offset.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Position<'a> {
    Dummy,
    FilePos {
        filename: Option<&'a str>, // None if the source is stdin.
        line: isize, // starting at 1. if 0, the position is dummy.
        column: isize, // byte offset from the head of the line, starting at 1
        offset: isize, // byte offset from the head of the file, starting at 0
    },
}

impl<'a> Position<'a> {
    pub fn dummy() -> Self {
        Position::Dummy
    }

    pub fn is_valid(&self) -> bool {
        match *self {
            Position::Dummy => false,
            _ => true,
        }
    }
}

impl<'a> fmt::Display for Position<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Position::Dummy => "-".fmt(f),
            Position::FilePos { filename, line, column, .. } =>
            match filename {
                None => write!(f, "{}:{}", line, column),
                Some(name) => write!(f, "{}:{}:{}", name, line, column),
            }
        }
    }
}

/// `Pos` is a compact representation of `Position`.
/// Since each AST node contains its own `Pos`, `Position` is too big.
/// To convert `Pos` to `Position`, use `File` struct.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pos(Option<isize>);

impl Pos {
    pub fn dummy() -> Self {
        Pos(None)
    }

    pub fn is_valid(&self) -> bool {
        self.0.is_some()
    }
}
