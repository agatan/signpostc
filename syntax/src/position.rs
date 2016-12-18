use std::fmt;

/// `Position` represents a source position including file name, line number, column number and
/// byte offset.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Position<'a> {
    Dummy,
    FilePos {
        filename: Option<&'a str>, // None if the source is stdin.
        line: usize, // starting at 1. if 0, the position is dummy.
        column: usize, // byte offset from the head of the line, starting at 1
        offset: usize, // byte offset from the head of the file, starting at 0
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
            Position::FilePos { filename, line, column, .. } => {
                match filename {
                    None => write!(f, "{}:{}", line, column),
                    Some(name) => write!(f, "{}:{}:{}", name, line, column),
                }
            }
        }
    }
}

/// `Pos` is a compact representation of `Position`.
/// Since each AST node contains its own `Pos`, `Position` is too big.
/// To convert `Pos` to `Position`, use `File` struct.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pos(Option<usize>);

impl Pos {
    pub fn dummy() -> Self {
        Pos(None)
    }

    pub fn is_valid(&self) -> bool {
        self.0.is_some()
    }
}

/// `File` represents a source file.
#[derive(Debug, Clone)]
pub struct File<'a> {
    name: Option<&'a str>,
    size: usize, // byte size of the file.
    lines: Vec<usize>, // byte offsets of heads of the file.
}

impl<'a> File<'a> {
    pub fn new(filename: Option<&'a str>, size: usize) -> Self {
        File {
            name: filename,
            size: size,
            lines: vec![0],
        }
    }

    pub fn name(&self) -> Option<&'a str> {
        self.name
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn line_count(&self) -> usize {
        self.lines.len()
    }

    /// Annotate new line.
    pub fn add_line(&mut self, offset: usize) {
        debug_assert!(self.lines[self.lines.len() - 1] < offset); // self.lines always contains at least 1 element.
        debug_assert!(offset < self.size);
        self.lines.push(offset);
    }

    /// returns the compact representation of the offset.
    pub fn pos(&self, offset: usize) -> Pos {
        debug_assert!(offset <= self.size);
        Pos(Some(offset))
    }

    pub fn offset(&self, pos: Pos) -> usize {
        debug_assert!(pos.0.is_some());
        pos.0.unwrap()
    }

    fn search_line(&self, offset: usize) -> usize {
        for (i, &head) in self.lines.iter().enumerate() {
            if head > offset {
                return i - 1;
            }
        }
        self.lines.len() - 1
    }

    /// returns the corresponding position of `pos`.
    pub fn position(&self, pos: Pos) -> Position<'a> {
        match pos.0 {
            None => Position::Dummy,
            Some(bs) => {
                let i = self.search_line(bs);
                let line = i + 1;
                let column = bs - self.lines[i] + 1;
                Position::FilePos {
                    filename: self.name,
                    line: line,
                    column: column,
                    offset: bs,
                }
            }
        }
    }
}

#[test]
fn test_position() {
    let mut file = File::new(Some("test"), 30);
    let heads = &[10, 15, 20];
    for &head in heads {
        file.add_line(head);
    }
    let tests = vec![
        // offset, expected line, expected column
        (0, 1, 1),
        (1, 1, 2),
        (10, 2, 1),
        (11, 2, 2),
        (15, 3, 1),
        (16, 3, 2),
        (29, 4, 10),
    ];

    for (offset, expected_line, expected_column) in tests {
        let pos = file.pos(offset);
        let position = file.position(pos);
        assert!(position.is_valid());
        assert_eq!(format!("test:{}:{}", expected_line, expected_column),
                   format!("{}", position));
    }
}
