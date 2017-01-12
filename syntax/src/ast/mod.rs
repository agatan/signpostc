use position::{DUMMY_POS, Pos};
use symbol::Symbol;

mod visitor;

pub use self::visitor::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(u32);

impl NodeId {
    pub fn new(x: usize) -> NodeId {
        assert!(x < (::std::u32::MAX as usize));
        NodeId(x as u32)
    }

    pub fn from_u32(x: u32) -> NodeId {
        NodeId(x)
    }

    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }

    pub fn as_u32(&self) -> u32 {
        self.0
    }
}

pub const DUMMY_NODE_ID: NodeId = NodeId(!0);

#[derive(Debug, Clone)]
pub struct Program {
    pub decls: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Error,
    Def(Pos, FunDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: Symbol,
    pub ty: Ty,
}

impl Param {
    pub fn new(name: Symbol, ty: Ty) -> Self {
        Param {
            name: name,
            ty: ty,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunDecl {
    pub name: Symbol,
    pub type_params: Vec<Symbol>,
    pub params: Vec<Param>,
    pub ret: Option<Ty>,
    pub body: Expr,
}

impl FunDecl {
    pub fn new(name: Symbol,
               type_params: Vec<Symbol>,
               params: Vec<Param>,
               ret: Option<Ty>,
               body: Expr)
               -> Self {
        FunDecl {
            name: name,
            type_params: type_params,
            params: params,
            ret: ret,
            body: body,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    pub id: NodeId,
    pub node: StmtKind,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    Error,
    /// expression without semicolon (returns value)
    Expr(Expr),
    /// expression with semicolon (discards value)
    Semi(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub id: NodeId,
    pub node: ExprKind,
    pub pos: Pos,
}

impl Expr {
    pub fn error() -> Expr {
        Expr {
            id: DUMMY_NODE_ID,
            node: ExprKind::Error,
            pos: DUMMY_POS,
        }
    }

    pub fn new(id: NodeId, pos: Pos, node: ExprKind) -> Expr {
        Expr {
            id: id,
            node: node,
            pos: pos,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    /// Placeholder to notate error
    Error,
    /// `1`, `"foo"`, `true`, ...
    Literal(Literal),
    /// `foo`
    Ident(Symbol),
    /// `-2`
    Prefix(Symbol, Box<Expr>),
    /// `1 + 2`
    Infix(Box<Expr>, Symbol, Box<Expr>),
    /// `(1 + 2)`
    Paren(Box<Expr>),
    /// `f(1, 2)`
    Call(Box<Expr>, Vec<Expr>),
    /// `x.method(args)`
    Ufcs(Box<Expr>, Symbol, Vec<Expr>),
    /// `{ ... }`
    Block(Vec<Stmt>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Unit,
    Int(i64),
    Bool(bool),
    String(Symbol),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinTy {
    Unit,
    Int,
    Bool,
    String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ty {
    pub id: NodeId,
    pub node: TyKind,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyKind {
    /// Represents error state for syntax checking.
    Error,
    /// Should be infered later.
    Hole,
    /// User defined type
    Ident(Symbol),
    /// Ty application. e.g, `Ref<Int>`
    App(Symbol, Vec<Ty>),
}

impl Ty {
    pub fn error() -> Ty {
        Ty {
            id: DUMMY_NODE_ID,
            node: TyKind::Error,
            pos: DUMMY_POS,
        }
    }
}
