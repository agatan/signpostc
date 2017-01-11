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
    pub ty: Type,
}

impl Param {
    pub fn new(name: Symbol, ty: Type) -> Self {
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
    pub ret: Type,
    // TODO(agatan): body should be `Vec<Stmt>`
    pub body: Expr,
}

impl FunDecl {
    pub fn new(name: Symbol,
               type_params: Vec<Symbol>,
               params: Vec<Param>,
               ret: Option<Type>,
               body: Expr)
               -> Self {
        FunDecl {
            name: name,
            type_params: type_params,
            params: params,
            ret: ret.unwrap_or(Type::Builtin(BuiltinType::Unit)),
            body: body,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Unit,
    Int(i64),
    Bool(bool),
    String(Symbol),
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
    Error,
    Literal(Literal),
    Ident(Symbol),
    Prefix(Symbol, Box<Expr>),
    Infix(Box<Expr>, Symbol, Box<Expr>),
    Paren(Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinType {
    Unit,
    Int,
    Bool,
    String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Represents error state for syntax checking.
    Error,
    /// Should be infered later.
    Hole,
    /// Builtin types
    Builtin(BuiltinType),
    /// Mutable reference type
    Ref,
    /// User defined type
    Ident(Symbol),
    /// Type application. e.g, `Ref<Int>`
    App(Box<Type>, Vec<Type>),
}

impl Type {
    pub fn from_symbol(sym: Symbol) -> Type {
        let s = sym.as_str();
        match &*s {
            "Unit" => Type::Builtin(BuiltinType::Unit),
            "Int" => Type::Builtin(BuiltinType::Int),
            "Bool" => Type::Builtin(BuiltinType::Bool),
            "String" => Type::Builtin(BuiltinType::String),
            "Ref" => Type::Ref,
            _ => Type::Ident(sym),
        }
    }

    pub fn error() -> Type {
        Type::Error
    }

    pub fn hole() -> Type {
        Type::Hole
    }

    pub fn app(base: Type, args: Vec<Type>) -> Type {
        Type::App(box base, args)
    }
}

