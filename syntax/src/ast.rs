use position::Pos;
use symbol::Symbol;

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
}

impl Param {
    pub fn new(name: Symbol) -> Self {
        Param { name: name }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunDecl {
    pub name: Symbol,
    pub type_params: Vec<Symbol>,
    pub params: Vec<Param>,
}

impl FunDecl {
    pub fn new(name: Symbol, params: Vec<Param>) -> Self {
        FunDecl {
            name: name,
            type_params: Vec::new(),
            params: params,
        }
    }

    pub fn generic(name: Symbol, type_params: Vec<Symbol>, params: Vec<Param>) -> Self {
        FunDecl {
            name: name,
            type_params: type_params,
            params: params,
        }
    }
}
