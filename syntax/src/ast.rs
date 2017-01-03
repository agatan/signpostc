use std::iter::IntoIterator;

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
        Param {
            name: name,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunDecl {
    pub name: Symbol,
    pub params: Vec<Param>,
}

impl FunDecl {
    pub fn new<I: IntoIterator<Item=Param>>(name: Symbol, params: I) -> Self {
        FunDecl {
            name: name,
            params: params.into_iter().collect(),
        }
    }
}
