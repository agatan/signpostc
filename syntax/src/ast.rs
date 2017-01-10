use std::io::{self, Write};
use std::fmt;

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
pub enum Expr {
    Error,
    Literal(Pos, Literal),
    Ident(Pos, Symbol),
    Prefix(Pos, Symbol, Box<Expr>),
    Infix(Pos, Box<Expr>, Symbol, Box<Expr>),
    Paren(Pos, Box<Expr>),
    Call(Pos, Box<Expr>, Vec<Expr>),
}

impl Expr {
    pub fn pos(&self) -> Pos {
        match *self {
            Expr::Error => Pos::dummy(),
            Expr::Literal(p, _) |
            Expr::Ident(p, _) |
            Expr::Prefix(p, _, _) |
            Expr::Infix(p, _, _, _) |
            Expr::Paren(p, _) |
            Expr::Call(p, _, _) => p,
        }
    }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VisitState<E> {
    Run,
    Interrupt,
    Error(E),
}

impl<E, E2: From<E>> ::std::convert::Into<Result<(), E2>> for VisitState<E> {
    fn into(self) -> Result<(), E2> {
        match self {
            VisitState::Error(e) => Err(E2::from(e)),
            _ => Ok(()),
        }
    }
}

macro_rules! visit {
    ($e:expr) => {
        match $e {
            VisitState::Run => (),
            e => return e,
        }
    }
}

macro_rules! try_visit {
    ($e:expr) => {
        match $e {
            Ok(t) => t,
            Err(e) => return VisitState::Error(e),
        }
    }
}

trait Visitor {
    type Error;

    fn visit_program(&mut self, prog: &Program) -> VisitState<Self::Error> {
        for decl in prog.decls.iter() {
            visit!(self.visit_decl(decl));
        }
        VisitState::Run
    }

    fn visit_decl(&mut self, decl: &Decl) -> VisitState<Self::Error> {
        match *decl {
            Decl::Error => VisitState::Run,
            Decl::Def(_, FunDecl { ref params, ref ret, ref body, .. }) => {
                for param in params {
                    visit!(self.visit_ty(&param.ty));
                }
                visit!(self.visit_ty(ret));
                visit!(self.visit_expr(body));
                VisitState::Run
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> VisitState<Self::Error> {
        match *expr {
            Expr::Error |
            Expr::Literal(_, _) |
            Expr::Ident(_, _) => VisitState::Run,
            Expr::Prefix(_, _, ref e) => self.visit_expr(e),
            Expr::Infix(_, ref lhs, _, ref rhs) => {
                visit!(self.visit_expr(lhs));
                self.visit_expr(rhs)
            }
            Expr::Paren(_, ref e) => self.visit_expr(e),
            Expr::Call(_, ref f, ref args) => {
                visit!(self.visit_expr(f));
                for arg in args {
                    visit!(self.visit_expr(arg));
                }
                VisitState::Run
            }
        }
    }

    fn visit_ty(&mut self, ty: &Type) -> VisitState<Self::Error> {
        match *ty {
            Type::Error | Type::Hole | Type::Builtin(_) | Type::Ref | Type::Ident(_) => {
                VisitState::Run
            }
            Type::App(ref base, ref args) => {
                visit!(self.visit_ty(base));
                for arg in args {
                    visit!(self.visit_ty(arg));
                }
                VisitState::Run
            }
        }
    }
}

pub struct Dumper<T> {
    indent: usize,
    depth: usize,
    w: T,
}

macro_rules! __dump {
    ($w:expr, $($args:tt)*) => {
        $w.writeln(format_args!($($args)*))
    }
}

macro_rules! __try_dump {
    ($w:expr, $($args:tt)*) => {
        try_visit!(__dump!($w, $($args)*))
    }
}

struct Indent<'a, T: 'a>(&'a mut Dumper<T>);

impl<'a, T> ::std::ops::Drop for Indent<'a, T> {
    fn drop(&mut self) {
        self.0.exit();
    }
}

impl<'a, T> ::std::ops::Deref for Indent<'a, T> {
    type Target = Dumper<T>;
    fn deref(&self) -> &Dumper<T> {
        self.0
    }
}

impl<'a, T> ::std::ops::DerefMut for Indent<'a, T> {
    fn deref_mut(&mut self) -> &mut Dumper<T> {
        self.0
    }
}

impl<T> Dumper<T> {
    pub fn new(w: T) -> Self {
        Dumper {
            indent: 2,
            depth: 0,
            w: w,
        }
    }

    fn enter<'a>(&'a mut self) -> Indent<'a, T> {
        self.depth += 1;
        Indent(self)
    }

    fn exit(&mut self) {
        self.depth -= 1;
    }
}

impl<T: Write> Dumper<T> {
    fn writeln(&mut self, args: fmt::Arguments) -> io::Result<()> {
        for _ in 0..(self.indent * self.depth) {
            self.w.write_all(b" ")?;
        }
        self.w.write_fmt(args)?;
        self.w.write_all(b"\n")
    }

    fn dump_param(&mut self, param: &Param) -> VisitState<io::Error> {
        __try_dump!(self, "param:");
        {
            let mut w = self.enter();
            __try_dump!(w, "name: {}", param.name.as_str());
            __try_dump!(w, "type:");
            {
                let mut w = w.enter();
                visit!(w.visit_ty(&param.ty));
            }
        }
        VisitState::Run
    }
}

impl<T: Write> Visitor for Dumper<T> {
    type Error = io::Error;

    fn visit_decl(&mut self, decl: &Decl) -> VisitState<Self::Error> {
        match *decl {
            Decl::Error => {
                try_visit!(self.writeln(format_args!("error:")));
                VisitState::Run
            }
            Decl::Def(_, ref f) => {
                __try_dump!(self, "def:");
                let mut w = self.enter();
                __try_dump!(w, "name: {}", f.name.as_str());
                __try_dump!(w, "type_params:");
                {
                    let mut w = w.enter();
                    for ty in f.type_params.iter() {
                        __try_dump!(w, "PARAM: {}", ty.as_str());
                    }
                }
                __try_dump!(w, "params:");
                {
                    let mut w = w.enter();
                    for param in f.params.iter() {
                        visit!(w.dump_param(param));
                    }
                }
                __try_dump!(w, "return:");
                {
                    let mut w = w.enter();
                    visit!(w.visit_ty(&f.ret));
                }
                __try_dump!(w, "body:");
                {
                    let mut w = w.enter();
                    visit!(w.visit_expr(&f.body));
                }
                VisitState::Run
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> VisitState<Self::Error> {
        match *expr {
            Expr::Error => __try_dump!(self, "error:"),
            Expr::Literal(_, ref lit) => {
                match *lit {
                    Literal::Unit => __try_dump!(self, "unit:"),
                    Literal::Int(i) => __try_dump!(self, "int: {}", i),
                    Literal::Bool(b) => __try_dump!(self, "bool: {}", b),
                    Literal::String(s) => __try_dump!(self, "string: {}", s.as_str()),
                }
            }
            Expr::Ident(_, ref name) => {
                __try_dump!(self, "ident: {}", name.as_str());
            }
            Expr::Prefix(_, op, ref e) => {
                __try_dump!(self, "prefix op:");
                {
                    let mut w = self.enter();
                    __try_dump!(w, "op: {}", op.as_str());
                    visit!(w.visit_expr(e));
                }
            }
            Expr::Infix(_, ref lhs, op, ref rhs) => {
                __try_dump!(self, "infix op:");
                {
                    let mut w = self.enter();
                    __try_dump!(w, "op: {}", op.as_str());
                    visit!(w.visit_expr(lhs));
                    visit!(w.visit_expr(rhs));
                }
            }
            Expr::Paren(_, ref e) => {
                visit!(self.visit_expr(e));
            }
            Expr::Call(_, ref f, ref args) => {
                __try_dump!(self, "call:");
                {
                    let mut w = self.enter();
                    __try_dump!(w, "function:");
                    {
                        let mut w = w.enter();
                        visit!(w.visit_expr(f));
                    }
                    __try_dump!(w, "arguments:");
                    {
                        let mut w = w.enter();
                        for arg in args {
                            visit!(w.visit_expr(arg));
                        }
                    }
                }
            }
        }
        VisitState::Run
    }

    fn visit_ty(&mut self, ty: &Type) -> VisitState<Self::Error> {
        match *ty {
            Type::Error => __try_dump!(self, "error:"),
            Type::Hole => __try_dump!(self, "_"),
            Type::Builtin(ty) => {
                match ty {
                    BuiltinType::Unit => __try_dump!(self, "Unit"),
                    BuiltinType::Int => __try_dump!(self, "Int"),
                    BuiltinType::Bool => __try_dump!(self, "Bool"),
                    BuiltinType::String => __try_dump!(self, "String"),
                }
            }
            Type::Ref => __try_dump!(self, "Ref"),
            Type::Ident(ref name) => __try_dump!(self, "Ident: {}", name.as_str()),
            Type::App(ref base, ref args) => {
                __try_dump!(self, "App:");
                {
                    let mut w = self.enter();
                    __try_dump!(w, "base:");
                    {
                        let mut w = w.enter();
                        visit!(w.visit_ty(base));
                    }
                    __try_dump!(w, "args:");
                    {
                        let mut w = w.enter();
                        for arg in args {
                            visit!(w.visit_ty(arg));
                        }
                    }
                }
            }
        }
        VisitState::Run
    }
}

pub fn dump_program<T: Write>(w: T, prog: &Program) -> Result<(), io::Error> {
    let mut dumper = Dumper::new(w);
    dumper.visit_program(prog).into()
}

pub fn dump_expression<T: Write>(w: T, expr: &Expr) -> Result<(), io::Error> {
    let mut dumper = Dumper::new(w);
    dumper.visit_expr(expr).into()
}
