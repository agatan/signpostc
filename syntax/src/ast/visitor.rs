use std::io::{self, Write};
use std::fmt;
use std::ops::{Deref, DerefMut, Drop};

use ast::*;

pub trait Visit {
    fn accept<T: Visitor + ?Sized>(&self, visitor: &mut T) -> VisitState<T::Error>;
}

impl Visit for Program {
    fn accept<T: Visitor + ?Sized>(&self, visitor: &mut T) -> VisitState<T::Error> {
        visitor.visit_program(self)
    }
}

impl Visit for Decl {
    fn accept<T: Visitor + ?Sized>(&self, visitor: &mut T) -> VisitState<T::Error> {
        visitor.visit_decl(self)
    }
}

impl Visit for Stmt {
    fn accept<T: Visitor + ?Sized>(&self, visitor: &mut T) -> VisitState<T::Error> {
        visitor.visit_stmt(self)
    }
}

impl Visit for Expr {
    fn accept<T: Visitor + ?Sized>(&self, visitor: &mut T) -> VisitState<T::Error> {
        visitor.visit_expr(self)
    }
}

impl Visit for Ty {
    fn accept<T: Visitor + ?Sized>(&self, visitor: &mut T) -> VisitState<T::Error> {
        visitor.visit_ty(self)
    }
}

#[must_use]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VisitState<E> {
    Run,
    Interrupt,
    Error(E),
}

impl<E> VisitState<E> {
    pub fn into_result(self) -> Result<(), E> {
        self.into()
    }
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

pub trait Visitor {
    type Error;

    fn visit<T: Visit>(&mut self, t: &T) -> VisitState<Self::Error> {
        t.accept(self)
    }

    fn visit_program(&mut self, prog: &Program) -> VisitState<Self::Error> {
        for decl in prog.decls.iter() {
            visit!(self.visit_decl(decl));
        }
        VisitState::Run
    }

    fn visit_decl(&mut self, decl: &Decl) -> VisitState<Self::Error> {
        match decl.node {
            DeclKind::Error => VisitState::Run,
            DeclKind::Def(FunDecl { ref params, ref ret, ref body, .. }) => {
                for param in params {
                    visit!(self.visit_ty(&param.ty));
                }
                if let Some(ref ret) = *ret {
                    visit!(self.visit_ty(ret));
                }
                self.visit_expr(body)
            }
            DeclKind::Struct(Struct { ref fields, .. }) => {
                for f in fields {
                    visit!(self.visit_ty(&f.ty));
                }
                VisitState::Run
            }
            DeclKind::Data(Data { ref variants, .. }) => {
                for v in variants {
                    for t in &v.params {
                        visit!(self.visit_ty(t));
                    }
                }
                VisitState::Run
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> VisitState<Self::Error> {
        match stmt.node {
            StmtKind::Error => VisitState::Run,
            StmtKind::Expr(ref e) |
            StmtKind::Semi(ref e) => self.visit_expr(e),
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> VisitState<Self::Error> {
        match expr.node {
            ExprKind::Error |
            ExprKind::Literal(_) |
            ExprKind::Ident(_) => VisitState::Run,
            ExprKind::Prefix(_, ref e) => self.visit_expr(e),
            ExprKind::Infix(ref lhs, _, ref rhs) => {
                visit!(self.visit_expr(lhs));
                self.visit_expr(rhs)
            }
            ExprKind::Paren(ref e) => self.visit_expr(e),
            ExprKind::Call(ref f, ref args) => {
                visit!(self.visit_expr(f));
                for arg in args {
                    visit!(self.visit_expr(arg));
                }
                VisitState::Run
            }
            ExprKind::Ufcs(ref base, _, ref args) => {
                visit!(self.visit_expr(base));
                for arg in args {
                    visit!(self.visit_expr(arg));
                }
                VisitState::Run
            }
            ExprKind::Block(ref stmts) => {
                for stmt in stmts {
                    visit!(self.visit_stmt(stmt));
                }
                VisitState::Run
            }
            ExprKind::Struct(_, ref fields) => {
                for f in fields {
                    visit!(self.visit_expr(&f.expr));
                }
                VisitState::Run
            }
        }
    }

    fn visit_ty(&mut self, ty: &Ty) -> VisitState<Self::Error> {
        match ty.node {
            TyKind::Error | TyKind::Hole | TyKind::Ident(_) => VisitState::Run,
            TyKind::App(_, ref args) => {
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
        let mut w = self.enter();
        __try_dump!(w, "name: {}", param.name.as_str());
        __try_dump!(w, "type:");
        {
            let mut w = w.enter();
            w.visit_ty(&param.ty)
        }
    }
}

struct Indent<'a, T: 'a>(&'a mut Dumper<T>);

impl<'a, T> Deref for Indent<'a, T> {
    type Target = Dumper<T>;
    fn deref(&self) -> &Dumper<T> {
        self.0
    }
}

impl<'a, T> DerefMut for Indent<'a, T> {
    fn deref_mut(&mut self) -> &mut Dumper<T> {
        self.0
    }
}

impl<'a, T> Drop for Indent<'a, T> {
    fn drop(&mut self) {
        self.0.exit();
    }
}

impl<T: Write> Visitor for Dumper<T> {
    type Error = io::Error;

    fn visit_decl(&mut self, decl: &Decl) -> VisitState<Self::Error> {
        match decl.node {
            DeclKind::Error => {
                __try_dump!(self, "error:");
                VisitState::Run
            }
            DeclKind::Def(ref f) => {
                __try_dump!(self, "def: {}", f.name.as_str());
                {
                    let mut w = self.enter();
                    __try_dump!(w, "type_params:");
                    {
                        let mut w = w.enter();
                        for ty in f.type_params.iter() {
                            __try_dump!(w, "name: {}", ty.as_str());
                        }
                    }
                    __try_dump!(w, "params:");
                    {
                        let mut w = w.enter();
                        for param in f.params.iter() {
                            visit!(w.dump_param(param));
                        }
                    }
                    match f.ret {
                        Some(ref ty) => {
                            __try_dump!(w, "return:");
                            {
                                let mut w = w.enter();
                                visit!(w.visit_ty(ty));
                            }
                        }
                        None => __try_dump!(w, "return: (default)"),
                    }
                    __try_dump!(w, "body:");
                    {
                        let mut w = w.enter();
                        visit!(w.visit_expr(&f.body));
                    }
                    VisitState::Run
                }
            }
            DeclKind::Struct(ref st) => {
                __try_dump!(self, "struct: {}", st.name.as_str());
                {
                    let mut w = self.enter();
                    __try_dump!(w, "type_params:");
                    {
                        let mut w = w.enter();
                        for ty in st.type_params.iter() {
                            __try_dump!(w, "name: {}", ty.as_str());
                        }
                    }
                    __try_dump!(w, "fields:");
                    {
                        let mut w = w.enter();
                        for f in st.fields.iter() {
                            __try_dump!(w, "name: {}", f.name.as_str());
                            __try_dump!(w, "type:");
                            visit!(w.visit_ty(&f.ty));
                        }
                    }
                    VisitState::Run
                }
            }
            DeclKind::Data(Data { name, ref variants }) => {
                __try_dump!(self, "data: {}", name.as_str());
                {
                    let mut w = self.enter();
                    for v in variants {
                        __try_dump!(w, "{}:", v.ident.as_str());
                        {
                            let mut w = w.enter();
                            for t in &v.params {
                                visit!(w.visit_ty(t));
                            }
                        }
                    }
                }
                VisitState::Run
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> VisitState<Self::Error> {
        match stmt.node {
            StmtKind::Error => __try_dump!(self, "error:"),
            StmtKind::Expr(ref e) => visit!(self.visit_expr(e)),
            StmtKind::Semi(ref e) => {
                __try_dump!(self, "semi:");
                {
                    let mut w = self.enter();
                    visit!(w.visit_expr(e))
                }
            }
        }
        VisitState::Run
    }

    fn visit_expr(&mut self, expr: &Expr) -> VisitState<Self::Error> {
        match expr.node {
            ExprKind::Error => __try_dump!(self, "error:"),
            ExprKind::Literal(ref lit) => {
                match *lit {
                    Literal::Unit => __try_dump!(self, "unit:"),
                    Literal::Int(i) => __try_dump!(self, "int: {}", i),
                    Literal::Bool(b) => __try_dump!(self, "bool: {}", b),
                    Literal::String(s) => __try_dump!(self, "string: {}", s.as_str()),
                }
            }
            ExprKind::Ident(ref name) => {
                __try_dump!(self, "ident: {}", name.as_str());
            }
            ExprKind::Prefix(op, ref e) => {
                __try_dump!(self, "prefix op: {}", op.as_str());
                {
                    let mut w = self.enter();
                    visit!(w.visit_expr(e))
                }
            }
            ExprKind::Infix(ref lhs, op, ref rhs) => {
                __try_dump!(self, "infix op: {}", op.as_str());
                {
                    let mut w = self.enter();
                    visit!(w.visit_expr(lhs));
                    visit!(w.visit_expr(rhs));
                }
            }
            ExprKind::Paren(ref e) => {
                visit!(self.visit_expr(e));
            }
            ExprKind::Call(ref f, ref args) => {
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
            ExprKind::Ufcs(ref base, fname, ref args) => {
                __try_dump!(self, "ufcs:");
                {
                    let mut w = self.enter();
                    visit!(w.visit_expr(base));
                    __try_dump!(w, "name: {}", fname.as_str());
                    __try_dump!(w, "arguments:");
                    {
                        let mut w = w.enter();
                        for arg in args {
                            visit!(w.visit_expr(arg));
                        }
                    }
                }
            }
            ExprKind::Block(ref stmts) => {
                __try_dump!(self, "block:");
                {
                    let mut w = self.enter();
                    for stmt in stmts {
                        visit!(w.visit_stmt(stmt));
                    }
                }
            }
            ExprKind::Struct(name, ref fields) => {
                __try_dump!(self, "struct: {}", name.as_str());
                {
                    let mut w = self.enter();
                    for f in fields {
                        __try_dump!(w, "{}:", f.ident.as_str());
                        {
                            let mut w = w.enter();
                            visit!(w.visit_expr(&f.expr));
                        }
                    }
                }
            }
        }
        VisitState::Run
    }

    fn visit_ty(&mut self, ty: &Ty) -> VisitState<Self::Error> {
        match ty.node {
            TyKind::Error => __try_dump!(self, "error:"),
            TyKind::Hole => __try_dump!(self, "_"),
            TyKind::Ident(ref name) => __try_dump!(self, "ident: {}", name.as_str()),
            TyKind::App(base, ref args) => {
                __try_dump!(self, "app:");
                {
                    let mut w = self.enter();
                    __try_dump!(w, "base: {}", base.as_str());
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

pub fn dump<T: Write, N: Visit>(w: T, node: &N) -> Result<(), io::Error> {
    let mut dumper = Dumper::new(w);
    dumper.visit(node).into()
}

pub fn dump_to_string<N: Visit>(node: &N) -> String {
    let mut buf = Vec::new();
    {
        let mut dumper = Dumper::new(&mut buf);
        dumper.visit(node).into_result().unwrap();
    }
    String::from_utf8_lossy(&buf).into()
}

#[test]
fn test_dump_to_string() {
    use position::DUMMY_POS;
    let expr = Expr {
        id: DUMMY_NODE_ID,
        node: ExprKind::Literal(Literal::Int(0)),
        pos: DUMMY_POS,
    };
    let actual = dump_to_string(&expr);
    let expected = "int: 0\n";
    assert_eq!(actual, expected);
}
