use std::io::{self, Write};
use std::fmt;

use ast::*;

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

pub trait Visitor {
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
                if let Some(ref ret) = *ret {
                    visit!(self.visit_ty(ret));
                }
                visit!(self.visit_expr(body));
                VisitState::Run
            }
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

    fn enter<'a>(&'a mut self) {
        self.depth += 1;
    }

    fn exit(&mut self) {
        self.depth -= 1;
    }

    pub fn with_indent<V, F>(&mut self, f: F) -> V
        where F: FnOnce(&mut Self) -> V
    {
        self.enter();
        let t = f(self);
        self.exit();
        t
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
        self.with_indent(|this| {
            __try_dump!(this, "name: {}", param.name.as_str());
            __try_dump!(this, "type:");
            this.with_indent(|this| this.visit_ty(&param.ty))
        })
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
                self.with_indent(|this| {
                    __try_dump!(this, "name: {}", f.name.as_str());
                    __try_dump!(this, "type_params:");
                    this.with_indent(|this| {
                        for ty in f.type_params.iter() {
                            __try_dump!(this, "PARAM: {}", ty.as_str());
                        }
                        VisitState::Run
                    });
                    __try_dump!(this, "params:");
                    this.with_indent(|this| {
                        for param in f.params.iter() {
                            visit!(this.dump_param(param));
                        }
                        VisitState::Run
                    });
                    match f.ret {
                        Some(ref ty) => {
                            __try_dump!(this, "return:");
                            this.with_indent(|this| {
                                this.visit_ty(ty)
                            });
                        }
                        None => __try_dump!(this, "return: (default)"),
                    }
                    __try_dump!(this, "body:");
                    this.with_indent(|this| {
                        this.visit_expr(&f.body)
                    })
                })
            }
        }
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
                __try_dump!(self, "prefix op:");
                self.with_indent(|this| {
                    __try_dump!(this, "op: {}", op.as_str());
                    this.visit_expr(e)
                });
            }
            ExprKind::Infix(ref lhs, op, ref rhs) => {
                __try_dump!(self, "infix op:");
                self.with_indent(|this| {
                    __try_dump!(this, "op: {}", op.as_str());
                    visit!(this.visit_expr(lhs));
                    visit!(this.visit_expr(rhs));
                    VisitState::Run
                });
            }
            ExprKind::Paren(ref e) => {
                visit!(self.visit_expr(e));
            }
            ExprKind::Call(ref f, ref args) => {
                __try_dump!(self, "call:");
                self.with_indent(|this| {
                    __try_dump!(this, "function:");
                    this.with_indent(|this| {
                        this.visit_expr(f)
                    });
                    __try_dump!(this, "arguments:");
                    this.with_indent(|this| {
                        for arg in args {
                            visit!(this.visit_expr(arg));
                        }
                        VisitState::Run
                    });
                    VisitState::Run
                });
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
                self.with_indent(|this| {
                    __try_dump!(this, "base: {}", base.as_str());
                    __try_dump!(this, "args:");
                    this.with_indent(|this| {
                        for arg in args {
                            visit!(this.visit_ty(arg));
                        }
                        VisitState::Run
                    });
                    VisitState::Run
                });
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
