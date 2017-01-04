use std::rc::Rc;
use std::cell::RefCell;

use token::{Token, TokenKind};
use symbol::Symbol;
use position::{Pos, Position, File};
use scanner::Scanner;
use errors::{ErrorList, Error};
use ast::{Program, Decl, Param, FunDecl, Type, Expr, Literal};

#[derive(Debug)]
pub struct Parser<'a> {
    scanner: Scanner<'a>,
    errors: Rc<RefCell<ErrorList>>,

    current_token: Token,
    next_token: Token,

    error_count: usize,
}

impl<'a> Parser<'a> {
    pub fn new(file: File, src: &'a str) -> Self {
        let errors = Rc::new(RefCell::new(ErrorList::new()));
        let mut sc = Scanner::new(file, src, errors.clone());
        let cur_token = Token::dummy();
        let next_token = sc.scan();
        Parser {
            scanner: sc,
            errors: errors,

            current_token: cur_token,
            next_token: next_token,

            error_count: 0,
        }
    }

    pub fn into_errors(self) -> ErrorList {
        let errors = self.errors.clone();
        ::std::mem::drop(self);
        Rc::try_unwrap(errors).expect("errors ref count should be 1 here").into_inner()
    }

    fn annotate_error(&mut self, err: Error) {
        self.errors.borrow_mut().add_error(err);
        self.error_count += 1;
    }

    fn position(&self, pos: Pos) -> Position {
        self.scanner.file().position(pos)
    }

    pub fn succ_token(&mut self) {
        self.current_token = self.next_token;
        self.next_token = self.scanner.scan();
    }

    fn next_is(&self, kind: TokenKind) -> bool {
        self.next_token.kind() == kind
    }

    fn expect_next(&mut self, kind: TokenKind) -> Result<(), Error> {
        if self.next_is(kind) {
            self.succ_token();
            Ok(())
        } else {
            let msg = format!("expected {}, got {}", kind, self.next_token.kind());
            let pos = self.next_token.pos();
            Err(Error {
                position: self.position(pos),
                message: msg,
            })
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut decls = Vec::new();
        while !self.next_is(TokenKind::EOF) {
            decls.push(self.parse_decl());
        }
        Program { decls: decls }
    }

    /// succeed tokens and sync next declaration (for error reporting).
    fn sync_decl(&mut self) {
        loop {
            if self.next_is(TokenKind::Def) || self.next_is(TokenKind::Let) ||
               self.next_is(TokenKind::EOF) {
                break;
            }
            self.succ_token();
        }
    }

    pub fn parse_decl(&mut self) -> Decl {
        let result = match self.next_token.kind() {
            TokenKind::Def => self.parse_def(),
            _ => {
                let position = self.position(self.next_token.pos());
                let msg = format!("unexcepted token: {}", self.next_token.kind());
                Err(Error {
                    position: position,
                    message: msg,
                })
            }
        };
        match result {
            Ok(d) => d,
            Err(e) => {
                self.annotate_error(e);
                self.sync_decl();
                Decl::Error
            }
        }
    }

    fn parse_def(&mut self) -> Result<Decl, Error> {
        self.expect_next(TokenKind::Def)?;
        let pos = self.current_token.pos();
        self.expect_next(TokenKind::Ident)?;
        let name = self.current_token.symbol();
        // TODO(agatan): generics parameters
        let type_params = self.parse_optional_type_params()?.unwrap_or(Vec::new());
        let params = self.parse_params()?;
        // TODO(agatan): return type spec
        self.expect_next(TokenKind::Lbrace)?;
        // TODO(gataan): body
        while !self.next_is(TokenKind::Rbrace) {
            self.succ_token();
            if self.next_is(TokenKind::EOF) {
                let pos = self.next_token.pos();
                let position = self.scanner.file().position(pos);
                return Err(Error {
                    position: position,
                    message: "unexpected EOF".to_string(),
                });
            }
        }
        self.succ_token();

        Ok(Decl::Def(pos, FunDecl::new(name, type_params, params)))
    }

    fn parse_param(&mut self) -> Result<Param, Error> {
        self.expect_next(TokenKind::Ident)?;
        let name = self.current_token.symbol();
        self.expect_next(TokenKind::Colon)?;
        let ty = self.parse_type();
        Ok(Param::new(name, ty))
    }

    fn parse_params(&mut self) -> Result<Vec<Param>, Error> {
        self.expect_next(TokenKind::Lparen)?;
        if self.expect_next(TokenKind::Rparen).is_ok() {
            // no arguments.
            return Ok(Vec::new());
        }
        let mut params = Vec::new();
        loop {
            let param = self.parse_param()?;
            params.push(param);
            if self.expect_next(TokenKind::Rparen).is_ok() {
                break;
            }
            self.expect_next(TokenKind::Comma)?;
            if self.expect_next(TokenKind::Rparen).is_ok() {
                // optional trailing comma.
                break;
            }
        }
        Ok(params)
    }

    /// TODO(agatan): Define type AST node.
    fn parse_type_param(&mut self) -> Result<Symbol, Error> {
        self.expect_next(TokenKind::Uident)?;
        Ok(self.current_token.symbol())
    }

    fn parse_optional_type_params(&mut self) -> Result<Option<Vec<Symbol>>, Error> {
        if self.next_is(TokenKind::Langle) {
            self.parse_type_params().map(Some)
        } else {
            Ok(None)
        }
    }

    fn parse_type_params(&mut self) -> Result<Vec<Symbol>, Error> {
        self.expect_next(TokenKind::Langle)?;
        let mut params = Vec::new();
        loop {
            let typ = self.parse_type_param()?;
            params.push(typ);
            if self.expect_next(TokenKind::Rangle).is_ok() {
                break;
            }
            self.expect_next(TokenKind::Comma)?;
            if self.expect_next(TokenKind::Rangle).is_ok() {
                // optional trailing comma.
                break;
            }
        }
        Ok(params)
    }

    pub fn parse_expr(&mut self) -> Expr {
        match self.parse_expr_(Prec::Lowest) {
            Ok(e) => e,
            Err(e) => {
                self.annotate_error(e);
                // TODO(agatan): sync with next exprssesion
                Expr::Error
            }
        }
    }

    fn parse_expr_(&mut self, prec: Prec) -> Result<Expr, Error> {
        match self.next_token.kind() {
            TokenKind::Ident => self.parse_identifier(),
            t => {
                let pos = self.next_token.pos();
                let position = self.position(pos);
                let msg = format!("unexpected token: {}. expression expected.", t);
                Err(Error {
                    position: position,
                    message: msg,
                })
            }
        }
    }

    fn parse_identifier(&mut self) -> Result<Expr, Error> {
        let pos = self.current_token.pos();
        self.expect_next(TokenKind::Ident)?;
        let sym = self.current_token.symbol();
        Ok(Expr::Ident(pos, sym))
    }

    pub fn parse_type(&mut self) -> Type {
        let ty = self.parse_type_();
        match ty {
            Ok(t) => t,
            Err(e) => {
                self.annotate_error(e);
                Type::Error
            }
        }
    }

    fn parse_type_(&mut self) -> Result<Type, Error> {
        self.expect_next(TokenKind::Uident)?;
        let base_sym = self.current_token.symbol();
        let mut ty = Type::from_symbol(base_sym);
        if self.expect_next(TokenKind::Langle).is_ok() {
            let mut args = Vec::new();
            loop {
                let ty = self.parse_type_()?;
                args.push(ty);
                if self.expect_next(TokenKind::Rangle).is_ok() {
                    break;
                }
                self.expect_next(TokenKind::Comma)?;
                if self.expect_next(TokenKind::Rangle).is_ok() {
                    break;
                }
            }
            ty = Type::app(ty, args);
        }
        Ok(ty)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Prec {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    More(usize),
}

#[cfg(test)]
mod tests {
    use super::*;
    use position::File;
    use ast::*;
    use symbol::Symbol;

    #[test]
    fn test_parse_def() {
        let input = r#"
            def f(x: Int) {
            }
        "#;
        let file = File::new(None, input.len());
        let mut parser = Parser::new(file, input);
        let program = parser.parse_program();
        assert_eq!(program.decls.len(),
                   1,
                   "decls size is not 1: {:?}",
                   program.decls);
        let ref decl = program.decls[0];
        let fun_decl = match *decl {
            Decl::Def(_, ref f) => f,
            _ => {
                panic!(format!("expected Decl::Def, got {:?}: error: {:?}",
                               decl,
                               parser.into_errors()))
            }
        };
        assert_eq!(fun_decl.name.as_str(), "f");
        assert_eq!(fun_decl.params.len(), 1);
        assert_eq!(fun_decl.type_params.len(), 0);
    }

    #[test]
    fn test_parse_params() {
        let tests = vec![("()", 0, vec![]),
                         ("(x: Int)", 1, vec!["x"]),
                         ("(x: Int, y: Bool)", 2, vec!["x", "y"])];
        for (input, len, names) in tests {
            let input = format!("def f{} {{ }}", input);
            let file = File::new(None, input.len());
            let mut parser = Parser::new(file, &input);
            let program = parser.parse_program();
            assert_eq!(program.decls.len(),
                       1,
                       "decls size is not 1: {:?}",
                       program.decls);
            let ref decl = program.decls[0];
            let fun_decl = match *decl {
                Decl::Def(_, ref f) => f,
                _ => {
                    panic!(format!("expected Decl::Def, got {:?}: error: {:?}",
                                   decl,
                                   parser.into_errors()))
                }
            };
            assert_eq!(fun_decl.name.as_str(), "f");
            assert_eq!(fun_decl.params.len(), len);
            assert_eq!(fun_decl.type_params.len(), 0);
            for (p, expected) in fun_decl.params.iter().zip(names.into_iter()) {
                assert_eq!(p.name.as_str(), expected);
            }
        }
    }

    #[test]
    fn test_parse_def_with_typeparams() {
        let tests = vec![("", 0, vec![]), ("<T>", 1, vec!["T"]), ("<T, U>", 2, vec!["T", "U"])];
        for (i, (input, len, names)) in tests.into_iter().enumerate() {
            let input = format!("def f{}() {{ }}", input);
            let file = File::new(None, input.len());
            let mut parser = Parser::new(file, &input);
            let program = parser.parse_program();
            assert_eq!(program.decls.len(),
                       1,
                       "decls size is not 1: {:?}",
                       program.decls);
            let ref decl = program.decls[0];
            let fun_decl = match *decl {
                Decl::Def(_, ref f) => f,
                _ => {
                    panic!(format!("test[#{}]: expected Decl::Def, got {:?}: error: {:?}",
                                   i,
                                   decl,
                                   parser.into_errors()))
                }
            };
            assert_eq!(fun_decl.type_params.len(), len);
            for (p, expected) in fun_decl.type_params.iter().zip(names.into_iter()) {
                assert_eq!(p.as_str(), expected);
            }
        }
    }

    #[test]
    fn test_parse_literal() {
        let tests = vec![("32", Literal::Int(32)),
                         (r#""abc""#, Literal::String(Symbol::intern(r#""abc""#))),
                         ("true", Literal::Bool(true)),
                         ("false", Literal::Bool(false))];
        for (i, (input, expected)) in tests.into_iter().enumerate() {
            let file = File::new(None, input.len());
            let mut parser = Parser::new(file, input);
            let e = parser.parse_expr();
            match e {
                Expr::Literal(_, actual) => {
                    assert_eq!(actual, expected, "test[#{}]: input = {}", i, input)
                }
                _ => {
                    assert!(false,
                            "test[#{}]: input = {}, got = {:?}, err = {:?}",
                            i,
                            input,
                            e,
                            parser.into_errors())
                }
            }
        }
    }

    #[test]
    fn test_parse_identifier() {
        let tests = vec!["a", "b", "abc"];
        for (i, input) in tests.into_iter().enumerate() {
            let file = File::new(None, input.len());
            let mut parser = Parser::new(file, input);
            let e = parser.parse_expr();
            match e {
                Expr::Ident(_, actual) => {
                    assert_eq!(actual.as_str(), input, "test[#{}]: input = {}", i, input)
                }
                _ => {
                    assert!(false,
                            "test[#{}]: input = {}, got = {:?}, err = {:?}",
                            i,
                            input,
                            e,
                            parser.into_errors())
                }
            }
        }
    }

    #[test]
    fn test_parse_type() {
        let tests = vec![("Int", Type::Builtin(BuiltinType::Int)),
                         ("Unit", Type::Builtin(BuiltinType::Unit)),
                         ("Bool", Type::Builtin(BuiltinType::Bool)),
                         ("Ref", Type::Ref),
                         ("T", Type::Ident(Symbol::intern("T"))),
                         ("Ref<T>",
                          Type::App(box Type::Ref, vec![Type::Ident(Symbol::intern("T"))]))];

        for (i, (input, expected_ty)) in tests.into_iter().enumerate() {
            let file = File::new(None, input.len());
            let mut parser = Parser::new(file, &input);
            let ty = parser.parse_type();
            assert_eq!(ty,
                       expected_ty,
                       "test[#{}]: input = {}, err = {:?}",
                       i,
                       input,
                       parser.into_errors());
        }
    }
}
