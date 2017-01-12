use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use token::{Token, TokenKind};
use symbol::Symbol;
use position::{Pos, Position, File};
use scanner::Scanner;
use errors::{ErrorList, Error};
use ast::*;

pub struct Parser<'a> {
    scanner: Scanner<'a>,
    errors: Rc<RefCell<ErrorList>>,

    current_token: Token,
    next_token: Token,

    error_count: usize,

    prefix_func_map: HashMap<TokenKind, PrefixFn<'a>>,
    infix_func_map: HashMap<TokenKind, InfixFn<'a>>,

    last_node_id: NodeId,
}

type PrefixFn<'a> = fn(&mut Parser<'a>) -> Result<Expr, Error>;
type InfixFn<'a> = fn(&mut Parser<'a>, Expr) -> Result<Expr, Error>;

impl<'a> Parser<'a> {
    pub fn new(file: File, src: &'a str) -> Self {
        let errors = Rc::new(RefCell::new(ErrorList::new()));
        let mut sc = Scanner::new(file, src, errors.clone());
        let cur_token = Token::dummy();
        let next_token = sc.scan();
        let mut parser = Parser {
            scanner: sc,
            errors: errors,

            current_token: cur_token,
            next_token: next_token,

            error_count: 0,

            prefix_func_map: HashMap::new(),
            infix_func_map: HashMap::new(),

            last_node_id: NodeId::new(0),
        };
        parser.register_prefix(TokenKind::Int, Self::parse_integer_literal);
        parser.register_prefix(TokenKind::String, Self::parse_string_literal);
        parser.register_prefix(TokenKind::True, Self::parse_bool_literal);
        parser.register_prefix(TokenKind::False, Self::parse_bool_literal);
        parser.register_prefix(TokenKind::Ident, Self::parse_identifier);
        parser.register_prefix(TokenKind::Operator, Self::parse_prefix_expr);
        parser.register_prefix(TokenKind::Lparen, Self::parse_paren_expr);
        parser.register_prefix(TokenKind::Lbrace, Self::parse_block_expr);

        parser.register_infix(TokenKind::Operator, Self::parse_infix_expr);
        parser.register_infix(TokenKind::Langle, Self::parse_infix_expr);
        parser.register_infix(TokenKind::Rangle, Self::parse_infix_expr);
        parser.register_infix(TokenKind::Lparen, Self::parse_call_expr);
        parser.register_infix(TokenKind::Dot, Self::parse_ufcs_expr);
        parser
    }

    fn register_prefix(&mut self, kind: TokenKind, f: PrefixFn<'a>) {
        self.prefix_func_map.insert(kind, f);
    }

    fn register_infix(&mut self, kind: TokenKind, f: InfixFn<'a>) {
        self.infix_func_map.insert(kind, f);
    }

    fn get_prefix_fn(&self, token: &Token) -> Option<PrefixFn<'a>> {
        self.prefix_func_map.get(&token.kind()).map(|&f| f)
    }

    fn get_infix_fn(&self, token: &Token) -> Option<InfixFn<'a>> {
        self.infix_func_map.get(&token.kind()).map(|&f| f)
    }

    fn make_error(&self, token: Token, msg: String) -> Error {
        let pos = token.pos();
        let position = self.position(pos);
        Error {
            position: position,
            message: msg,
        }
    }

    pub fn errors(&self) -> ::std::cell::Ref<ErrorList> {
        self.errors.borrow()
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

    fn next_id(&mut self) -> NodeId {
        let id = self.last_node_id.as_u32();
        self.last_node_id = NodeId::from_u32(id + 1);
        self.last_node_id
    }

    fn position(&self, pos: Pos) -> Position {
        self.scanner.file().position(pos)
    }

    pub fn succ_token(&mut self) {
        self.current_token = self.next_token;
        self.next_token = self.scanner.scan();
    }

    fn skip_newlines(&mut self) {
        while self.next_is(TokenKind::Newline) {
            self.succ_token()
        }
    }

    fn next_is(&self, kind: TokenKind) -> bool {
        self.next_token.kind() == kind
    }

    fn next_is_separator(&self) -> bool {
        let kind = self.next_token.kind();
        kind == TokenKind::EOF || kind == TokenKind::Newline || kind == TokenKind::Semicolon ||
        kind == TokenKind::Rparen || kind == TokenKind::Rbrace
    }

    fn expect_without_newline(&mut self, kind: TokenKind) -> Result<(), Error> {
        self.skip_newlines();
        self.expect(kind)
    }

    fn expect(&mut self, kind: TokenKind) -> Result<(), Error> {
        if self.next_is(kind) {
            self.succ_token();
            Ok(())
        } else {
            let msg = format!("expected {}, got {}", kind, self.next_token);
            Err(self.make_error(self.next_token, msg))
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
        self.skip_newlines();
        let result = match self.next_token.kind() {
            TokenKind::Def => self.parse_def(),
            TokenKind::Struct => self.parse_struct_decl(),
            _ => {
                let msg = format!("unexcepted token: {}", self.next_token);
                Err(self.make_error(self.next_token, msg))
            }
        };
        match result {
            Ok(d) => d,
            Err(e) => {
                self.annotate_error(e);
                let pos = self.next_token.pos();
                self.sync_decl();
                Decl::new(self.next_id(), pos, DeclKind::Error)
            }
        }
    }

    fn parse_def(&mut self) -> Result<Decl, Error> {
        self.expect_without_newline(TokenKind::Def)?;
        let pos = self.current_token.pos();
        self.expect_without_newline(TokenKind::Ident)?;
        let name = self.current_token.symbol();
        let type_params = self.parse_optional_type_params()?.unwrap_or(Vec::new());
        let params = self.parse_params()?;
        let ret_ty = if self.expect_without_newline(TokenKind::Colon).is_ok() {
            Some(self.parse_type())
        } else {
            None
        };
        self.expect_without_newline(TokenKind::Eq)?;
        let body = self.parse_expr();
        self.succ_token();

        Ok(Decl::new(self.next_id(),
                     pos,
                     DeclKind::Def(FunDecl::new(name, type_params, params, ret_ty, body))))
    }

    fn parse_field(&mut self) -> Result<Field, Error> {
        self.expect_without_newline(TokenKind::Ident)?;
        let name = self.current_token.symbol();
        self.expect_without_newline(TokenKind::Colon)?;
        let ty = self.parse_type();
        if self.expect_without_newline(TokenKind::Comma).is_err() &&
           !self.next_is(TokenKind::Rbrace) {
            let err = self.make_error(self.next_token,
                                      format!("expected ',' or '}}'. found {}", self.next_token));
            Err(err)
        } else {
            Ok(Field {
                name: name,
                ty: ty,
            })
        }
    }

    fn parse_fields(&mut self) -> Result<Vec<Field>, Error> {
        self.expect_without_newline(TokenKind::Lbrace)?;
        if self.expect_without_newline(TokenKind::Rbrace).is_ok() {
            return Ok(Vec::new());
        }
        let mut fields = Vec::new();
        while self.expect_without_newline(TokenKind::Rbrace).is_err() {
            let field = self.parse_field()?;
            fields.push(field);
            if self.next_is(TokenKind::EOF) {
                let err = self.make_error(self.next_token, "expected '}'. found 'EOF'".into());
                return Err(err);
            }
        }
        Ok(fields)
    }

    fn parse_struct_decl(&mut self) -> Result<Decl, Error> {
        self.expect(TokenKind::Struct)?;
        let pos = self.current_token.pos();
        self.expect_without_newline(TokenKind::Uident)?;
        let name = self.current_token.symbol();
        let type_params = self.parse_optional_type_params()?.unwrap_or(Vec::new());
        let fields = self.parse_fields()?;
        Ok(Decl::new(self.next_id(),
                     pos,
                     DeclKind::Struct(Struct {
                         name: name,
                         type_params: type_params,
                         fields: fields,
                     })))
    }

    fn parse_param(&mut self) -> Result<Param, Error> {
        self.expect_without_newline(TokenKind::Ident)?;
        let name = self.current_token.symbol();
        self.expect_without_newline(TokenKind::Colon)?;
        let ty = self.parse_type();
        Ok(Param::new(name, ty))
    }

    fn parse_params(&mut self) -> Result<Vec<Param>, Error> {
        self.expect_without_newline(TokenKind::Lparen)?;
        if self.expect_without_newline(TokenKind::Rparen).is_ok() {
            // no arguments.
            return Ok(Vec::new());
        }
        let mut params = Vec::new();
        loop {
            let param = self.parse_param()?;
            params.push(param);
            if self.expect_without_newline(TokenKind::Rparen).is_ok() {
                break;
            }
            self.expect_without_newline(TokenKind::Comma)?;
            if self.expect_without_newline(TokenKind::Rparen).is_ok() {
                // optional trailing comma.
                break;
            }
        }
        Ok(params)
    }

    /// TODO(agatan): Define type AST node.
    fn parse_type_param(&mut self) -> Result<Symbol, Error> {
        self.expect_without_newline(TokenKind::Uident)?;
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
        self.expect_without_newline(TokenKind::Langle)?;
        let mut params = Vec::new();
        loop {
            let typ = self.parse_type_param()?;
            params.push(typ);
            if self.expect_without_newline(TokenKind::Rangle).is_ok() {
                break;
            }
            self.expect_without_newline(TokenKind::Comma)?;
            if self.expect_without_newline(TokenKind::Rangle).is_ok() {
                // optional trailing comma.
                break;
            }
        }
        Ok(params)
    }

    pub fn parse_stmt(&mut self) -> Stmt {
        // always in block expressions
        // TODO(agatan): currently, Stmt only consists of Expr.
        let e = self.parse_expr();
        let pos = e.pos;
        let node = if self.expect(TokenKind::Semicolon).is_ok() {
            StmtKind::Semi(e)
        } else if self.expect(TokenKind::Newline).is_ok() {
            StmtKind::Expr(e)
        } else if self.next_is(TokenKind::Rbrace) {
            // `{ 1 }` style block
            StmtKind::Expr(e)
        } else {
            let err = self.make_error(self.next_token,
                                      format!("unexpected token: {}. expected ',' or line break",
                                              self.next_token));
            self.annotate_error(err);
            StmtKind::Error
        };
        Stmt::new(self.next_id(), pos, node)
    }

    pub fn parse_expr(&mut self) -> Expr {
        match self.parse_expr_(Assoc::lowest()) {
            Ok(e) => e,
            Err(e) => {
                self.annotate_error(e);
                // TODO(agatan): sync with next exprssesion
                Expr::error()
            }
        }
    }

    fn parse_expr_(&mut self, assoc: Assoc) -> Result<Expr, Error> {
        self.skip_newlines();
        let prefix = match self.get_prefix_fn(&self.next_token) {
            Some(f) => f,
            None => {
                let msg = format!("unexpected token: {}. expression expected.",
                                  self.next_token);
                return Err(self.make_error(self.next_token, msg));
            }
        };
        let mut left = prefix(self)?;
        while !self.next_is_separator() {
            let next_assoc = Assoc::infix_token(self.next_token);
            if next_assoc.prec < assoc.prec {
                break;
            }
            if assoc.fixity == Fixity::Left && next_assoc.prec == assoc.prec {
                break;
            }
            let infix = match self.get_infix_fn(&self.next_token) {
                Some(f) => f,
                None => {
                    break;
                }
            };
            left = infix(self, left)?;
        }
        Ok(left)
    }

    fn parse_paren_expr(&mut self) -> Result<Expr, Error> {
        self.expect_without_newline(TokenKind::Lparen)?;
        let pos = self.current_token.pos();
        let e = self.parse_expr_(Assoc::lowest())?;
        self.expect_without_newline(TokenKind::Rparen)?;
        Ok(Expr::new(self.next_id(), pos, ExprKind::Paren(box e)))
    }

    fn parse_integer_literal(&mut self) -> Result<Expr, Error> {
        self.expect_without_newline(TokenKind::Int)?;
        let pos = self.current_token.pos();
        let sym = self.current_token.symbol();
        let num = sym.as_str().parse::<i64>().expect("integer parse error");
        Ok(Expr::new(self.next_id(), pos, ExprKind::Literal(Literal::Int(num))))
    }

    fn parse_string_literal(&mut self) -> Result<Expr, Error> {
        self.succ_token();
        let pos = self.current_token.pos();
        let sym = self.current_token.symbol();
        Ok(Expr::new(self.next_id(), pos, ExprKind::Literal(Literal::String(sym))))
    }

    fn parse_bool_literal(&mut self) -> Result<Expr, Error> {
        self.succ_token();
        let pos = self.current_token.pos();
        let lit = if self.current_token.kind() == TokenKind::True {
            Literal::Bool(true)
        } else {
            Literal::Bool(false)
        };
        Ok(Expr::new(self.next_id(), pos, ExprKind::Literal(lit)))
    }

    fn parse_identifier(&mut self) -> Result<Expr, Error> {
        self.expect_without_newline(TokenKind::Ident)?;
        let pos = self.current_token.pos();
        let sym = self.current_token.symbol();
        Ok(Expr::new(self.next_id(), pos, ExprKind::Ident(sym)))
    }

    fn parse_prefix_expr(&mut self) -> Result<Expr, Error> {
        if !self.next_token.is_prefix_operator() {
            return Err(self.make_error(self.next_token,
                                       format!("unexpected token: {}. Prefix operator should \
                                                starts with '-', '!' or '@'",
                                               self.next_token)));
        }
        self.succ_token();
        let pos = self.current_token.pos();
        let op = self.current_token.symbol();
        let expr = self.parse_expr_(Assoc {
                fixity: Fixity::Right,
                prec: Prec::Prefix,
            })?;
        Ok(Expr::new(self.next_id(), pos, ExprKind::Prefix(op, box expr)))
    }

    fn parse_block_expr(&mut self) -> Result<Expr, Error> {
        self.expect(TokenKind::Lbrace)?;
        let pos = self.current_token.pos();
        let mut stmts = Vec::new();
        while self.expect_without_newline(TokenKind::Rbrace).is_err() {
            let stmt = self.parse_stmt();
            stmts.push(stmt);
            if self.next_is(TokenKind::EOF) {
                let err = self.make_error(self.next_token, "expected '}', found 'EOF'".into());
                return Err(err);
            }
        }
        Ok(Expr::new(self.next_id(), pos, ExprKind::Block(stmts)))
    }

    fn parse_infix_expr(&mut self, left: Expr) -> Result<Expr, Error> {
        let assoc = Assoc::infix_token(self.next_token);
        let pos = self.next_token.pos();
        let op = self.next_token.symbol();
        self.succ_token();
        let right = self.parse_expr_(assoc)?;
        Ok(Expr::new(self.next_id(),
                     pos,
                     ExprKind::Infix(box left, op, box right)))
    }

    fn parse_call_expr(&mut self, f: Expr) -> Result<Expr, Error> {
        let pos = f.pos;
        let args = self.parse_arguments()?;
        Ok(Expr::new(self.next_id(), pos, ExprKind::Call(box f, args)))
    }

    fn parse_ufcs_expr(&mut self, base: Expr) -> Result<Expr, Error> {
        let pos = base.pos;
        self.expect(TokenKind::Dot)?;
        self.expect_without_newline(TokenKind::Ident)?;
        let fname = self.current_token.symbol();
        let args = self.parse_arguments()?;
        Ok(Expr::new(self.next_id(), pos, ExprKind::Ufcs(box base, fname, args)))
    }

    fn parse_arguments(&mut self) -> Result<Vec<Expr>, Error> {
        self.expect(TokenKind::Lparen)?;
        let mut args = Vec::new();
        loop {
            if self.expect_without_newline(TokenKind::Rparen).is_ok() {
                break;
            }
            let e = self.parse_expr_(Assoc::lowest())?;
            args.push(e);
            if self.expect(TokenKind::Rparen).is_ok() {
                break;
            }
            self.expect(TokenKind::Comma)?;
        }
        Ok(args)
    }

    pub fn parse_type(&mut self) -> Ty {
        let ty = self.parse_type_();
        match ty {
            Ok(t) => t,
            Err(e) => {
                self.annotate_error(e);
                Ty::error()
            }
        }
    }

    fn parse_type_(&mut self) -> Result<Ty, Error> {
        self.expect_without_newline(TokenKind::Uident)?;
        let pos = self.current_token.pos();
        let base_sym = self.current_token.symbol();
        let node = if self.expect(TokenKind::Langle).is_ok() {
            let mut args = Vec::new();
            loop {
                let ty = self.parse_type_()?;
                args.push(ty);
                if self.expect_without_newline(TokenKind::Rangle).is_ok() {
                    break;
                }
                self.expect_without_newline(TokenKind::Comma)?;
                if self.expect_without_newline(TokenKind::Rangle).is_ok() {
                    break;
                }
            }
            TyKind::App(base_sym, args)
        } else {
            TyKind::Ident(base_sym)
        };
        Ok(Ty {
            id: self.next_id(),
            node: node,
            pos: pos,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Prec {
    Lowest,

    /// assign operations
    Assign,
    /// `|`
    Infix0,
    /// `^`
    Infix1,
    /// `&`
    Infix2,
    /// `<`, `>`
    Infix3,
    /// `=`, `!`
    Infix4,
    /// `:`
    Infix5,
    /// `+`, `-`
    Infix6,
    /// `*`, `/`, `%`
    Infix7,
    /// other special characters
    Infix8,

    Prefix,
    /// call expression
    Call,
    /// never reduced
    Highest,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Fixity {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Assoc {
    fixity: Fixity,
    prec: Prec,
}

impl Assoc {
    fn lowest() -> Self {
        Assoc {
            fixity: Fixity::Left,
            prec: Prec::Lowest,
        }
    }

    fn infix_token(token: Token) -> Assoc {
        match token.kind() {
            TokenKind::Lparen | TokenKind::Dot => {
                return Assoc {
                    fixity: Fixity::Left,
                    prec: Prec::Call,
                };
            }
            _ => (),
        }
        let s = &*token.symbol().as_str();
        let mut prec = match token.kind() {
            TokenKind::Langle | TokenKind::Rangle => Prec::Infix3,
            TokenKind::Operator => {
                let c = s.chars().next().unwrap();
                match c {
                    '|' => Prec::Infix0,
                    '^' => Prec::Infix1,
                    '&' => Prec::Infix2,
                    '<' | '>' => Prec::Infix3,
                    '=' | '!' => Prec::Infix4,
                    ':' => Prec::Infix5,
                    '+' | '-' => Prec::Infix6,
                    '*' | '/' | '%' => Prec::Infix7,
                    _ => Prec::Infix8,
                }
            }
            _ => Prec::Highest,
        };
        let mut fixity = if s.ends_with(':') {
            Fixity::Right
        } else {
            Fixity::Left
        };

        // exceptional rule for assignmanets.
        if s.ends_with('=') {
            fixity = Fixity::Right;
            prec = Prec::Assign;
        }

        Assoc {
            fixity: fixity,
            prec: prec,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use position::{DUMMY_POS, File};
    use symbol::Symbol;

    #[test]
    fn test_parse_def() {
        let input = r#"
            def f(x: Int) = {
                x
            }
        "#;
        let file = File::new(None, input.len());
        let mut parser = Parser::new(file, input);
        let program = parser.parse_program();
        assert_eq!(program.decls.len(),
                   1,
                   "decls size is not 1: {:?}",
                   program.decls);
        test_parse_error(&parser);
        let ref decl = program.decls[0];
        let fun_decl = match decl.node {
            DeclKind::Def(ref f) => f,
            _ => panic!(format!("expected DeclKind::Def, got {:?}", decl)),
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
            let input = format!("def f{} = {{ 1 }}", input);
            let file = File::new(None, input.len());
            let mut parser = Parser::new(file, &input);
            let program = parser.parse_program();
            test_parse_error(&parser);
            assert_eq!(program.decls.len(),
                       1,
                       "decls size is not 1: {:?}",
                       program.decls);
            let ref decl = program.decls[0];
            let fun_decl = match decl.node {
                DeclKind::Def(ref f) => f,
                _ => panic!(format!("expected DeclKind::Def, got {:?}", decl)),
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
            let input = format!("def f{}() = {{ 1 }}", input);
            let file = File::new(None, input.len());
            let mut parser = Parser::new(file, &input);
            let program = parser.parse_program();
            test_parse_error(&parser);
            assert_eq!(program.decls.len(),
                       1,
                       "decls size is not 1: {:?}",
                       program.decls);
            let ref decl = program.decls[0];
            let fun_decl = match decl.node {
                DeclKind::Def(ref f) => f,
                _ => panic!(format!("test[#{}]: expected DeclKind::Def, got {:?}", i, decl)),
            };
            assert_eq!(fun_decl.type_params.len(), len);
            for (p, expected) in fun_decl.type_params.iter().zip(names.into_iter()) {
                assert_eq!(p.as_str(), expected);
            }
        }
    }

    #[test]
    fn test_parse_struct() {
        let input = r#"
        struct Pair<T, U> {
            first: T,
            second: U,
        }
        "#;
        let d = super::super::parse_decl(input).unwrap();
        if let DeclKind::Struct(ref st) = d.node {
            assert_eq!(st.name.as_str(), "Pair");
            assert_eq!(st.type_params.len(), 2);
            for (act, exp) in st.type_params.iter().zip(vec!["T", "U"].into_iter()) {
                assert_eq!(act.as_str(), exp);
            }
            for (act, exp) in st.fields.iter().zip(vec!["first", "second"].into_iter()) {
                assert_eq!(act.name.as_str(), exp);
            }
        } else {
            panic!("d is not a struct declaration, got = {:?}", d.node);
        }
    }

    #[test]
    fn test_parse_stmt() {
        let expected_expr = Expr::new(NodeId::new(1),
                                      DUMMY_POS,
                                      ExprKind::Literal(Literal::Int(1)));
        let tests = vec![("1\n", StmtKind::Expr(expected_expr.clone())),
                         ("1;", StmtKind::Semi(expected_expr.clone()))];
        for (input, expected) in tests {
            let file = File::new(None, input.len());
            let mut parser = Parser::new(file, input);
            let s = parser.parse_stmt();
            test_parse_error(&parser);
            assert_eq!(s.node, expected);
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
            test_parse_error(&parser);
            match e.node {
                ExprKind::Literal(actual) => {
                    assert_eq!(actual, expected, "test[#{}]: input = {:?}", i, input)
                }
                _ => panic!("test[#{}]: input = {:?}, got = {:?}", i, input, e),
            }
        }
    }

    #[test]
    fn test_parse_identifier() {
        let tests = vec!["a", "b", "abc"];
        for input in tests {
            let file = File::new(None, input.len());
            let mut parser = Parser::new(file, input);
            let e = parser.parse_expr();
            test_identifier(input, e);
        }
    }

    #[test]
    fn test_prefix_expr() {
        let tests = vec![// builtin operators
                         ("!false", "!", Literal::Bool(false)),
                         ("-12", "-", Literal::Int(12)),
                         ("@0", "@", Literal::Int(0)),
                         // user defined operators
                         ("@~0", "@~", Literal::Int(0)),
                         ("!~0", "!~", Literal::Int(0))];
        for (i, (input, expected_op, expected_lit)) in tests.into_iter().enumerate() {
            let file = File::new(None, input.len());
            let mut parser = Parser::new(file, input);
            let e = parser.parse_expr();
            test_parse_error(&parser);
            if let ExprKind::Prefix(op, box Expr { node: ExprKind::Literal(lit), .. }) = e.node {
                assert_eq!(op.as_str(),
                           expected_op,
                           "test[#{}]: input = {:?}, got op = {:?}",
                           i,
                           input,
                           op);
                assert_eq!(lit,
                           expected_lit,
                           "test[#{}]: input = {:?}, got lit = {:?}",
                           i,
                           input,
                           lit);
            } else {
                panic!("test[#{}]: input = {:?}, got = {:?}", i, input, e)
            }
        }
    }

    #[test]
    fn test_infix_expr() {
        let tests = vec![("5 + 5", 5, "+", 5),
                         ("5 - 5", 5, "-", 5),
                         ("5 * 5", 5, "*", 5),
                         ("5 / 5", 5, "/", 5),
                         ("5 > 5", 5, ">", 5),
                         ("5 == 5", 5, "==", 5),
                         ("5 := 5", 5, ":=", 5),
                         ("5 != 5", 5, "!=", 5),
                         ("5 | 5", 5, "|", 5),
                         ("5 || 5", 5, "||", 5)];
        for (i, (input, expected_lhs, expected_op, expected_rhs)) in tests.into_iter().enumerate() {
            let file = File::new(None, input.len());
            let mut parser = Parser::new(file, input);
            let e = parser.parse_expr();
            test_parse_error(&parser);
            match e.node {
                ExprKind::Infix(box lhs, op, box rhs) => {
                    test_integer(expected_lhs, lhs);
                    test_integer(expected_rhs, rhs);
                    assert_eq!(op.as_str(),
                               expected_op,
                               "test[#{}]: op is not {:?}, got {:?}",
                               i,
                               expected_op,
                               op);
                }
                _ => {
                    panic!("test[#{}]: expression is not an infix expression. got = {:?}",
                           i,
                           e);
                }
            }
        }
    }

    #[test]
    fn test_infix_precedence() {
        let input = "1 + 2 :: 3";
        let file = File::new(None, input.len());
        let mut parser = Parser::new(file, input);
        let e = parser.parse_expr();
        test_parse_error(&parser);
        let (left, right) = match e.node {
            ExprKind::Infix(box left, op, box right) => {
                assert_eq!(op.as_str(), "::");
                (left, right)
            }
            _ => panic!("expression is not an infix expression: got = {:?}", e.node),
        };
        test_integer(3, right);
        match left.node {
            ExprKind::Infix(box left, op, box right) => {
                assert_eq!(op.as_str(), "+");
                test_integer(1, left);
                test_integer(2, right);
            }
            _ => {
                panic!("left expression is not an infix expression: got = {:?}",
                       left.node)
            }
        }
    }

    #[test]
    fn test_parse_paren() {
        let tests = vec!["(1 + 2)", "(\n 1 + 2 )", "(1 + \n 2)", "(1 + 2 \n )"];
        for (i, input) in tests.iter().enumerate() {
            let file = File::new(None, input.len());
            let mut parser = Parser::new(file, &input);
            let e = parser.parse_expr();
            test_parse_error(&parser);
            match e.node {
                ExprKind::Paren(_) => (),
                _ => panic!("test[#{}]: input = {}, got = {:?}", i, input, e),
            }
        }

        {
            let input = "(1 - 2) * 3";
            let file = File::new(None, input.len());
            let mut parser = Parser::new(file, input);
            let e = parser.parse_expr();
            test_parse_error(&parser);
            if let ExprKind::Infix(box left, _, _) = e.node {
                if let ExprKind::Paren(_) = left.node {
                    // ok
                } else {
                    panic!("left is not a paren expression, got = {:?}", left.node);
                }
            } else {
                panic!("e is not an infix expression, got = {:?}", e.node);
            }
        }
    }

    #[test]
    fn test_parse_call() {
        let tests = vec![("f()", vec![]),
                         ("f(1)", vec![1]),
                         ("f(1, 2)", vec![1, 2]),
                         ("f(1, 2,)", vec![1, 2]),
                         ("f(\n1,\n 2,\n)", vec![1, 2])];
        for (i, (input, n)) in tests.into_iter().enumerate() {
            let file = File::new(None, input.len());
            let mut parser = Parser::new(file, &input);
            let e = parser.parse_expr();
            test_parse_error(&parser);
            match e.node {
                ExprKind::Call(f, args) => {
                    test_identifier("f", *f);
                    for (arg, expected) in args.into_iter().zip(n.into_iter()) {
                        test_integer(expected, arg);
                    }
                }
                _ => panic!("test[#{}]: expression is not a call: got = {:?}", i, e),
            }
        }
    }

    #[test]
    fn test_parse_ufcs() {
        let input = "x.f(1, 2).g()";
        let file = File::new(None, input.len());
        let mut parser = Parser::new(file, input);
        let e = parser.parse_expr();
        test_parse_error(&parser);
        match e.node {
            ExprKind::Ufcs(box base, fname, args) => {
                assert_eq!("g", fname.as_str());
                assert_eq!(args.len(), 0);
                match base.node {
                    ExprKind::Ufcs(box base, fname, args) => {
                        assert_eq!("f", fname.as_str());
                        assert_eq!(args.len(), 2);
                        for (expected, arg) in vec![1, 2].into_iter().zip(args.into_iter()) {
                            test_integer(expected, arg);
                        }
                        test_identifier("x", base);
                    }
                    _ => panic!("base is not an UFCS expression, got = {:?}", base.node),
                }
            }
            _ => panic!("e is not an UFCS expression, got = {:?}", e.node),
        }
    }

    #[test]
    fn test_parse_block_expr() {
        let one = ExprKind::Literal(Literal::Int(1));
        let tests = vec![r#"
            {
                1;
                1
            }
            "#,

                         r#" { 1; 1 }"#,

                         r#" {
            1;
            1 }"#];
        for input in tests {
            let file = File::new(None, input.len());
            let mut parser = Parser::new(file, input);
            let e = parser.parse_expr();
            test_parse_error(&parser);
            if let ExprKind::Block(stmts) = e.node {
                assert_eq!(stmts.len(), 2);
                if let StmtKind::Semi(ref first) = stmts[0].node {
                    assert_eq!(first.node, one);
                } else {
                    panic!("first statement is not a semi statement, got = {:?}",
                           stmts[0].node);
                }
                if let StmtKind::Expr(ref second) = stmts[1].node {
                    assert_eq!(second.node, one);
                } else {
                    panic!("second statement is not a semi statement, got = {:?}",
                           stmts[1].node);
                }
            } else {
                panic!("e is not a block expression, got = {:?}", e);
            }
        }
    }

    #[test]
    fn test_parse_app_type() {
        let input = "Ref<T>";

        let file = File::new(None, input.len());
        let mut parser = Parser::new(file, &input);
        let ty = parser.parse_type();
        test_parse_error(&parser);
        match ty.node {
            TyKind::App(base, args) => {
                assert_eq!(base.as_str(), "Ref");
                assert!(args.len() == 1);
                match args[0].node {
                    TyKind::Ident(sym) => assert_eq!(sym.as_str(), "T"),
                    _ => panic!("App's argument is not Ident(T), got = {:?}", args[0]),
                }
            }
            _ => panic!("ty is not an App, got = {:?}", ty.node),
        }
    }

    fn test_identifier(expected: &str, e: Expr) {
        match e.node {
            ExprKind::Ident(name) => assert_eq!(name.as_str(), expected),
            _ => panic!("expression is not an identifier: got {:?}", e),
        }
    }

    fn test_integer(expected: i64, e: Expr) {
        match e.node {
            ExprKind::Literal(Literal::Int(i)) => assert_eq!(i, expected),
            _ => panic!("expression is not an integer literal: got {:?}", e),
        }
    }

    fn test_parse_error<'a>(p: &Parser<'a>) {
        assert!(p.errors().is_empty(), "parse error: {:?}", p.errors());
    }
}
