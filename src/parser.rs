

use std::rc::Rc;
use core::fmt::Debug;

use crate::lexer;
use crate::err;
use crate::DEBUG;

use lexer::Token;


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeKind {
    Integer,
    Float,
}

#[derive(Copy, Clone)]
pub union Value {
    pub i: i64,
    pub f: f64,
}

impl Value {
    pub unsafe fn add(self, r: Value, ty: &TypeKind) -> Self {
        match ty {
            TypeKind::Integer => (self.i + r.i).into(),
            TypeKind::Float => (self.f + r.f).into(),
        }
    }
    pub unsafe fn sub(self, r: Value, ty: &TypeKind) -> Self {
        match ty {
            TypeKind::Integer => (self.i - r.i).into(),
            TypeKind::Float => (self.f - r.f).into(),
        }
    }
    pub unsafe fn mul(self, r: Value, ty: &TypeKind) -> Self {
        match ty {
            TypeKind::Integer => (self.i * r.i).into(),
            TypeKind::Float => (self.f * r.f).into(),
        }
    }
    pub unsafe fn div(self, r: Value, ty: &TypeKind) -> Self {
        match ty {
            TypeKind::Integer => (self.i / r.i).into(),
            TypeKind::Float => (self.f / r.f).into(),
        }
    }
    pub unsafe fn pow(self, pow: Value, ty: &TypeKind) -> Self {
        match ty {
            TypeKind::Integer => self.i.pow(pow.i as u32).into(),
            TypeKind::Float => self.f.powf(pow.f).into(),
        }
    }

    pub unsafe fn as_float(&mut self) {
        self.f = self.i as f64;
    }
    pub unsafe fn as_int(&mut self) {
        self.i = self.f as i64;
    }
    pub unsafe fn is_negative(&self, ty: &TypeKind) -> bool {
        match ty {
            TypeKind::Integer => self.i < 0,
            TypeKind::Float => self.f < 0.,
        }
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self { Value { f: value } }
}
impl From<i64> for Value {
    fn from(value: i64) -> Self { Value { i: value } }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return unsafe {
            f.debug_struct("Value")
                .field("i", &self.i)
                .field("f", &self.f)
                .finish()
        };
    }
}


#[derive(Debug, Clone)]
pub enum AstNode {
    Identifier { id: String },
    FnIdent { id: String, params: Rc<AstNode> },
    Literal { kind: TypeKind, data: Value },
    Csv { values: Vec<AstNode> },

    //SolveAssign {  },
    VarAssign { id_expr: Rc<AstNode>, assign: Rc<AstNode> },
    BinaryExpr { left: Rc<AstNode>, right: Rc<AstNode>, op: String },
    UnaryExpr { sign: String, value: Rc<AstNode> },
    FnCall { id: String, args: Rc<AstNode> },
    BlockExpr { body: Vec<AstNode>, is_fn: bool },

    Void
}

pub struct Ast {
    pub nodes   :   Vec<AstNode>,
    pub tokens  :   Vec<Token>,
    pub fns     :   Vec<String>,
    pub at      :   usize,
}

use Token::*;
impl Ast {
    fn parse_root(&mut self) -> AstNode {
        let node = self.parse_keyword();
        if self.next_is(End) {
            self.next();
        }

        return node;
    }

    fn parse_keyword(&mut self) -> AstNode {
        match self.at() {
            Ident(id) if id == "fn" => {
                let left = self.parse_fn();
                if !self.has_next() || !self.next_is(Equals) { return left; }
                self.next();
                self.check_next("Expected expression after =, but found nothing");
                let mut assign = Rc::new(self.parse_add());
                if let AstNode::BlockExpr { is_fn,.. } = Rc::get_mut(&mut assign).unwrap() {
                    *is_fn = true;
                }
                if let AstNode::FnIdent { id,.. } = &left {
                    self.fns.push(id.clone());
                }

                return AstNode::VarAssign {
                    id_expr: Rc::new(left),
                    assign
                };
            },
            Ident(id) if id == "let" => {
                todo!()
            }
            _ => self.parse_assign()
        }
    }

    fn parse_fn(&mut self) -> AstNode {
        if *self.at() != Ident("".into()) { return self.parse_add(); }
        if let Ident(id) = self.at() { if id != "fn" { return self.parse_add(); }}

        self.expect(Ident("".into()), "Expected function name after fn keyword");
        let id = if let AstNode::Identifier{id} = self.parse_primary() {id} else {
            err!(fatal "Expected identifier, but didnt find one... HOW DID THIS HAPPEN");
        };

        self.expect(Paren(true), "Expected ( after function name");
        self.check_next("Expected function parameters or )");

        let params = if *self.at() != Paren(false) {
            let p = self.parse_csv();
            self.expect(Paren(false), "Expected ) after parameters");
            p
        } else { AstNode::Void };

        return AstNode::FnIdent {
            id,
            params: params.into()
        };
    }

    fn parse_csv(&mut self) -> AstNode {
        let mut val = self.parse_assign();
        if !self.has_next() || !self.next_is(Comma) { return val; }
        let mut csv = vec![val];

        while self.has_next() && self.next_is(Comma) {
            self.next();
            self.check_next("Expected value after , trailing commas arent allowed");
            val = self.parse_assign();
            csv.push(val);
        }

        return AstNode::Csv { values: csv }; 
    }

    fn parse_assign(&mut self) -> AstNode {
        let left = self.parse_add();
        if !self.has_next() || !self.next_is(Equals) { return left; }
        self.next();
        self.check_next("Expected expression after =, but found nothing");
        let assign = Rc::new(self.parse_add());
        if self.is_end() {
            self.next();
        }

        return AstNode::VarAssign {
            id_expr: Rc::new(left),
            assign
        };
    }


    fn parse_add(&mut self) -> AstNode {
        let mut node = self.parse_mult();

        while self.snext_is("+") || self.snext_is("-") {
            let op = self.next().to_string();
            self.check_next("Expected a value");
            let right = self.parse_mult();

            node = AstNode::BinaryExpr {
                left: Rc::new(node),
                right: Rc::new(right),
                op
            };
        }

        return node;
    }

    fn parse_mult(&mut self) -> AstNode {
        let mut node = self.parse_exp();

        while self.snext_is("*") || self.snext_is("/") || self.snext_is("(") || self.next_is(Ident("".into())) {
            if self.snext_is("(") || self.next_is(Ident("".into())) {
                self.next();
                let right = self.parse_exp();

                node = AstNode::BinaryExpr {
                    left: Rc::new(node),
                    right: Rc::new(right), 
                    op: "*".into()
                };
                continue;
            }
            let op = self.next().to_string();
            self.check_next("Expected a value");
            let right = self.parse_exp();

            node = AstNode::BinaryExpr {
                left: Rc::new(node),
                right: Rc::new(right),
                op
            };
        }

        return node;
    }

    fn parse_exp(&mut self) -> AstNode {
        let mut node = self.parse_unary();

        while self.snext_is("^") {
            self.next();
            self.check_next("Expected a value");
            let pow = self.parse_unary();

            node = AstNode::BinaryExpr {
                left: Rc::new(node),
                right: Rc::new(pow),
                op: String::from("^")
            };
        }

        return node;
    }

    fn parse_unary(&mut self) -> AstNode {
        let at = self.at().to_string();
        if (at != "-" && at != "+") || !self.has_next() { return self.parse_float(); }
        self.next();
        return AstNode::UnaryExpr { sign: at, value: Rc::new(self.parse_float()) };
    }

    fn parse_float(&mut self) -> AstNode {
        let mut value = if self.at() == &Dot {
            self.expect(Int("".into()), "Expected number after decimal point");
            let at_str = self.at().to_string();
            AstNode::Literal {
                kind: TypeKind::Float,
                data: (at_str.parse::<f64>().unwrap() 
                    / 10usize.pow(at_str.len() as u32) as f64)
                    .into()
            }
        } else {
            self.parse_call()
        };
        
        if let AstNode::Literal { kind, data } = &mut value {
            if *kind == TypeKind::Float { return value; }

            if !self.next_is(Dot) { return value; }
            self.next();
            *kind = TypeKind::Float;
            unsafe { data.f = data.i as f64; }

            if !self.next_is(Int("".into())) {
                return value;
            }
            let nstr = self.next().to_string();
            data.f = unsafe {data.f} + nstr.parse::<f64>().unwrap() 
                / 10usize.pow(nstr.len() as u32) as f64;
        }
        return value;
    }

    fn parse_call(&mut self) -> AstNode {
        let id = self.parse_primary();
        if !self.snext_is("(") { return id; }
        if let Ident(ident) = self.at() {
            if !self.fns.contains(&ident) { return id; }
        } else { return id; }
        self.next();
        let args = self.parse_paren();
        let id = if let AstNode::Identifier {id} = id { id } else { "".into() };

        return AstNode::FnCall {
            id,
            args: args.into()
        };
    }

    fn parse_primary(&mut self) -> AstNode {
        match self.at().clone() {
            Int(x) => AstNode::Literal { kind: TypeKind::Integer, data: x.parse::<i64>().unwrap().into() },
            Ident(name) => AstNode::Identifier { id: name },
            Paren(true) => self.parse_paren(),
            Brace(true) => self.parse_block(),

            End => { 
                if unsafe { DEBUG } { println!("{:#?}", self.nodes); }
                err!(fatal "Unexpected expression ending"); 
            },
            _ => {
                err!(fatal format!("Unidentified token: {:?}", self.at()));
            }
        }
    }

    fn parse_paren(&mut self) -> AstNode {
        if !self.has_next() { err!(fatal "Expected expression OR ) after ( but found nothing"); }
        let at = self.next();
        if *at == Paren(false) { return AstNode::Void; }
        let expr = self.parse_csv();
        self.expect(Paren(false), "Expected a ) after expression");
        return expr;
    }

    fn parse_block(&mut self) -> AstNode {
        if !self.has_next() { err!(fatal "Expected expression(s) or }, after {, but found nothing"); }
        let mut body = Vec::new();
        while self.has_next() && !self.snext_is("}") {
            self.next();
            let node = self.parse_root();
            println!("{:#?}\n{:?}", node, self.at());
            body.push(node);
        }
        self.check_next("Unclosed block, expected }");

        return AstNode::BlockExpr { 
            body,
            is_fn: false
        };
    }
}







impl Ast {
    pub fn parse(tokens: Vec<Token>, fns: Vec<String>) -> Ast {
        let mut ast = Ast {
            nodes: vec![],
            fns,
            tokens,
            at: 0
        };

        let mut node = AstNode::Void;
        loop {
            node = ast.parse_root();
            ast.nodes.push(node.clone());

            if !ast.has_next() { break; }
            ast.next();
        }

        return ast;
    }

    fn has_next(&mut self) -> bool { 
        let mut has_next = self.at < self.tokens.len() - 1;
        //if has_next && self.tokens[self.tokens.len() - 1] == End {
        //    self.at += 1;
        //    has_next = false;
        //}

        return has_next;
    }
    fn has_last(&self) -> bool { self.at > 0 }
    fn next(&mut self) -> &Token {
        self.at += 1;
        return self.at();
    }
    fn last(&mut self) -> &Token {
        self.at -= 1;
        return self.at();
    }
    fn at(&self) -> &Token {
        if let Some(t) = self.tokens.get(self.at) {
            return t;
        }
        err!(fatal "Can't get token");
    }
    fn get_next(&self) -> &Token {
        if let Some(t) = self.tokens.get(self.at + 1) {
            return t;
        }
        err!(fatal "Can't get next token");
    }
    fn get_last(&mut self) -> &Token {
        if let Some(t) = self.tokens.get(self.at - 1) {
            return t;
        }
        err!(fatal "Can't get last token");
    }

    fn is_end(&mut self) -> bool {
        self.next_is(End)
    }
    fn check_next(&mut self, msg: &str) {
        if !self.has_next() { err!(fatal msg); }
        self.at += 1;
    }
    fn expect(&mut self, tkn: Token, msg: &str) {
        self.check_next(msg);
        if self.at() != &tkn { err!(msg); }
    }
    fn next_is(&mut self, tkn: Token) -> bool {
        self.has_next() && *self.get_next() == tkn
    }
    fn snext_is(&mut self, str: &str) -> bool {
        self.has_next() && self.get_next().to_string() == str
    }
}
