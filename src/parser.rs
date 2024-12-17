

use std::rc::Rc;

use crate::lexer;
use crate::err;

use lexer::Token;


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
    Integer,
    Float,
}

#[derive(Debug, Clone)]
pub enum AstNode {
    Identifier { id: String },
    Literal { kind: TypeKind, data: String },

    VarAssign { id_expr: Rc<AstNode>, assign: Rc<AstNode> },
    BinaryExpr { left: Rc<AstNode>, right: Rc<AstNode>, op: String },
    UnaryExpr { sign: String, value: Rc<AstNode> },

    Void
}

pub struct Ast {
    pub nodes   :   Vec<AstNode>,
    pub tokens  :   Vec<Token>,
    pub at      :   usize,
}

use Token::*;
impl Ast {
    fn parse_root(&mut self) -> AstNode {
        self.parse_assign()
    }

    fn parse_assign(&mut self) -> AstNode {
        let left = self.parse_add();
        if !self.has_next() || !self.next_is(Equals) { return left; }
        self.next();
        self.check_next("Expected expression after =, but found nothing");
        let assign = Rc::new(self.parse_add());
        return AstNode::VarAssign {
            id_expr: Rc::new(left),
            assign
        };
    }

    fn parse_add(&mut self) -> AstNode {
        let left = self.parse_mult();
        let mut node = AstNode::Void;
        let mut extra_op = false;

        if !self.has_next() { return left; } else { self.next(); }
        let mut at = self.at().to_string();
        while at == "+" || at == "-" {
            let op = at;
            self.check_next("Expected a value");
            let right = self.parse_mult();

            if extra_op {
                node = AstNode::BinaryExpr {
                    left: Rc::new(node),
                    right: Rc::new(right),
                    op
                };
            } else {
                node = AstNode::BinaryExpr {
                    left: Rc::new(left.clone()),
                    right: Rc::new(right),
                    op
                };
            }

            extra_op = true;
            if self.has_next() {
                let next = self.get_next().to_string();
                if next == "+" || next == "-" { self.next(); }
            }
            at = self.at().to_string();
        }

        if let AstNode::Void = node { 
            self.last();
            return left;
        }

        return node;
    }

    fn parse_mult(&mut self) -> AstNode {
        let left = self.parse_impl_mult();
        let mut node = AstNode::Void;
        let mut extra_op = false;

        if !self.has_next() { return left; } else { self.next(); }
        let mut at = self.at().to_string();
        while at == "*" || at == "/" {
            let op = at;
            self.check_next("Expected a value");
            let right = self.parse_impl_mult();

            if extra_op {
                node = AstNode::BinaryExpr {
                    left: Rc::new(node),
                    right: Rc::new(right),
                    op
                };
            } else {
                node = AstNode::BinaryExpr {
                    left: Rc::new(left.clone()),
                    right: Rc::new(right),
                    op
                };
            }

            extra_op = true;
            if self.has_next() {
                let next = self.get_next().to_string();
                if next == "*" || next == "/" { self.next(); }
            }
            at = self.at().to_string();
        }

        if let AstNode::Void = node { 
            self.last();
            return left;
        }

        return node;
    }

    fn parse_impl_mult(&mut self) -> AstNode {
        let left = self.parse_unary();
        if !self.has_next() || (!self.next_is(Paren(true)) && !self
            .next_is(Ident("".into()))) { return left; }
        self.next();
        let right = self.parse_unary();
        return AstNode::BinaryExpr {
            left: Rc::new(left),
            right: Rc::new(right), 
            op: "*".into()
        };
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
            AstNode::Literal { kind: TypeKind::Float, data: format!("0.{}", self.at().to_string()) }
        } else {
            self.parse_primary()
        };
        
        if let AstNode::Literal { kind, data } = &mut value {
            if *kind == TypeKind::Float { return value; }

            if !self.next_is(Dot) { return value; }
            self.next();
            data.push('.');
            *kind = TypeKind::Float;

            if !self.next_is(Int("".into())) {
                data.push('0');
                return value;
            }
            data.push_str(&self.next().to_string());
        }
        return value;
    }

    fn parse_primary(&mut self) -> AstNode {
        match self.at().clone() {
            Int(x) => AstNode::Literal { kind: TypeKind::Integer, data: x },
            Ident(name) => AstNode::Identifier { id: name },
            Paren(true) => self.parse_paren(),

            _ => {
                err!(fatal format!("Unidentified token: {:?}", self.at()));
            }
        }
    }

    fn parse_paren(&mut self) -> AstNode {
        if !self.has_next() { err!(fatal "Expected expression OR ) after ( but found nothing"); }
        let at = self.next();
        if *at == Paren(false) { return AstNode::Void; }
        let expr = self.parse_add();
        self.expect(Paren(false), "Expected a ) after expression");
        return expr;
    }
}







impl Ast {
    pub fn parse(tokens: Vec<Token>) -> Ast {
        let mut ast = Ast {
            nodes: vec![],
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

    fn has_next(&self) -> bool { self.at < self.tokens.len() - 1 }
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
    fn get_next(&mut self) -> &Token {
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
}
