

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::err;
use crate::parser;
use parser::Value;
use parser::TypeKind;
use parser::AstNode;

pub struct Variable {
    pub id: String,
    pub ty: TypeKind,
    pub ad: Address
}

pub struct Frame {
    pub start: Address,
    vars: HashMap<String, Variable>,
}

impl Frame {
    pub fn new(start: Address) -> Self {
        Frame {
            start,
            vars: HashMap::new()
        }
    }
}

pub struct Env {
    mem: Vec<Value>,
    frames: Vec<Frame>,
    frame: Frame
}

pub type Address = usize;


impl Env {
    pub fn new() -> Self {
        let frame = Frame::new(0);
        Env {
            mem: Vec::with_capacity(100),
            frames: vec![],
            frame
        }
    }

    pub fn get(&self, var: &Variable) -> &Value {
        let val = self.mem.get(var.ad);
        return match val {
            Some(x) => x,
            None => { err!(fatal "Invalid variable"); }
        };
    }
    pub fn alloc(&mut self, data: Value) -> Address {
        self.mem.push(data);
        return self.mem.len() - 1;
    }
    pub fn free_to(&mut self, adr: Address) {
        while self.mem.len() - 1 != adr {
            self.mem.pop();
        }
    }
}

use AstNode::*;
impl Env {
    pub fn eval_ast(&mut self, ast: Vec<AstNode>) {
        for node in ast {
            let (t, v) = self.eval_node(&node);
            match t {
                TypeKind::Integer => println!("Result: {}", unsafe {v.i}),
                TypeKind::Float => println!("Result: {}", unsafe {v.f}),
            }
        }
    }

    fn eval_node(&mut self, node: &AstNode) -> (TypeKind, Value) {
        match node {
            Identifier { id } => self.eval_ident(id),
            Literal { kind, data } => (*kind, *data),
            VarAssign { id_expr, assign } => self.eval_assignment(id_expr, assign),
            BinaryExpr { left, right, op } => self.eval_binary(left, right, op),
            UnaryExpr { sign, value } => self.eval_unary(sign, value),
            Void => todo!(),
        }
    }

    fn eval_assignment(&mut self, left: &AstNode, right: &AstNode) -> (TypeKind, Value) {
        let id = if let Identifier { id } = left { id } else {
            // else if not an ident
            todo!();
        };

        let (t, v) = self.eval_node(right);
        let ad = self.alloc(v);
        let var = Variable {
            id: id.to_owned(),
            ty: t,
            ad
        };

        self.frame.vars.insert(id.to_owned(), var);
        return (t, v);
    }

    fn eval_binary(&mut self, left: &AstNode, right: &AstNode, op: &str) -> (TypeKind, Value) {
        let (lt, mut l) = self.eval_node(left);
        let (rt, mut r) = self.eval_node(right);
        
        // if types are different, one is a float so the expression 
        // should be evaluated as a float
        let t = if lt == rt { lt } else { 
            match lt {
                TypeKind::Integer => l.f = unsafe { l.i as f64 },
                TypeKind::Float => r.f = unsafe { r.i as f64 },
            }
            TypeKind::Float 
        };

        let v = unsafe { 
            match op {
                "+" => l.add(r, &t),
                "-" => l.sub(r, &t),
                "*" => l.mul(r, &t),
                "/" => l.div(r, &t),
                _ => { err!(fatal format!("Invalid binary expression operator: {}", op)); }
            }
        };

        return (t, v);
    }

    fn eval_unary(&mut self, sign: &str, expr: &AstNode) -> (TypeKind, Value) {
        let (t, v) = self.eval_node(expr);
        return (t, match sign {
            "+" => match t {
                TypeKind::Integer => unsafe { v.i.abs().into() },
                TypeKind::Float => unsafe { v.f.abs().into() }
            }, 
            "-" => match t {
                TypeKind::Integer => unsafe { (-v.i).into() },
                TypeKind::Float => unsafe { (-v.f).into() }
            },
            _ => v
        });
    }

    fn eval_ident(&mut self, ident: &str) -> (TypeKind, Value) {
        let var = self.frame.vars.get(ident);
        return match var {
            Some(x) => (x.ty, *self.get(x)),
            None => { err!(fatal format!("Variable {} is not declared", ident)); }
        }
    }
}




















