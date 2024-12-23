

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::err;
use crate::err::RtResult;
use crate::err::RuntimeError::*;
use crate::parser;
use crate::DEBUG;
use parser::Value;
use parser::TypeKind;
use parser::AstNode;

#[derive(Debug, Clone)]
pub struct Variable {
    pub id: String,
    pub ty: TypeKind,
    pub ad: Address
}

#[derive(Debug, Clone)]
pub struct Function {
    pub id: String,
    pub params: Vec<String>,
    pub body: AstNode
}

#[derive(Debug, Clone)]
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

    pub fn declare(&mut self, id: &str, var: Variable) {
        self.vars.insert(id.to_owned(), var);
    }

    pub fn has_var(&self, id: &str) -> bool {
        return self.vars.contains_key(id);
    }

    pub fn get(&self, id: &str) -> Option<&Variable> {
        return self.vars.get(id);
    }

    pub fn get_mut(&mut self, id: &str) -> Option<&mut Variable> {
        return self.vars.get_mut(id);
    }
}

#[derive(Debug, Clone)]
pub struct Env {
    mem: Vec<Value>,
    funcs: HashMap<(String, usize), Function>,
    frames: Vec<Frame>,
    frame: Frame
}

pub type Address = usize;
pub type RuntimeValue = (TypeKind, Value);

impl Env {
    pub fn new() -> Self {
        let frame = Frame::new(0);
        Env {
            mem: Vec::with_capacity(100),
            funcs: HashMap::new(),
            frames: vec![],
            frame
        }
    }

    pub fn new_frame(&mut self) {
        let frame = Frame::new(self.mem.len());
        self.frames.push(frame);
        let current_frame = self.frames.len() - 1;
        std::mem::swap(&mut self.frames[current_frame], &mut self.frame);
    }
    pub fn pop_frame(&mut self) {
        let start = self.frame.start;
        self.frame = self.frames.pop().unwrap();
        self.free_to(start);
    }

    pub fn get_fn(&self, id: &str, p_count: usize) -> Option<Function> {
        return self.funcs.get(&(id.to_string(), p_count)).cloned();
    }

    pub fn get(&self, var: &Variable) -> RtResult<&Value> {
        let val = self.mem.get(var.ad);
        return match val {
            Some(x) => Ok(x),
            None => Err(VarUndefined(var.id.clone()))
        };
    }

    pub fn set(&mut self, adr: Address, data: Value) {
        self.mem[adr] = data;
    }

    pub fn alloc(&mut self, data: Value) -> Address {
        self.mem.push(data);
        return self.mem.len() - 1;
    }
    pub fn free_to(&mut self, adr: Address) {
        while self.mem.len() != adr {
            self.mem.pop();
        }
    }
}

use AstNode::*;
impl Env {
    pub fn eval_ast(&mut self, ast: Vec<AstNode>) {
        for node in ast {
            let value = self.eval_node(&node);
            if unsafe { DEBUG } { println!("{:#?}", self); }
            let (t, v) = value.unwrap();
            match t {
                TypeKind::Integer => println!("Result: {}", unsafe {v.i}),
                TypeKind::Float => println!("Result: {}", unsafe {v.f}),
            }
        }
    }

    fn eval_node(&mut self, node: &AstNode) -> RtResult<RuntimeValue> {
        match node {
            Identifier { id } => self.eval_ident(id),
            Literal { kind, data } => ok(*kind, *data),
            VarAssign { id_expr, assign } => self.eval_assignment(id_expr, assign),
            BinaryExpr { left, right, op } => self.eval_binary(left, right, op),
            UnaryExpr { sign, value } => self.eval_unary(sign, value),
            FnCall { id, args } => self.eval_fn_call(id, args),
            Void | FnIdent {..} | Csv {..} => todo!(),
        }
    }

    fn eval_fn_call(&mut self, id: &str, args: &AstNode) -> RtResult<RuntimeValue> {
        let args = match args {
            Csv { values } => {
                let mut err = ok(TypeKind::Integer, 0.into());
                let values = values.iter().map(|v| match self.eval_node(v) {
                    Ok(x) => x,
                    Err(e) => { err = Err(e); (TypeKind::Integer, 0.into()) }
                }).collect();
                _ = err?;
                values
            },
            _ => vec![self.eval_node(args)?]
        };
        let func = match self.get_fn(id, args.len()) {
            Some(f) => Ok(f),
            None => Err(FnUndefined(id.to_string(), args.len()))
        }?;
        self.new_frame();
        for (arg, param) in args.iter().zip(func.params.iter()) {
            let (t, v) = arg;
            let ad = self.alloc(*v);
            let var = Variable {
                id: param.to_string(),
                ty: *t,
                ad
            };
            self.frame.declare(param, var);
        }
        let body = func.body;
        //println!("{:#?}", self);
        let value = self.eval_node(&body);
        self.pop_frame();

        return value;
    }

    fn eval_assignment(&mut self, left: &AstNode, right: &AstNode) -> RtResult<RuntimeValue> {
        if let FnIdent { id, params } = left {
            return self.eval_fn_dec(id, &params, right);
        }

        let id = if let Identifier { id } = left { id } else {
            // else if not an ident
            todo!();
        };

        let (t, v) = self.eval_node(right)?;
        match self.frame.get(id) {
            Some(var) => {
                self.set(var.ad, v);
            },
            None => {
                let ad = self.alloc(v);
                let var = Variable {
                    id: id.to_owned(),
                    ty: t,
                    ad
                };
                self.frame.declare(id, var);
            }
        }

        return ok(t, v);
    }

    fn eval_fn_dec(&mut self, id: &str, params: &AstNode, body: &AstNode) -> RtResult<RuntimeValue> {
        let params = match params {
            Identifier { id } => vec![id.to_string()],
            Csv { values } => {
                let mut valid = true;
                let values: Vec<String> = values.iter().map(|v| { 
                    match v {
                        Identifier { id } => id.to_string(),
                        Literal { kind, data } => {
                            valid = false;
                            match kind {
                                TypeKind::Float => unsafe { data.f.to_string() }
                                TypeKind::Integer => unsafe { data.i.to_string() }
                            }
                        },
                        _ => "[Expression]".into()
                    }
                }).collect();

                if !valid { return Err(InvalidFnDec(values)); }
                values
            },
            _ => return Err(InvalidFnDec(vec!["Expression".into()]))
        };

        let id = id.to_string();
        let func = Function { id, params, body: body.clone() };
        self.funcs.insert((func.id.clone(), func.params.len()), func);

        return ok(TypeKind::Integer, 0.into());
    }

    fn solve_for<'a>(&mut self, mut left: &'a AstNode, mut right: &'a AstNode, id: &str) -> RtResult<RuntimeValue> {
        let olvars = self.find_undec_vars(left);
        let orvars = self.find_undec_vars(right);
        let (lvars, rvars) = match (olvars, orvars) {
            (None, Some(vars)) => {
                std::mem::swap(&mut left, &mut right);
                (vars, Vec::<String>::with_capacity(0))
            },
            (Some(vars), None) => (vars, Vec::<String>::with_capacity(0)),
            (Some(lvars), Some(rvars)) => (lvars, rvars),
            (None, None) => {
                return Err(SolveFor("Unable to solve for variables because all variables have already been declared".into()));
            }
        };
        // only support linear equations rn

        return ok(TypeKind::Integer, 0.into());
    }

    fn find_undec_vars(&self, node: &AstNode) -> Option<Vec<String>> {
        match node {
            Identifier { id } if !self.frame.has_var(id) => Some(vec![id.to_string()]),
            BinaryExpr { left, right, .. } => {
                let l = self.find_undec_vars(left);
                let r = self.find_undec_vars(right);
                if l.is_none() && r.is_none() { return None; }

                Some(match (l, r) {
                    (Some(mut a), Some(mut b)) => { a.append(&mut b); a },
                    (Some(a), None) => a,
                    (None, Some(b)) => b,
                    _ => todo!()
                })
            },
            _ => None
        }
    }

    fn eval_binary(&mut self, left: &AstNode, right: &AstNode, op: &str) -> RtResult<RuntimeValue> {
        let (lt, mut l) = self.eval_node(left)?;
        let (rt, mut r) = self.eval_node(right)?;
        
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

        return ok(t, v);
    }

    fn eval_unary(&mut self, sign: &str, expr: &AstNode) -> RtResult<RuntimeValue> {
        let (t, v) = self.eval_node(expr)?;
        return ok(t, match sign {
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

    fn eval_ident(&mut self, ident: &str) -> RtResult<RuntimeValue> {
        let var = self.frame.vars.get(ident);
        return match var {
            Some(x) => ok(x.ty, *self.get(x)?),
            None => Err(VarUndefined(ident.to_string()))
        }
    }
}




pub fn ok(t: TypeKind, v: Value) -> RtResult<RuntimeValue> {
    Ok((t, v))
}



















