

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::err;
use crate::err::RtResult;
use crate::err::RuntimeError;
use crate::err::RuntimeError::*;
use crate::parser;
use crate::ENV;
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
pub struct Struct {
    pub id: String,
    pub offsets: HashMap<String, usize>,
    pub types: Vec<TypeKind>
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
    structs: HashMap<String, Struct>,
    frames: Vec<Frame>,
    frame: Frame
}

pub type Address = usize;
#[derive(Debug, Clone)]
pub enum RuntimeValue {
    Word(TypeKind, Value),
    Chunk(TypeKind, Box<[Value]>)
}

impl RuntimeValue {
    pub fn get_type(&self) -> TypeKind {
        match self {
            RuntimeValue::Word(t, _) 
            | RuntimeValue::Chunk(t, _) => t.clone()
        }
    }
}

fn word(t: TypeKind, v: Value) -> RuntimeValue { RuntimeValue::Word(t, v) }
fn chunk(t: TypeKind, data: Box<[Value]>) -> RuntimeValue { RuntimeValue::Chunk(t, data) }
fn zero_value() -> RuntimeValue { word(TypeKind::Integer, 0.into()) }

impl Env {
    pub fn new() -> Self {
        let frame = Frame::new(0);
        Env {
            mem: Vec::with_capacity(100),
            funcs: HashMap::new(),
            structs: HashMap::new(),
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

    pub fn getc(&self, var: &Variable, size: usize) -> RtResult<Box<[Value]>> {
        if var.ad + size > self.mem.len() { 
            return Err(VarUndefined(var.id.clone()));
        }
        let val = &self.mem[var.ad..var.ad + size];
        return Ok(val.into());
    }

    pub fn set(&mut self, adr: Address, data: &[Value]) {
        for (i, d) in data.iter().enumerate() {
            self.mem[adr + i] = *d;
        }
    }

    pub fn allocc(&mut self, data: &[Value]) -> Address {
        let ad = self.mem.len();
        for d in data {
            self.mem.push(*d);
        }
        return ad;
    }
    pub fn alloc(&mut self, data: Value) -> Address {
        let ad = self.mem.len();
        self.mem.push(data);
        return ad;
    }
    pub fn free_to(&mut self, adr: Address) {
        while self.mem.len() != adr {
            self.mem.pop();
        }
    }
}

use AstNode::*;
impl Env {
    pub fn eval_ast(&mut self, ast: Vec<AstNode>) -> RtResult<Vec<RuntimeValue>> {
        let mut results = Vec::with_capacity(ast.len());
        for node in ast {
            let value = self.eval_node(&node);
            //println!("{:#?}", value_err);
            if unsafe { ENV } { println!("{:#?}", self); }
            results.push(value?);
        }

        return Ok(results);
    }

    fn eval_node(&mut self, node: &AstNode) -> RtResult<RuntimeValue> {
        match node {
            Identifier { id } => self.eval_ident(id),
            Literal { kind, data } => Ok(word(kind.clone(), *data)),
            ListLit { size, data } => self.eval_list(size, data),
            VarAssign { id_expr, assign } => self.eval_assignment(id_expr, assign),
            BinaryExpr { left, right, op } => self.eval_binary(left, right, op),
            UnaryExpr { sign, value } => self.eval_unary(sign, value),
            FnCall { id, args } => self.eval_fn_call(id, args),
            BlockExpr { body, is_fn } => self.eval_block(body, *is_fn),
            Void | FnIdent {..} | Csv {..} => todo!(),
        }
    }

    fn eval_list(&mut self, size: &AstNode, data: &Vec<AstNode>) -> RtResult<RuntimeValue> {
        let len = self.eval_node(size)?;
        let len = if let RuntimeValue::Word(t, mut v) = len { unsafe {
            if t == TypeKind::Float { v.as_int(); }
            v.i as usize
        }} else { 
            return Err(RuntimeError::InvalidVal(format!("expected int but found {}", len.get_type())));
        };

        let mut list = Vec::with_capacity(len);
        let mut has_type = false;
        let mut ty = TypeKind::Integer; // default type
        for node in data {
            let d = self.eval_node(node)?;
            let typ;
            match d {
                RuntimeValue::Word(t, v) => {
                    typ = t;
                    list.push(v);
                },
                RuntimeValue::Chunk(t, c) => {
                    typ = t;
                    list.append(&mut c.into());
                },
            }

            if !has_type {
                ty = typ;
                has_type = true;
            } else if ty != typ {
                return Err(RuntimeError::InvalidType(format!("expected {}, but found {}", ty, typ)));
            }
        }

        return Ok(chunk(TypeKind::List(len, ty.into()), list.into()));
    }

    fn eval_fn_call(&mut self, id: &str, args: &AstNode) -> RtResult<RuntimeValue> {
        let args = match args {
            Csv { values } => {
                let mut err = Ok(zero_value());
                let values = values.iter().map(|v| match self.eval_node(v) {
                    Ok(x) => x,
                    Err(e) => { 
                        err = Err(e);
                        zero_value()
                    }
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
            let ad = match arg {
                RuntimeValue::Word(_, v) => self.alloc(*v),
                RuntimeValue::Chunk(_, c) => self.allocc(c)
            };
            let var = Variable {
                id: param.to_string(),
                ty: arg.get_type(),
                ad
            };
            self.frame.declare(param, var);
        }
        let body = func.body;
        let value = self.eval_node(&body);
        self.pop_frame();

        return value;
    }

    fn eval_block(&mut self, body: &Vec<AstNode>, is_fn: bool) -> RtResult<RuntimeValue> {
        // for now, never create new frame
        if !is_fn { return Err(FnUndefined("".into(), 0)); }
        let mut value = zero_value();
        for node in body {
            value = self.eval_node(node)?;
        }

        return Ok(value);
    }

    fn eval_assignment(&mut self, left: &AstNode, right: &AstNode) -> RtResult<RuntimeValue> {
        if let FnIdent { id, params } = left {
            return self.eval_fn_dec(id, &params, right);
        }

        let id = if let Identifier { id } = left { id } else {
            // else if not an ident
            todo!();
        };

        let data = self.eval_node(right)?;
        //println!("{:#?}", data);
        match self.frame.get(id) {
            Some(var) => match &data {
                RuntimeValue::Word(_, v) => self.set(var.ad, &[*v]),
                RuntimeValue::Chunk(_, c) => self.set(var.ad, c),
            },
            None => {
                let ty = data.get_type();
                let ad = match &data {
                    RuntimeValue::Word(_, v) => self.alloc(*v),
                    RuntimeValue::Chunk(_, c) => self.allocc(c),
                };
                let var = Variable {
                    id: id.to_owned(),
                    ty,
                    ad
                };
                //println!("{:#?}", var);
                self.frame.declare(id, var);
            }
        }

        return Ok(data);
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
                                TypeKind::List(..) 
                                | TypeKind::Struct(_) => format!("{}", kind)
                            }
                        },
                        _ => {
                            valid = false;
                            "[Expression]".into()
                        }
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

        return Ok(zero_value());
    }

    /*
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
    */

    fn eval_binary(&mut self, left: &AstNode, right: &AstNode, op: &str) -> RtResult<RuntimeValue> {
        let l = self.eval_node(left)?;
        let r = self.eval_node(right)?;
        let (lt, mut l) = if let RuntimeValue::Word(t, v) = l { (t, v) } else {
            return Err(RuntimeError::CannotOp(format!("Cannot perform {} on {}", op, l.get_type())));
        };
        let (rt, mut r) = if let RuntimeValue::Word(t, v) = r { (t, v) } else {
            return Err(RuntimeError::CannotOp(format!("Cannot perform {} on {}", op, r.get_type())));
        };
        
        // if types are different, one is a float so the expression 
        // should be evaluated as a float
        let t = unsafe {
            if lt == rt { 
                match op {
                    "^" if r.is_negative(&rt) && rt == TypeKind::Integer => {
                        l.as_float();
                        r.as_float();
                        TypeKind::Float
                    },
                    "/" if lt == TypeKind::Integer => {
                        l.as_float();
                        r.as_float();
                        TypeKind::Float
                    },
                    _ => lt
                }
            } else { 
                match lt {
                    TypeKind::Integer => l.as_float(),
                    TypeKind::Float => r.as_float(),
                    _ => (),
                }
                TypeKind::Float 
            }
        };

        let v = unsafe { 
            match op {
                "+" => l.add(r, &t),
                "-" => l.sub(r, &t),
                "*" => l.mul(r, &t),
                "/" => l.div(r, &t),
                "^" => l.pow(r, &t),
                _ => { err!(fatal format!("Invalid binary expression operator: {}", op)); }
            }
        };

        return Ok(word(t, v));
    }

    fn eval_unary(&mut self, sign: &str, expr: &AstNode) -> RtResult<RuntimeValue> {
        let data = self.eval_node(expr)?;
        let (t, v) = if let RuntimeValue::Word(t, v) = data { (t, v) } else {
            return Err(CannotOp(format!("Cannot perform unary {} on {}", sign, data.get_type()))); 
        };

        return Ok(word(t.clone(), match sign {
            "+" => match t {
                TypeKind::Integer => unsafe { v.i.abs().into() },
                TypeKind::Float => unsafe { v.f.abs().into() },
                _ => v // cant use unary on list or struct
            }, 
            "-" => match t {
                TypeKind::Integer => unsafe { (-v.i).into() },
                TypeKind::Float => unsafe { (-v.f).into() },
                _ => v // cant use unary on list or struct
            },
            _ => v
        }));
    }

    fn eval_ident(&mut self, ident: &str) -> RtResult<RuntimeValue> {
        let var = self.frame.vars.get(ident);
        return match var {
            Some(x) => {
                Ok(match &x.ty {
                    TypeKind::Integer | TypeKind::Float => word(x.ty.clone(), *self.get(x)?),
                    TypeKind::List(size, t) => chunk(*t.clone(), self.getc(x, *size)?),
                    _ => zero_value(),
                })
            },
            None => Err(VarUndefined(ident.to_string()))
        }
    }
}






















