mod lexer;
mod parser;
mod interpreter;
mod err;

use std::io;
use interpreter::RuntimeValue;
use io::Write;
use parser::TypeKind;

pub const VERSION: &'static str = "v0.15";
pub static mut TOKENS: bool = false;
pub static mut AST: bool = false;
pub static mut ENV: bool = false;

#[macro_export]
macro_rules! err {
    ($msg: expr) => {
        println!("Error: {}", $msg);
        return;
    };
    (fatal $msg: expr) => {
        panic!("Fatal Error: {}", $msg);
    };
    ($msg: expr, $ret: expr) => {
        println!("Error: {}", $msg);
        return $ret;
    };
    ($msg: expr, $ty: ty) => {
        println!("Error: {}", $msg);
        return $ty::default();
    };
    ($msg: expr, $lbl: ident) => {
        println!("Error: {}", $msg);
        break $lbl;
    };
    ($msg: expr, $lbl: ident, $ret: expr) => {
        println!("Error: {}", $msg);
        break $lbl $expr;
    }
}


fn main() -> io::Result<()> {
    let mut input = String::new();
    let args = std::env::args()
        .collect::<Vec<String>>(); 
    let mut arg_count = 0;
    unsafe {
        for arg in args.iter().skip(1) {
            if arg.starts_with("-") { arg_count += 1; }
            if      arg == "-tokens" || arg == "-t" { TOKENS = true; }
            else if arg == "-ast" || arg == "-a"    { AST = true; }
            else if arg == "-env" || arg == "-e"    { ENV = true; }
        }
    }
    
    // file path
    if args.len() - 1 - arg_count == 1 {
        let path = args.last().unwrap();
        let src = std::fs::read_to_string(path)
            .expect("Unable to read file");

        let tokens = lexer::tokenize(&src);
        if unsafe { TOKENS } { println!("Tokens: {:#?}", tokens); }
        let ast = parser::Ast::parse(tokens, vec![]);
        if unsafe { AST } { println!("Ast: {:#?}", ast.nodes); }
        let mut env = interpreter::Env::new();
        let results = env.eval_ast(ast.nodes);
        unsafe { print_results(&results.unwrap()); }

        return Ok(());
    }


    println!("Repl Environment {VERSION}");
    let mut env = interpreter::Env::new();
    let mut out = io::stdout();
    let stdin = io::stdin();
    let mut fns = vec![];
    'main : loop {
        print!("> ");
        out.flush()?;
        stdin.read_line(&mut input)?;
        if input == ".exit\n" { break 'main Ok(()); }

        let tokens = lexer::tokenize(&input);
        if unsafe { TOKENS } { println!("Tokens: {:#?}", tokens); }
        let ast = parser::Ast::parse(tokens, fns);
        if unsafe { AST } { println!("Ast: {:#?}", ast.nodes); }
        let results = env.eval_ast(ast.nodes);
        unsafe { print_results(&results.unwrap()); }

        fns = ast.fns;
        input.clear();
    }
}


unsafe fn print_results(results: &Vec<RuntimeValue>) {
    for data in results {
        let t = data.get_type();
        match data {
            RuntimeValue::Word(_, v) => println!("Result: {}", v.to_string(&t)),
            RuntimeValue::Chunk(t, c) => {
                let t = if let TypeKind::List(_, ty) = t { ty } else { t };
                print!("Result: {{ ");
                for (i, v) in c.iter().enumerate() {
                    if i > 0 { print!(", "); }
                    print!("{}", v.to_string(t));
                }
                println!(" }}");
            }
        }
    }
}

pub fn error(msg: &str) {
    println!("Error: {}", msg);
}
