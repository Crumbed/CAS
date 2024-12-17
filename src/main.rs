
mod lexer;
mod parser;

use std::io;
use io::Write;

pub const VERSION: &'static str = "v0.0.4";

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
    //let args = std::env::args() .collect::<Vec<String>>(); 

    println!("Repl Environment {VERSION}");
    let mut out = io::stdout();
    let stdin = io::stdin();
    'main : loop {
        print!("> ");
        out.flush()?;
        stdin.read_line(&mut input)?;
        if input == ".exit\n" { break 'main Ok(()); }

        println!("eq: {input}");
        let tokens = lexer::tokenize(&input);
        println!("tokens: {:#?}", tokens);
        let ast = parser::Ast::parse(tokens);
        println!("ast: {:#?}", ast.nodes);

        input.clear();
    }
}


pub fn error(msg: &str) {
    println!("Error: {}", msg);
}
