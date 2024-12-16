
mod lexer;

use std::io;
use io::Write;

pub const VERSION: &'static str = "v0.0.3";


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

        input.clear();
    }
}
