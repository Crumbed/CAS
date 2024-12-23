use std::str::Chars;




const ID_BREAK: [char; 23] = [
    ' ',
    ',',
    '.',
    '>',
    '<',
    '!',
    '|',
    '&',
    ';', 
    '(',
    ')',
    '[',
    ']',
    '{',
    '}',
    '=',
    '+',
    '-',
    '*',
    '/',
    '\n', 
    '^',
    ':'
];

pub type Open = bool;

#[derive(Debug, Clone)]
pub enum Token {
    Ident(String),
    Int(String),
    
    BinaryOp(String),
    Equals,
    Bang,
    Comma,
    Dot,
    Colon,

    Paren(Open),
    Bracket(Open),
    Brace(Open),

    End,
    Unassigned(String)
}

impl Default for Token {
    fn default() -> Self {
        Token::Unassigned("".into())
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        use Token::*;

        match (self, other) {
            (Ident(_), Ident(_)) => true,
            (Int(_), Int(_)) => true,
            (BinaryOp(_), BinaryOp(_)) => true,

            (Equals, Equals) => true,
            (Bang, Bang) => true,
            (Comma, Comma) => true,
            (Dot, Dot) => true,
            (Colon, Colon) => true,
            (End, End) => true,

            (Paren(a), Paren(b)) 
                | (Bracket(a), Bracket(b)) 
                | (Brace(a), Brace(b)) if a == b => true,
            (Unassigned(a), Unassigned(b)) if a == b => true,
            _ => false
        }
    }
}

impl ToString for Token {
    fn to_string(&self) -> String {
        use Token::*;

        match self {
            Ident(x) | Int(x) | BinaryOp(x) | Unassigned(x) => x.to_string(),

            Equals => "=".to_string(),
            Bang => "!".to_string(),
            Comma => ",".to_string(),
            Dot => ".".to_string(),
            Colon => ":".to_string(),
            End => ";".to_string(),

            Paren(open) => if *open { "(" } else { ")" }.to_string(),
            Bracket(open) => if *open { "[" } else { "]" }.to_string(),
            Brace(open) => if *open { "{" } else { "}" }.to_string(),
        }
    }
}


pub fn tokenize(src: &str) -> Vec<Token> {
    use Token::*;
    let mut tokens = vec![];
    let mut chars = src.chars();

    let mut c = ' ';
    let mut next = true;
    loop {
        if next { c = if let Some(c) = chars.next() {c} else {break;}; }

        if c.is_ascii() && c.is_alphabetic() {
            let tkn = lex_ident(&mut chars, &mut c);
            tokens.push(tkn);
            next = false;
            continue;
        }
        if is_num(&c) {
            let tkn = lex_number(&mut chars, &mut c);
            tokens.push(tkn);
            next = false; 
            continue;
        }

        next = true;
        let tkn = match c {
            ' ' | '\t' => continue,
            '+' | '-' | '*' | '/' | '^' => BinaryOp(c.to_string()),
            ';' | '\n' => End,

            '=' => Equals,
            '!' => Bang,
            ',' => Comma,
            '.' => Dot,
            ':' => Colon,

            '(' => Paren(true),
            ')' => Paren(false),
            '[' => Bracket(true),
            ']' => Bracket(false),
            '{' => Brace(true),
            '}' => Brace(false),

            _ => Unassigned(c.to_string())
        };

        if *tokens.last().unwrap() == End && tkn == End { continue; }
        tokens.push(tkn);
    }

    return tokens;
}

fn lex_ident(chars: &mut Chars, c: &mut char) -> Token {
    let mut id = String::new();
    while !ID_BREAK.contains(c) {
        id.push(*c);
        *c = if let Some(c) = chars.next() {c} else {break;};
    }

    return Token::Ident(id);
}

fn lex_number(chars: &mut Chars, c: &mut char) -> Token {
    let mut num = String::new();
    while is_num(c) {
        num.push(*c);
        *c = if let Some(c) = chars.next() {c} else {break;};
    }

    return Token::Int(num);
}


fn is_num(c: &char) -> bool {
    ('0'..='9').contains(c)
}











