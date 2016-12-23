use std::io::{Stdin, Stdout};
use std::io::{ Read, Write };
use interpreter::lexer::Lexer;
use interpreter::token::Token;

const PROMPT: &'static str = ">> ";

pub struct Repl {
    
}

impl Repl {
    pub fn start(stdin: &mut Stdin, stdout: &mut Stdout) {
        loop {
            print!("{}", PROMPT);
            stdout.flush().unwrap();
            let mut buffer = String::new();
            stdin.read_line(&mut buffer);

            let mut l = Lexer::new(buffer);

            let mut tok = l.next_token();
            while tok != Token::Eof {
                println!("{}", tok);
                tok = l.next_token();
            }
        } 
    }
}
