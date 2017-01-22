pub mod token;
pub mod lexer;
pub mod keywords;
pub mod repl;
pub mod ast;
pub mod program;
pub mod parser;
pub mod object;
pub mod evaluator;
pub mod environment;
pub mod builtins;

use std::fs::File;
use std::io::{ Read, Write };
use std::io::{Stdout, Stdin};
use std::sync::Arc;
use std::cell::RefCell;

use interpreter::lexer::Lexer;
use interpreter::parser::Parser;
use interpreter::program::Program;
use interpreter::evaluator::Evaluator;
use interpreter::environment::Environment;


pub fn hello() {
    println!("Hello from interpreter lib...")
}

fn print_parse_errors(stdout: &mut Stdout, errors: &Vec<String>) {
    // write!(stdout, "{}", MONKEY_FACE);
    write!(stdout, "Whoops! We ran into some monkey business here!\n");
    write!(stdout, " parse errors: \n");
    for e in errors {
        write!(stdout, "\t{}\n}}", e);
    }
}

pub fn interpret_file(file: String, stdout: &mut Stdout) {
    let mut file = File::open(file).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();

    let mut env = Arc::new(RefCell::new(Environment::new()));
    let lexer = Lexer::new(contents);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    match program {
        Some(p) => {
            if parser.get_errors().len() != 0 {
                print_parse_errors(stdout, &parser.get_errors());
            }
            else {
                let _ = Evaluator::eval_program(&p, &mut env);
                // write!(stdout, "{}", evaluated);
                // write!(stdout, "\n");
            }
        },
        None => {
            println!("Error parsing program");
        }
    }
}

