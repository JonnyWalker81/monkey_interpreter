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

pub fn hello() {
    println!("Hello from interpreter lib...")
}
