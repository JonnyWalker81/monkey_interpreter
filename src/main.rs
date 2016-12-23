extern crate monkey_lib;
use std::io;

fn main() {
    println!("Hello, world!");
    monkey_lib::interpreter::hello();

    println!("Hello ...! This is the Monkey Programming Language!");
    println!("Feel free to type in commands");

    let mut stdin = io::stdin();
    let mut stdout = io::stdout();
    let r = monkey_lib::interpreter::repl::Repl::start(&mut stdin, &mut stdout);
}
