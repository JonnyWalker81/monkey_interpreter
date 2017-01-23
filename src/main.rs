extern crate monkey_lib;
#[macro_use]
extern crate clap;

use std::io;
use std::env;

fn main() {

    let matches = clap_app!(myapp =>
                            (version: "1.0")
                            (author: "Jonathan Rothberg")
                            (about: "Monkey Interpreter")
                            (@arg INPUT: "Monkey File to be interpreted")
    ).get_matches();


    let mut stdin = io::stdin();
    let mut stdout = io::stdout();

    match matches.value_of("INPUT") {
        Some(i) => {
            let _ = monkey_lib::interpreter::interpret_file(i.into(), &mut stdout);
        },
        None => {
            let user = match env::var("USER") {
                Ok(u) => u,
                Err(_) => String::new()
            };
            println!("Hello {}! This is the Monkey Programming Language!", user);
            println!("Feel free to type in commands");
            let _ = monkey_lib::interpreter::repl::Repl::start(&mut stdin, &mut stdout);
        }
    }
}
