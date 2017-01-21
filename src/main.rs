extern crate monkey_lib;
#[macro_use]
extern crate clap;

use std::io;

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
            println!("Hello ...! This is the Monkey Programming Language!");
            println!("Feel free to type in commands");
            let _ = monkey_lib::interpreter::repl::Repl::start(&mut stdin, &mut stdout);
        }
    }
}
