use std::io::{Stdin, Stdout};
use std::io::{ Read, Write };
use std::sync::Arc;
use std::cell::RefCell;
use interpreter::lexer::Lexer;
use interpreter::token::Token;
use interpreter::parser::Parser;
use interpreter::evaluator::Evaluator;
use interpreter::environment::Environment;
use interpreter::object::ObjectType;
use std::env;
use readline;

const PROMPT: &'static str = ">> ";
const MONKEY_FACE: &'static str = r#"
        __,__
  .--. .-" "-. .--.
/ .. \/ .-. .-. \/ .. \
| | '| / Y \ |' | |
| \ \ \ 0 | 0 / / / |
\ '- ,\.-"""""""-./, -' /
 ''-' /_ ^ ^ _\ '-''
    | \._ _./ |
    \ \ '~' / /
    '._ '-=-' _.'
       '-----'
"#;

pub struct Repl {
}

impl Repl {
    pub fn start(stdin: &mut Stdin, stdout: &mut Stdout) {
        let mut buffer = String::new();
        let mut env = Arc::new(RefCell::new(Environment::new()));

        let monkey_path = match env::var("MONKEY_PATH") {
            Ok(p) => {
                println!("{}", p);
                p
            },
            Err(_) => {
                println!("MONKEY_PATH not found...");
                String::new()
            }
        };


        loop {
            // print!("{}", PROMPT);
            stdout.flush().unwrap();
            buffer.clear();
            // stdin.read_line(&mut buffer);
            let input = readline::readline(PROMPT);
             match input {
                 Ok(string) => {
                     readline::add_history(string.as_str());
                     buffer = string;
                 },
                Err(e) => break,
            }

            let mut l = Lexer::new(buffer.clone());
            let mut p = Parser::new(l);


            let program = p.parse_program();
            if let Some(pro) = program {
                if p.get_errors().len() != 0 {
                    Repl::print_parse_errors(stdout, &p.get_errors());
                    continue;
                }

                let evaluated = Evaluator::eval_program(&pro, &mut env);
                match evaluated {
                    ObjectType::Null => {
                    },
                    _ => {
                        write!(stdout, "{}", evaluated);
                        write!(stdout, "\n");
                    }
                }
            }
        } 
    }

    // TODO: test
    pub fn start_calc(stdin: &mut Stdin, stdout: &mut Stdout) {
        let mut buffer = String::new();
        let mut env = Arc::new(RefCell::new(Environment::new()));
        let mut last_good_result = String::new();

        loop {
            // print!("{}", PROMPT);
            stdout.flush().unwrap();
            buffer.clear();
            // stdin.read_line(&mut buffer);
            let input = readline::readline(PROMPT);
             match input {
                 Ok(string) => {
                     readline::add_history(string.as_str());
                     buffer = string;
                 },
                Err(e) => break,
            }

            let mut l = Lexer::new(buffer.clone());
            let mut p = Parser::new(l);


            let program = p.parse_program();
            if let Some(pro) = program {
                if p.get_errors().len() != 0 {
                        write!(stdout, "{}", last_good_result);
                        write!(stdout, "\n");
                    // Repl::print_parse_errors(stdout, &p.get_errors());
                    continue;
                }

                let evaluated = Evaluator::eval_program(&pro, &mut env);
                match evaluated {
                    ObjectType::Null => {
                        write!(stdout, "{}", last_good_result);
                        write!(stdout, "\n");
                    },
                    ObjectType::Error(..) => {
                        write!(stdout, "{}", last_good_result);
                        write!(stdout, "\n");
                    },
                    _ => {
                        last_good_result.clear();
                        last_good_result = format!("{}", evaluated);
                        write!(stdout, "{}", evaluated);
                        write!(stdout, "\n");
                    }
                }
            }
        }
    }

    fn print_parse_errors(stdout: &mut Stdout, errors: &Vec<String>) {
        write!(stdout, "{}", MONKEY_FACE);
        write!(stdout, "Whoops! We ran into some monkey business here!\n");
        write!(stdout, " parse errors: \n");
        for e in errors {
            write!(stdout, "\t{}\n}}", e);
        }
    }
}
