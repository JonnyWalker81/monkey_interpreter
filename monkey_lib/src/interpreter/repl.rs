use std::io::{Stdin, Stdout};
use std::io::{ Read, Write };
use interpreter::lexer::Lexer;
use interpreter::token::Token;
use interpreter::parser::Parser;
use interpreter::evaluator::Evaluator;

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
        loop {
            print!("{}", PROMPT);
            stdout.flush().unwrap();
            let mut buffer = String::new();
            stdin.read_line(&mut buffer);

            let mut l = Lexer::new(buffer);
            let mut p = Parser::new(l);


            let program = p.parse_program();
            if let Some(pro) = program {
                if p.get_errors().len() != 0 {
                    Repl::print_parse_errors(stdout, &p.get_errors());
                    continue;
                }

                let evaluated = Evaluator::eval_program(&pro);
                write!(stdout, "{}", evaluated);
                write!(stdout, "\n");
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
