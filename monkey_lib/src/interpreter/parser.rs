use interpreter::lexer::Lexer;
use interpreter::token::Token;
use interpreter::program::Program;
use interpreter::ast::{ Statement, LetStatement, NodeType };
use std::any::Any;

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token
}

impl Parser {
    fn new(lexer: Lexer) -> Parser {
        let mut p = Parser {lexer: lexer, cur_token: Token::Illegal, peek_token: Token::Illegal};

        // Read two tokens, so cur_token and peek_token are both set
        p.next_token();
        p.next_token();

        return p;
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Option<Program> {
        return None;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    struct TestCase {
        expected_identifier: String
    }

    #[test]
    fn test_let_statements() {
        

        let test_cases = vec![
            TestCase{expected_identifier: String::from("x")},
            TestCase{expected_identifier: String::from("y")},
            TestCase{expected_identifier: String::from("foobar")}
        ];

        let input = r#"let x = 5;
                       let y = 10;
                       let foobar = 838383;"#;

        let lexer = Lexer::new(String::from(input));
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        match program {
            Some(p) => {
                if p.statements.len() != 3 {
                    println!("program.statements does not contain 3 statements, got={}", p.statements.len());
                    for i in 0..test_cases.len() {
                        let stmt = &p.statements[i];
                        let test_case = &test_cases[i];
                        if !tests::test_let_statement(stmt, test_case) {
                            return
                        }
                    }
                } 
            },
            None => println!("parse_program() returned None.")
        };
    }

    fn test_let_statement(stmt: &Box<Statement>, test_case: &TestCase) -> bool {
        let ref s = *stmt;
        let st = s.token_literal();
        if st != Token::Let {
            println!("s.token_literal not 'let'. got={}", st);
            return false;
        }

        if s.node_type() != NodeType::LetStatement {
            println!("s not LetStatement. got={}", st);
        }

        // if let Ok(ls) = s.downcast::<LetStatement>() {
        //     println!("Downcast worked...");
        // }

        // if let Ok(ls) = (*s as Box<Any + 'static>).downcast() {
            
        // }

        // if s.name.value != test_case.expected_identifier {
        //     println!("letStmt.Name.Value not '{}'. got={}", test_case.expected_identifier, s.name.value);
        //     return false;
        // }

        return true;
    }
}
