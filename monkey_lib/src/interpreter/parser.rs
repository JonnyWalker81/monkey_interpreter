use interpreter::lexer::Lexer;
use interpreter::token::Token;
use interpreter::program::Program;
use interpreter::ast::{ Statement, Identifier, StatementKind, Expression, ExpressionKind, NodeType };
use std::any::Any;

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>
}

impl Parser {
    fn new(lexer: Lexer) -> Parser {
        let mut p = Parser {lexer: lexer, cur_token: Token::Illegal, peek_token: Token::Illegal, errors: Vec::new()};

        // Read two tokens, so cur_token and peek_token are both set
        p.next_token();
        p.next_token();

        return p;
    }

    fn get_errors(&self) -> &Vec<String> {
        return &self.errors;
    }

    fn peek_errors(&mut self, t: Token) {
        let msg = format!("expected next token to be {}, got {} instead.", t, self.peek_token);
        self.errors.push(msg);
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program::new();

        while !self.cur_token_is(Token::Eof) {
            let stmt = self.parse_statement();
            match stmt {
                Some(s) => {
                    program.statements.push(s);
                },
                None => {
                    
                }
            }

            self.next_token();
        }
        return Some(program);
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Token::Let => {
                println!("In Let branch of match...");
                return Some(Statement::new(self.parse_let_statement().unwrap()));
            },
            _ => {
                return None;
            }
        }
    }

    fn parse_let_statement(&mut self) -> Option<StatementKind> {
        println!("Token -> {}", self.peek_token);
        let peek = self.peek_token.clone();
        let identifier = match peek {
            Token::Ident(ref s) => {
                println!("Ident branch of match...");
                self.next_token();
                Identifier{token: self.cur_token.clone(), value: s.clone()}
            },
            _ => {
                return None;
            }
        };

        let stmt = match self.peek_token {
            Token::Assign => {
                while !self.cur_token_is(Token::Semicolon) {
                    self.next_token();
                }

                StatementKind::LetStatement(Token::Let, identifier, None)
            },
            _ => {
                return None;
            }
        };

        return Some(stmt);
    }

    fn cur_token_is(&self, t: Token) -> bool {
        return self.cur_token == t;
    }

    fn peek_token_is(&self, t: Token) -> bool {
        return self.peek_token == t;
    }

    fn expect_peek(&mut self, t: Token) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            return true;
        }
        else {
            return false;
        }
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
            None =>{ println!("parse_program() returned None.");
                assert!(false);
            }
        };
    }

    fn test_let_statement(stmt: &Statement, test_case: &TestCase) -> bool {
        if let StatementKind::LetStatement(t, i, e) = stmt.stmtKind.clone() {
            let name = i.value;
            // let tok = i.token;

            if name != test_case.expected_identifier {
                println!("letStmt.Name.Value not '{}'. got={}", test_case.expected_identifier, name);
                assert!(false);
                return false;
            }
        }
        else {
            println!("s.token_literal not 'let'. got={}", stmt.stmtKind);
            return false;
        }
        
        // match stmt.stmtKind {
        //     StatementKind::LetStatement(_, _, _) => {
                
        //     },
        //     _ => println!("s.token_literal not 'let'. got={}", stmt.stmtKind)
        // };
        
        // if s.node_type() != NodeType::LetStatement {
        //     println!("s not LetStatement. got={}", st);
        // }

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
