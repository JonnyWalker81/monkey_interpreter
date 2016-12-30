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

    fn peek_error(&mut self, t: Token) {
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
                let let_stmt = self.parse_let_statement();
                match let_stmt {
                    Some(s) => {
                        return Some(Statement::new(s));
                    },
                    None => {return None;}
                }
            },
            Token::Return => {
                let return_stmt = self.parse_return_statement();
                match return_stmt {
                    Some(s) => {
                        return Some(Statement::new(s));
                    },
                    None => { return None; }
                }
            }
            _ => {
                return None;
            }
        }
    }

    fn parse_let_statement(&mut self) -> Option<StatementKind> {
        println!("Token -> {}", self.peek_token);
        if !self.peek_token_is_ident() {
            return None;
        }

        let cur = self.cur_token.clone();
        let identifier = match cur {
            Token::Ident(ref s) => {
                // self.next_token();
                println!("cur match...");
                Identifier{token: cur.clone(), value: s.clone()}
            },
            _ => {
                return None;
            }
        };


        if !self.expect_peek(Token::Assign) {
            println!("not Assign token..");
           return None;
        }

        let stmt = match self.cur_token {
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

    fn parse_return_statement(&mut self) -> Option<StatementKind> {
        let token = self.cur_token.clone();
        let stmt = StatementKind::ReturnStatement(token, None);

        self.next_token();

        while !self.cur_token_is(Token::Semicolon) {
            self.next_token();
        }

        return Some(stmt);
    }

    fn peek_token_is_ident(&mut self) -> bool {
        if let Token::Ident(..) = self.peek_token {
            self.next_token();
            return true;
        }
        else {
            let t = self.peek_token.clone();
            println!("peek_error -> {}", t);
            self.peek_error(Token::Ident(String::from("")));
            return false;
        }
    }

    fn cur_token_is(&self, t: Token) -> bool {
        return self.cur_token == t;
    }

    fn peek_token_is(&self, t: Token) -> bool {
        return self.peek_token == t;
    }

    fn expect_peek(&mut self, t: Token) -> bool {
        let checkToken = t.clone();
        if self.peek_token_is(checkToken) {
            self.next_token();
            return true;
        }
        else {
            self.peek_error(t);
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
        check_parse_errors(&parser);

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

    fn check_parse_errors(parser: &Parser) {
        let errors = parser.get_errors();

        if errors.len() == 0 {
            return;
        }

        println!("Parser has {} errors", errors.len());
        for error in errors {
            println!("parser error: {} ", error);
        }

        assert!(false);
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

    #[test]
    fn test_return_statements() {
        let input = r#"return 5;
        return 10;
        return 993322;"#;

        let lexer = Lexer::new(String::from(input));
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_errors(&parser);

        match program {
            Some(p) => {
                if p.statements.len() != 3 {
                    println!("program.statements does not contain 3 statements. got = {}", p.statements.len());
                    assert!(false);
                }

                for s in p.statements {
                    match s.stmtKind {
                        StatementKind::ReturnStatement(ref t, _) => {
                            match *t {
                                Token::Return => {
                                },
                                _ => {
                                    println!("return literal not 'return', got={}", t);
                                }
                            } 
                        },
                        _ => {
                            println!("stmt not a returnStatement. got={}", s.stmtKind);
                            assert!(false);
                        }
                    } 
                }
            },
            None => {
            }
        }
    }
}
