use interpreter::lexer::Lexer;
use interpreter::token::Token;
use interpreter::program::Program;
use interpreter::ast::{ Statement, Identifier, StatementKind, Expression, ExpressionKind, NodeType };
use std::any::Any;
use std::collections::HashMap;
use std::sync::Arc;

type PrefixFn = fn(&mut Parser) -> Expression;
type InfixFn = fn(&mut Parser, expr: Expression) -> Expression;

pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <>
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call         // myFunction(X)
}

impl Precedence {
    fn precedence(&self) -> usize {
        match *self {
            Precedence::Lowest => 1,
            Precedence::Equals => 2,
            Precedence::LessGreater => 3,
            Precedence::Sum => 4,
            Precedence::Product => 5,
            Precedence::Prefix => 6,
            Precedence::Call => 7,
        }
    }
}

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<usize, PrefixFn>,
    infix_parse_fns: HashMap<usize, InfixFn>
}

impl Parser {
    fn new(lexer: Lexer) -> Parser {
        let mut p = Parser {
            lexer: lexer,
            cur_token: Token::Illegal,
            peek_token: Token::Illegal,
            errors: Vec::new(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new()
        };

        // p.register_prefix(Token::Ident(String::from("")).id(), Parser::parse_identifier);
        // p.register_prefix(Token::Int(-1).id(), Parser::parse_integer_literal);

        // Read two tokens, so cur_token and peek_token are both set
        p.next_token();
        p.next_token();

        return p;
    }

    fn register_prefix(&mut self, token_id: usize, prefix: PrefixFn) {
        self.prefix_parse_fns.insert(token_id, prefix);
    }

    fn register_infix(&mut self, token_id: usize, infix: InfixFn) {
        self.infix_parse_fns.insert(token_id, infix);
    }

    fn get_errors(&self) -> &Vec<String> {
        return &self.errors;
    }

    fn peek_error(&mut self, t: Token) {
        let msg = format!("expected next token to be {}, got {} instead.", t, self.peek_token);
        self.errors.push(msg);
    }

    fn no_prefix_parse_fn_error(&mut self, t: &Token) {
        let msg = format!("no prefix parse function for {} found", t);
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
                let expression_stmt = self.parse_expression_statement();
                match expression_stmt {
                    Some(s) => {
                        return Some(Statement::new(s));
                    },
                    None => { return None; }
                }
                return None;
            }
        }
    }

    fn parse_identifier(&mut self) -> Expression {
        let ident = match self.cur_token {
            Token::Ident(ref s) => s.clone(),
            _ => String::from("")
        };

        return Expression {
            exprKind: ExpressionKind::Ident(self.cur_token.clone(), ident)
        }
    }

    fn parse_integer_literal(&mut self) -> Expression {
        let integer = match self.cur_token {
            Token::Int(ref i) => *i,
            _ => {
                let msg = format!("could not parse {} as integer", self.cur_token);
                self.errors.push(msg);
                -1
            }
        };

        return Expression {
            exprKind: ExpressionKind::IntegerLiteral(self.cur_token.clone(), integer)
        }
    }

    fn parse_prefix_expression(&mut self) -> Expression {
        let tok = self.cur_token.clone();

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix);

        Expression {
            exprKind: ExpressionKind::PrefixExpression(tok.clone(), format!("{}", tok), Arc::new(right.unwrap_or_default()))
        }
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Expression {
        let tok = self.cur_token.clone();
        let op = format!("{}", tok);

        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence).unwrap_or_default();

        Expression {
            exprKind: ExpressionKind::InfixExpression(tok, Arc::new(left), op, Arc::new(right))
        }
    }

    fn prefix_parse_table(&mut self, token: &Token) -> Option<Expression> {
        match *token {
            Token::Ident(..) => Some(self.parse_identifier()),
            Token::Int(..) => Some(self.parse_integer_literal()),
            Token::Bang | Token::Minus => Some(self.parse_prefix_expression()),
            Token::True | Token::False => Some(self.parse_boolean()),
            _ => None
        } 
    }

    fn infix_parse_table(&mut self, token: &Token, expr: Expression) -> Option<Expression> {
        match *token {
            Token::Plus | Token::Minus
                | Token::Slash | Token::Asterisk
                | Token::EqualEqual | Token::NotEqual
                | Token::Lt | Token::Gt => Some(self.parse_infix_expression(expr)),
            _ => None
        }
    }

    fn precedences(&self, token: &Token) -> Precedence {
        match *token {
            Token::EqualEqual | Token::NotEqual => Precedence::Equals,
            Token::Lt | Token::Gt => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk => Precedence::Product,
            _ => Precedence::Lowest
            
        }
    }

    fn peek_precendence(&self) -> Precedence {
        self.precedences(&self.peek_token)
    }

    fn cur_precedence(&self) -> Precedence {
        self.precedences(&self.cur_token)
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

    fn parse_expression_statement(&mut self) -> Option<StatementKind> {
        let tok = self.cur_token.clone();
        let expr = self.parse_expression(Precedence::Lowest);

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        let stmt = StatementKind::ExpressionStatement(tok, expr);

        return Some(stmt);
    }

    fn parse_boolean(&mut self) -> Expression {
        Expression {
            exprKind: ExpressionKind::Boolean(self.cur_token.clone(), self.cur_token_is(Token::True))
        }
    }

    fn parse_expression(&mut self, pre: Precedence) -> Option<Expression> {
        // let lookup = self.prefix_parse_fns.get_mut(&self.cur_token.id());

        // let clone = lookup;
        
        // match clone {
        //     Some(prefix) => {
        //         Some(prefix(self))
        //     },
        //     None => None
        // }

        let tok = self.cur_token.clone();
        let mut prefix = self.prefix_parse_table(&tok);
        let tok = self.cur_token.clone();
        if prefix == None {
            self.no_prefix_parse_fn_error(&tok);
            return None
        }

        while !self.peek_token_is(Token::Semicolon) && pre.precedence() < self.peek_precendence().precedence() {
            self.next_token();
            let cur_tok = self.cur_token.clone();
            prefix = self.infix_parse_table(&cur_tok, prefix.unwrap_or_default());
        }

        return prefix;
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

    #[test]
    fn test_identifier_expression() {
        let input = r#"foobar;"#;

        let lexer = Lexer::new(String::from(input));
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_errors(&parser);

        match program {
            Some(p) => {
                assert!(p.statements.len() == 1, "program has not enough statements. got={}", p.statements.len());
                let stmt = p.statements[0].clone();
                match stmt.stmtKind {
                    StatementKind::ExpressionStatement(_, ref e) => {
                        let expr = e.clone();
                        match expr {
                            Some(e) => {
                                match e.exprKind {
                                    ExpressionKind::Ident(ref t, ref s) => {
                                        assert!(s == "foobar");
                                       let tok = t.clone(); 
                                        match tok {
                                            Token::Ident(ref ss) => {
                                                assert!(ss == "foobar");
                                            },
                                            _ => {
                                                assert!(false, "Token is not an Ident");
                                            }
                                        }
                                    },
                                    _ => {}
                                }
                            },
                            None => {
                                assert!(false, "exp not Idenfitier. got=None");
                            }
                        }
                        
                    },
                    _ => {
                        assert!(false, "program.statements[0] is not ExpressionStatement. got={}", stmt.stmtKind);
                    }
                }
            },
            None => {
                assert!(false, "Program is None.");
            }
        }

    }

    #[test]
    fn test_integer_literal_expression() {
        let input = r#"5;"#;

        let lexer = Lexer::new(String::from(input));
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_errors(&parser);

        match program {
            Some(p) => {
                assert!(p.statements.len() == 1, "program has not enough statements. got={}", p.statements.len());
                println!("Number of statements: {}", p.statements.len());
                let stmt = p.statements[0].clone();
                match stmt.stmtKind {
                    StatementKind::ExpressionStatement(_, ref e) => {
                        let expr = e.clone();
                        match expr {
                            Some(e) => {
                                match e.exprKind {
                                    ExpressionKind::IntegerLiteral(ref t, ref v) => {
                                        assert!(*v == 5);
                                       let tok = t.clone(); 
                                        match tok {
                                            Token::Int(ref i) => {
                                                assert!(*i == 5);
                                            },
                                            _ => {
                                                assert!(false, "Token is not an Ident");
                                            }
                                        }
                                    },
                                    _ => {
                                        
                                    }
                                }
                            },
                            None => {
                                assert!(false, "exp not Idenfitier. got=None");
                            }
                        }
                        
                    },
                    _ => {
                        assert!(false, "program.statements[0] is not ExpressionStatement. got={}", stmt.stmtKind);
                    }
                }
            },
            None => {
                assert!(false, "Program is None.");
            }
        }

    }

    #[test]
    fn test_boolean_expression() {
        let input = r#"true;"#;

        let lexer = Lexer::new(String::from(input));
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_errors(&parser);

        match program {
            Some(p) => {
                assert!(p.statements.len() == 1, "program has not enough statements. got={}", p.statements.len());
                println!("Number of statements: {}", p.statements.len());
                let stmt = p.statements[0].clone();
                match stmt.stmtKind {
                    StatementKind::ExpressionStatement(_, ref e) => {
                        let expr = e.clone();
                        match expr {
                            Some(e) => {
                                match e.exprKind {
                                    ExpressionKind::Boolean(ref t, ref b) => {
                                        assert!(*b == true);
                                        let tok = t.clone(); 
                                        match tok {
                                            Token::True => {
                                            },
                                            _ => {
                                                assert!(false, "Token is not an true");
                                            }
                                        }
                                    },
                                    _ => {
                                        
                                    }
                                }
                            },
                            None => {
                                assert!(false, "exp not Idenfitier. got=None");
                            }
                        }
                        
                    },
                    _ => {
                        assert!(false, "program.statements[0] is not ExpressionStatement. got={}", stmt.stmtKind);
                    }
                }
            },
            None => {
                assert!(false, "Program is None.");
            }
        }

    }

    #[test]
    fn test_parsing_prefix_expressions() {
        struct PrefixTests {
            input: String,
            operator: String,
            integer_value: Arc<Any>
        }

        let prefix_tests = vec![
            PrefixTests {input: String::from("!5"), operator: String::from("!"), integer_value: Arc::new(5)},
            PrefixTests {input: String::from("-15"), operator: String::from("-"), integer_value: Arc::new(15)},
            PrefixTests {input: String::from("!true"), operator: String::from("!"), integer_value: Arc::new(true)},
            PrefixTests {input: String::from("!false"), operator: String::from("!"), integer_value: Arc::new(false)}
        ];

        for pt in prefix_tests {
            let lexer = Lexer::new(String::from(pt.input));
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parse_errors(&parser);

            match program {
                Some(p) => {
                    assert!(p.statements.len() == 1, "program has not enough statements. got={}", p.statements.len());
                    println!("Number of statements: {}", p.statements.len());
                    let stmt = p.statements[0].clone();
                    match stmt.stmtKind {
                        StatementKind::ExpressionStatement(_, ref e) => {
                            let expr = e.clone();
                            match expr {
                                Some(e) => {
                                    match e.exprKind {
                                        ExpressionKind::PrefixExpression(ref t, ref op, ref r) => {
                                            assert!(*op == pt.operator);
                                            let ref right_exp = *r.clone();
                                            // if !test_integer_literal(right_exp, pt.integer_value) {
                                            if !test_literal_expression(right_exp, &pt.integer_value) {
                                                return;
                                            }
                                            // let tok = t.clone(); 
                                            // match tok {
                                            //     Token::Int(ref i) => {
                                            //         assert!(*i == 5);
                                            //     },
                                            //     _ => {
                                            //         assert!(false, "Token is not an Ident");
                                            //     }
                                            // }
                                        },
                                        _ => {

                                        }
                                    }
                                },
                                None => {
                                    assert!(false, "exp not Idenfitier. got=None");
                                }
                            }

                        },
                        _ => {
                            assert!(false, "program.statements[0] is not ExpressionStatement. got={}", stmt.stmtKind);
                        }
                    }
                },
                None => {
                    assert!(false, "Program is None.");
                }
            }
        }

    }

    #[test]
    fn test_parsing_infix_expressions() {
        struct InfixTestCase {
            input: String,
            left_value: Arc<Any>,
            operator: String,
            right_value: Arc<Any>
        }

        let test_cases = vec![
            InfixTestCase { input: String::from("5 + 5"), left_value: Arc::new(5), operator: String::from("+"), right_value: Arc::new(5)},
            InfixTestCase { input: String::from("5 - 5"), left_value: Arc::new(5), operator: String::from("-"), right_value: Arc::new(5)},
            InfixTestCase { input: String::from("5 * 5"), left_value: Arc::new(5), operator: String::from("*"), right_value: Arc::new(5)},
            InfixTestCase { input: String::from("5 / 5"), left_value: Arc::new(5), operator: String::from("/"), right_value: Arc::new(5)},
            InfixTestCase { input: String::from("5 > 5"), left_value: Arc::new(5), operator: String::from(">"), right_value: Arc::new(5)},
            InfixTestCase { input: String::from("5 < 5"), left_value: Arc::new(5), operator: String::from("<"), right_value: Arc::new(5)},
            InfixTestCase { input: String::from("5 == 5"), left_value: Arc::new(5), operator: String::from("=="), right_value: Arc::new(5)},
            InfixTestCase { input: String::from("5 != 5"), left_value: Arc::new(5), operator: String::from("!="), right_value: Arc::new(5)},
            InfixTestCase { input: String::from("true == true"), left_value: Arc::new(true), operator: String::from("!="), right_value: Arc::new(5)},
            InfixTestCase { input: String::from("5 != 5"), left_value: Arc::new(5), operator: String::from("!="), right_value: Arc::new(5)},
            InfixTestCase { input: String::from("5 != 5"), left_value: Arc::new(5), operator: String::from("!="), right_value: Arc::new(5)},
        ];

        for tt in test_cases {
            let lexer = Lexer::new(tt.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().unwrap_or_default();
            check_parse_errors(&parser);
            
            if program.statements.len() != 1 {
                assert!(false, "program.statements does not contain {} statements. got={}", 1, program.statements.len());
            }
            else {
                let stmt = program.statements[0].clone();
                match stmt.stmtKind {
                    StatementKind::ExpressionStatement(ref t, ref e) => {
                        let ex = e.clone();
                        let exp = ex.unwrap_or_default();
                        match exp.exprKind {
                            ExpressionKind::InfixExpression(ref t, ref l, ref o, ref r) => {
                                // if !test_integer_literal(l, *tt.left_value) {
                                if !test_literal_expression(l, &tt.left_value) {
                                    return;
                                }

                                if *o != tt.operator {
                                    assert!(false, "e.operator is not '{}'. got={}", tt.operator, *o);
                                }

                                // if !test_integer_literal(r, tt.right_value) {
                                if !test_literal_expression(r, &tt.right_value) {
                                    return;
                                }
                            },
                            _ => {
                                assert!(false, "e is not InfixExpression. got={}", exp.exprKind);
                            }
                        }
                    },
                    _ => {
                        assert!(false, "exp is not ExpressionStatement. got={}", stmt.stmtKind);
                    }
                }
            }
        }
    }

    fn test_integer_literal(exp: &Expression, value: i64) -> bool {
        let result = match exp.exprKind {
            ExpressionKind::IntegerLiteral(ref t, ref v) => {
                let tok_value = match *t {
                    Token::Int(ref i) => *i,
                    _ => {
                        -1
                    }
                };
                if *v != value {
                    println!("value not {}. got {}", value, *v);
                    false
                }
                else if tok_value != value {
                    println!("Token::Int value is not {}. got={}", value, tok_value);
                    false
                }
                else {
                    true
                }
            },
            _ => {
                println!("exp is not an ItergerLiteral. got{}", exp.exprKind);
                false
            }
        };

        return result;
    }

    fn test_identifier(exp: &Expression, value: String) -> bool {
        let result = match exp.exprKind {
            ExpressionKind::Ident(ref t, ref v) => {
                let tok_value = match *t {
                    Token::Ident(ref i) => i.clone(),
                    _ => {
                        String::from("")
                    }
                };

                if *v != value {
                    println!("value is not {}. got={}", value, *v);
                    false
                }
                else if tok_value != value {
                    println!("Token::Int value is not {}. got={}", value, tok_value);
                    false
                }
                else {
                    true
                }
            },
            _ => {
                println!("exp is not an Identifier. got={}", exp.exprKind);
                false
            }
        };

        return result;
    }

    fn test_boolean_literal(exp: &Expression, value: bool) -> bool {
        let result = match exp.exprKind {
            ExpressionKind::Boolean(ref t, ref b) => {
                let tok_value = match *t {
                    Token::True => {
                        if value {
                            false
                        }
                        else {
                            true
                        }
                    },
                    Token::False => {
                        if !value {
                            false
                        }
                        else {
                            true
                        }
                    },
                    _ => {
                        false
                    }
                };

                if tok_value != value {
                    println!("Value not {}. got={}", value, t);
                    false
                }
                else if *b != value {
                    println!("Token is not {}. got={}", value, *b);
                    false
                }
                else {
                    true
                }
            },
            _ => {
                println!("Did not get a boolean token. got={}", exp.exprKind);
                false
            }
        };

        return result;
    }

    fn test_literal_expression(exp: &Expression, expected: &Any) -> bool {
        if expected.is::<i32>() {
            if let Some(i) = expected.downcast_ref::<i32>() {
                return test_integer_literal(exp, *i as i64);
            }
        }
        else if expected.is::<i64>() {
            if let Some(i) = expected.downcast_ref::<i64>() {
                return test_integer_literal(exp, *i);
            }
        }
        else if expected.is::<String>() {
            if let Some(s) = expected.downcast_ref::<String>() {
                return test_identifier(exp, s.clone());
            }
        }
        else if expected.is::<bool>() {
            if let Some(b) = expected.downcast_ref::<bool>() {
                return test_boolean_literal(exp, *b);
            }
        }
        
        println!("type of exp not handled. got={}", exp.exprKind);
        false
    }

    fn test_infix_expression(exp: &Expression, left: &Any, operator: String, right: &Any) -> bool {
        match exp.exprKind {
            ExpressionKind::InfixExpression(ref t, ref l, ref o, ref r) => {
                if let Some(left_exp) = left.downcast_ref::<Expression>() {
                    if !test_literal_expression(l, left_exp) {
                        return false;
                    }
                }

                if *o != operator {
                    println!("exp.Operator is not '{}'. got={}", operator, *o);
                    return false;
                }

                if let Some(right_exp) = right.downcast_ref::<Expression>() {
                    if !test_literal_expression(r, right_exp) {
                        return false;
                    }
                }

            },
            _ => {
                
            }
        }

        return true;
    }

    #[test]
    fn test_operator_precedence_parsing() {
        struct TestCase {
            input: String,
            expected: String
        }

        let test_cases = vec![
            TestCase { input: String::from("-a * b"), expected: String::from("((-a) * b)")},
            TestCase { input: String::from("!-a"), expected: String::from("(!(-a))")},
            TestCase { input: String::from("a + b + c"), expected: String::from("((a + b) + c)")},
            TestCase { input: String::from("a + b - c"), expected: String::from("((a + b) - c)")},
            TestCase { input: String::from("a * b * c"), expected: String::from("((a * b) * c)")},
            TestCase { input: String::from("a * b / c"), expected: String::from("((a * b) / c)")},
            TestCase { input: String::from("a + b / c"), expected: String::from("(a + (b / c))")},
            TestCase { input: String::from("a + b * c + d / e - f"), expected: String::from("(((a + (b * c)) + (d / e)) - f)")},
            TestCase { input: String::from("3 + 4; -5 * 5"), expected: String::from("(3 + 4)((-5) * 5)")},
            TestCase { input: String::from("5 > 4 == 3 < 4"), expected: String::from("((5 > 4) == (3 < 4))")},
            TestCase { input: String::from("5 < 4 != 3 > 4"), expected: String::from("((5 < 4) != (3 > 4))")},
            TestCase { input: String::from("3 + 4 * 5 == 3 * 1 + 4 * 5"), expected: String::from("((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))")},
            TestCase { input: String::from("3 + 4 * 5 == 3 * 1 + 4 * 5"), expected: String::from("((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))")},
            TestCase { input: String::from("true"), expected: String::from("true")},
            TestCase { input: String::from("false"), expected: String::from("false")},
            TestCase { input: String::from("3 > 5 == false"), expected: String::from("((3 > 5) == false)")},
            TestCase { input: String::from("3 < 5 == true"), expected: String::from("((3 < 5) == true)")},
        ];

        for tt in test_cases {
            let lexer = Lexer::new(tt.input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program().unwrap_or_default();
            check_parse_errors(&parser);

            let actual = format!("{}", program);
            assert!(actual == tt.expected, "expected={}, got={}", tt.expected, actual);
        }
    }
}
