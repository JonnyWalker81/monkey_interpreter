use interpreter::lexer::Lexer;
use interpreter::token::Token;
use interpreter::program::Program;
use interpreter::ast::{ Statement, Identifier, StatementKind, Expression, ExpressionKind, NodeType, BlockStatement };
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

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<usize, PrefixFn>,
    infix_parse_fns: HashMap<usize, InfixFn>
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
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

    pub fn get_errors(&self) -> &Vec<String> {
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

    pub fn parse_program(&mut self) -> Option<Program> {
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
            Token::LParen => Some(self.parse_grouped_expression()),
            Token::If => Some(self.parse_if_expression()),
            Token::Function => Some(self.parse_functional_literal()),
            Token::StringToken(..) => Some(self.parse_string_literal()),
            Token::LBracket => Some(self.parse_array_literal()),
            _ => None
        } 
    }

    fn infix_parse_table(&mut self, token: &Token, expr: Expression) -> Option<Expression> {
        match *token {
            Token::Plus | Token::Minus
                | Token::Slash | Token::Asterisk
                | Token::EqualEqual | Token::NotEqual
                | Token::Lt | Token::Gt => Some(self.parse_infix_expression(expr)),
            Token::LParen => Some(self.parse_call_expression(expr)),
            _ => None
        }
    }

    fn precedences(&self, token: &Token) -> Precedence {
        match *token {
            Token::EqualEqual | Token::NotEqual => Precedence::Equals,
            Token::Lt | Token::Gt => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk => Precedence::Product,
            Token::LParen => Precedence::Call,
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
                // while !self.cur_token_is(Token::Semicolon) {
                //     self.next_token();
                // }

                self.next_token();

                let value = self.parse_expression(Precedence::Lowest);

                if self.peek_token_is(Token::Semicolon) {
                    self.next_token();
                }

                StatementKind::LetStatement(Token::Let, identifier, value)
            },
            _ => {
                return None;
            }
        };

        return Some(stmt);
    }

    fn parse_return_statement(&mut self) -> Option<StatementKind> {
        let token = self.cur_token.clone();

        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest);
        
        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }
        
        let stmt = StatementKind::ReturnStatement(token, return_value);

        return Some(stmt);
    }

    fn parse_array_literal(&mut self) -> Expression {
        let tok = self.cur_token.clone();
        let elements = self.parse_expression_list(Token::RBracket);

        Expression {
            exprKind: ExpressionKind::ArrayLiteral(tok, elements)
        }
    }

    fn parse_expression_list(&mut self, end: Token) -> Vec<Expression> {
        let mut list = Vec::new();

        if self.peek_token_is(end.clone()) {
            self.next_token();
            return list;
        }

        self.next_token();
        if let Some(e) = self.parse_expression(Precedence::Lowest) {
            list.push(e);
        }

        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();
            if let Some(e) = self.parse_expression(Precedence::Lowest) {
                list.push(e);
            }
        }

        if !self.expect_peek(end.clone()) {
            return list; // this may not work as expected
        }

        return list;
    }

    fn parse_string_literal(&self) -> Expression {
        Expression {
            exprKind: ExpressionKind::StringLiteral(self.cur_token.clone(), format!("{}", self.cur_token))
        }
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

    fn parse_grouped_expression(&mut self) -> Expression {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest).unwrap_or_default();

        if !self.expect_peek(Token::RParen) {
            return Expression::default();
        }

        return exp;
    }

    fn parse_boolean(&mut self) -> Expression {
        Expression {
            exprKind: ExpressionKind::Boolean(self.cur_token.clone(), self.cur_token_is(Token::True))
        }
    }

    fn parse_if_expression(&mut self) -> Expression {
        let cur_tok = self.cur_token.clone();

        if !self.expect_peek(Token::LParen) {
            return Expression::default();
        }

        self.next_token();
        let condition = match self.parse_expression(Precedence::Lowest) {
            Some(ex) => ex,
            None => Expression::default()
        };

        if !self.expect_peek(Token::RParen) {
            return Expression::default();
        }

        if !self.expect_peek(Token::LBrace) {
            return Expression::default();
        }

        let consequence = self.parse_block_statement();

        let mut alt: Option<BlockStatement> = None;
        if self.peek_token_is(Token::Else) {
            self.next_token();

            if !self.expect_peek(Token::LBrace) {
                return Expression::default();
            }

            alt = Some(self.parse_block_statement());            
        }

        return Expression {
            exprKind: ExpressionKind::If(cur_tok, Arc::new(condition), consequence, alt)
        };
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let cur_tok = self.cur_token.clone();

        self.next_token();

        let mut block = BlockStatement{ token: cur_tok, statements: Vec::new()};

        while !self.cur_token_is(Token::RBrace) {
            let stmt = self.parse_statement();
            if let Some(st) = stmt {
                block.statements.push(st.clone());
            }

            self.next_token();
        }

        return block;
    }

    fn parse_functional_literal(&mut self) -> Expression {
        let cur_tok = self.cur_token.clone();

        if !self.expect_peek(Token::LParen) {
            return Expression::default();
        }

        let parameters = self.parse_function_parameters();

        if !self.expect_peek(Token::LBrace) {
            return Expression::default();
        }

        let body = self.parse_block_statement();

        return Expression {
            exprKind: ExpressionKind::FunctionLiteral(cur_tok, parameters, body)
        }
    }

    fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut result = Vec::new();

        if self.peek_token_is(Token::RParen) {
            self.next_token();
            return result;
        }

        self.next_token();

        let mut cur_tok = self.cur_token.clone();
        let mut ident = match cur_tok {
            Token::Ident(ref s) => {
                Identifier { token: cur_tok.clone(), value: s.clone()}
            },
            _ => Identifier::default()
        };

        result.push(ident);

        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();
            cur_tok = self.cur_token.clone();
            ident = match cur_tok {
                Token::Ident(ref s) => {
                    Identifier { token: cur_tok.clone(), value: s.clone()}
                },
                _ => Identifier::default()
            };
            result.push(ident);
        }

        if !self.expect_peek(Token::RParen) {
            return Vec::new();
        }

        return result;
    }

    fn parse_call_expression(&mut self, function: Expression) -> Expression {
        let cur_tok = self.cur_token.clone();
        // let args = self.parse_call_arguments();
        let args = self.parse_expression_list(Token::RParen);

        Expression {
            exprKind: ExpressionKind::Call(cur_tok.clone(), Arc::new(function), args)
        }
    }

    fn parse_call_arguments(&mut self) -> Vec<Expression> {
        let mut args = Vec::new();

        if self.peek_token_is(Token::RParen) {
            self.next_token();
            return args;
        }

        self.next_token();
        args.push(self.parse_expression(Precedence::Lowest).unwrap());

        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::Lowest).unwrap());
        }

        if !self.expect_peek(Token::RParen) {
            return args;
        }

        return args;
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
        input: String,
        expected_identifier: String,
        expected_value_int: Option<i32>,
        expected_value_bool: Option<bool>,
        expected_value_str: Option<String>
    }

    #[test]
    fn test_let_statements() {
        

        let test_cases = vec![
            TestCase{input: String::from("let x = 5;"), expected_identifier: String::from("x"), expected_value_int: Some(5), expected_value_bool: None, expected_value_str: None},
            TestCase{input: String::from("let y = true;"), expected_identifier: String::from("y"), expected_value_int: None, expected_value_bool: Some(true), expected_value_str: None},
            TestCase{input: String::from("let foobar = y;"), expected_identifier: String::from("foobar"), expected_value_int: None, expected_value_bool: None, expected_value_str: Some(String::from("y"))}
        ];

        // let input = r#"let x = 5;
        //                let y = 10;
        //                let foobar = 838383;"#;


        for tt in test_cases {
        
            let lexer = Lexer::new(String::from(tt.input.clone()));
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parse_errors(&parser);

            match program {
                Some(p) => {
                    if p.statements.len() != 1 {
                        println!("program.statements does not contain 3 statements, got={}", p.statements.len());
                        // for i in 0..test_cases.len() {
                        //     let stmt = &p.statements[i];
                        //     let test_case = &test_cases[i];
                        //     if !tests::test_let_statement(stmt, test_case) {
                        //         return
                        //     }
                        // }
                    } 

                    let stmt = p.statements[0].clone();
                    if !test_let_statement(&stmt, &tt) {
                        assert!(false, "test_let_statement failed. expected={} got={}", tt.expected_identifier, stmt.stmtKind);
                    }

                    match stmt.stmtKind {
                        StatementKind::LetStatement(ref t, ref i, ref e) => {
                            if let Some(ex) = e.clone() {
                                if let Some(evi) = tt.expected_value_int {
                                    if !test_literal_expression(&ex, &evi) {
                                        assert!(false, "test_literal_expression failed, expected={}  got={}", evi, ex.exprKind);
                                    }
                                }

                                if let Some(evb) = tt.expected_value_bool {
                                    if !test_literal_expression(&ex, &evb) {
                                        assert!(false, "test_literal_expression failed, expected={}  got={}", evb, ex.exprKind);
                                    }
                                }

                                if let Some(evs) = tt.expected_value_str.clone() {
                                    if !test_literal_expression(&ex, &evs) {
                                        assert!(false, "test_literal_expression failed, expected={}  got={}", evs, ex.exprKind);
                                    }
                                }
                            }
                        },
                        _ => {
                            
                        }
                    }
                },
                None =>{ println!("parse_program() returned None.");
                    assert!(false);
                }
            };

        }
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
        
        return true;
    }

    #[test]
    fn test_return_statements() {
        let input = r#"return 5;
        return 10;
        return 993322;"#;

        struct TestData {
            input: String,
            expected_value_int: Option<i32>,
            expected_value_bool: Option<bool>,
            expected_value_str: Option<String>
        }

        let tests = vec![
            TestData {input: String::from("return 5;"), expected_value_int: Some(5), expected_value_bool: None, expected_value_str: None},
            TestData {input: String::from("return true;"), expected_value_int: None, expected_value_bool: Some(true), expected_value_str: None},
            TestData {input: String::from("return foobar;"), expected_value_int: None, expected_value_bool: None, expected_value_str: Some(String::from("foobar"))}
        ];


        for tt in tests {
            let lexer = Lexer::new(String::from(tt.input.clone()));
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parse_errors(&parser);

            match program {
                Some(p) => {
                    if p.statements.len() != 1 {
                        println!("program.statements does not contain 1 statements. got = {}", p.statements.len());
                        assert!(false);
                    }

                    let stmt = p.statements[0].clone();
                    match stmt.stmtKind {
                        StatementKind::ReturnStatement(ref t, ref e) => {
                            assert!(*t == Token::Return);

                            if let Some(ex) = e.clone() {
                                if let Some(evi) = tt.expected_value_int {
                                    if !test_literal_expression(&ex, &evi) {
                                        assert!(false, "test_literal_expression failed...");
                                    }
                                }

                                if let Some(evb) = tt.expected_value_bool {
                                    if !test_literal_expression(&ex, &evb) {
                                        assert!(false, "test_literal_expression failed...");
                                    }
                                }

                                if let Some(evs) = tt.expected_value_str {
                                    if !test_literal_expression(&ex, &evs) {
                                        assert!(false, "test_literal_expression failed...");
                                    }
                                }
                            }
                        },
                        _ => {
                            
                        }
                    }
                },
                None => {
                }
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
        #[derive(Clone)]
        struct PrefixTests{
            input: String,
            operator: String,
            integer_value: Option<i32>,
            bool_value: Option<bool>
        }

        let prefix_tests = vec![
            PrefixTests {input: String::from("!5"), operator: String::from("!"), integer_value: Some(5), bool_value: None},
            PrefixTests {input: String::from("-15"), operator: String::from("-"), integer_value: Some(15), bool_value: None},
            PrefixTests {input: String::from("!true"), operator: String::from("!"), integer_value: None, bool_value: Some(true)},
            PrefixTests {input: String::from("!false"), operator: String::from("!"), integer_value: None, bool_value: Some(false)}
        ];

        for pt in prefix_tests {
            let lexer = Lexer::new(String::from(pt.input.clone()));
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

                                            // let pt_clone = pt.clone();
                                            // let int_clone = pt_clone.integer_value.clone();
                                            //     let expected = Arc::downgrade(&int_clone);
                                            //     let expected_val = &expected.clone();
                                            if let Some(iv) = pt.integer_value {
                                                if !test_literal_expression(right_exp, &iv) {
                                                    assert!(false, "right_exp does not match. got={}", right_exp.exprKind);
                                                }
                                            }

                                            if let Some(b) = pt.bool_value {
                                                if !test_literal_expression(right_exp, &b) {
                                                    println!("b: {}", b);
                                                    assert!(false, "right_exp does not match. got={}", right_exp.exprKind);
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

    }

    #[test]
    fn test_parsing_infix_expressions() {
        struct InfixTestCase {
            input: String,
            left_value_int: Option<i32>,
            left_value_bool: Option<bool>,
            operator: String,
            right_value_int: Option<i32>,
            right_value_bool: Option<bool>
        }

        let test_cases = vec![
            InfixTestCase { input: String::from("5 + 5"), left_value_int: Some(5), left_value_bool: None, operator: String::from("+"), right_value_int: Some(5), right_value_bool: None},
            InfixTestCase { input: String::from("5 - 5"), left_value_int: Some(5), left_value_bool: None, operator: String::from("-"), right_value_int: Some(5), right_value_bool: None},
            InfixTestCase { input: String::from("5 * 5"), left_value_int: Some(5), left_value_bool: None, operator: String::from("*"), right_value_int: Some(5), right_value_bool: None},
            InfixTestCase { input: String::from("5 / 5"), left_value_int: Some(5), left_value_bool: None, operator: String::from("/"), right_value_int: Some(5), right_value_bool: None},
            InfixTestCase { input: String::from("5 > 5"), left_value_int: Some(5), left_value_bool: None, operator: String::from(">"), right_value_int: Some(5), right_value_bool: None},
            InfixTestCase { input: String::from("5 < 5"), left_value_int: Some(5), left_value_bool: None, operator: String::from("<"), right_value_int: Some(5), right_value_bool: None},
            InfixTestCase { input: String::from("5 == 5"), left_value_int: Some(5), left_value_bool: None, operator: String::from("=="), right_value_int: Some(5), right_value_bool: None},
            InfixTestCase { input: String::from("5 != 5"), left_value_int: Some(5), left_value_bool: None, operator: String::from("!="), right_value_int: Some(5), right_value_bool: None},
            InfixTestCase { input: String::from("true == true"), left_value_int: None, left_value_bool: Some(true), operator: String::from("=="), right_value_int: None, right_value_bool: Some(true)},
            InfixTestCase { input: String::from("5 != 5"), left_value_int: Some(5),  left_value_bool: None, operator: String::from("!="), right_value_int: Some(5), right_value_bool: None},
            InfixTestCase { input: String::from("5 != 5"), left_value_int: Some(5),  left_value_bool: None, operator: String::from("!="), right_value_int: Some(5), right_value_bool: None},
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
                                if let Some(lvi) = tt.left_value_int {
                                    if !test_literal_expression(l, &lvi) {
                                        assert!(false, "left value does not match. got={}", l.exprKind);
                                    }
                                }

                                if let Some(lvb) = tt.left_value_bool {
                                    if !test_literal_expression(l, &lvb) {
                                        assert!(false, "left value does not match. got={}", l.exprKind);
                                    }
                                }

                                if *o != tt.operator {
                                    assert!(false, "e.operator is not '{}'. got={}", tt.operator, *o);
                                }

                                if let Some(rvi) = tt.right_value_int {
                                    if !test_literal_expression(r, &rvi) {
                                        assert!(false, "right does not match. got={}", &r.exprKind);
                                    }
                                }

                                if let Some(rvb) = tt.right_value_bool {
                                    if !test_literal_expression(r, &rvb) {
                                        assert!(false, "right does not match. got={}", &r.exprKind);
                                    }
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
                            true
                        }
                        else {
                            false
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
                    println!("tok_value: {}", tok_value);
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
        if expected.is::<bool>() {
            if let Some(b) = expected.downcast_ref::<bool>() {
                return test_boolean_literal(exp, *b);
            }
        }
        else if expected.is::<i32>() {
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
        
        
        println!("type of exp not handled. got={}", exp.exprKind);
        println!("type. got={:?}", expected);
        assert!(false);
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
            TestCase { input: String::from("1 + (2 + 3) + 4"), expected: String::from("((1 + (2 + 3)) + 4)")},
            TestCase { input: String::from("(5 + 5) * 2"), expected: String::from("((5 + 5) * 2)")},
            TestCase { input: String::from("2 / ( 5 + 5)"), expected: String::from("(2 / (5 + 5))")},
            TestCase { input: String::from("-(5 + 5)"), expected: String::from("(-(5 + 5))")},
            TestCase { input: String::from("!(true == true)"), expected: String::from("(!(true == true))")},
            TestCase { input: String::from("a + add(b * c) + d"), expected: String::from("((a + add((b * c))) + d)")},
            TestCase { input: String::from("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))"), expected: String::from("add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))")},
            TestCase { input: String::from("add(a + b + c * d / f + g)"), expected: String::from("add((((a + b) + ((c * d) / f)) + g))")},
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

    #[test]
    fn test_if_expression() {
        let input = r#"if (x < y) { x }"#;

        let lexer = Lexer::new(String::from(input));
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap_or_default();
        check_parse_errors(&parser);

        if program.statements.len() != 1 {
            assert!(false, "program.Body does nto contain {} statements. got={}", 1, program.statements.len());
        }

        let stmt = program.statements[0].clone();
        match stmt.stmtKind {
            StatementKind::ExpressionStatement(ref t, ref e) => {
                let ex = e.clone();
                if let Some(exp) = ex {
                    match exp.exprKind {
                        ExpressionKind::If(ref t, ref con, ref cs, ref alt) => {
                            let left = String::from("x");
                            let operator = String::from("<");
                            let right = String::from("y");
                            if !test_infix_expression(con, &left, operator, &right) {
                                return;
                            } 

                            if cs.statements.len() != 1 {
                                assert!(false, "consequence is not 1 statements. got={}", cs.statements.len());
                            }

                            let csStmt = cs.statements[0].clone();
                            match csStmt.stmtKind {
                                StatementKind::ExpressionStatement(ref t, ref e) => {
                                    if let Some(ex) = e.clone() {
                                        if !test_identifier(&ex, String::from("x")) {
                                            return;
                                        }
                                    }
                                },
                                _ => {
                                    assert!(false, "Statements[0] is not ExpressionStatement. got={}", csStmt.stmtKind);
                                }
                            }

                            if let Some(a) = alt.clone() {
                                println!("Alternative was not None. got={}", a);
                            }
                        },
                        _ => {
                            assert!(false, "Expression is not an IfExpression. got={}", exp.exprKind);
                        }
                    }
                }
            },
            _ => {
                assert!(false, "program.statements[0] is not an ExpressionStatement. got={}", stmt.stmtKind);
            }
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = r#"if (x < y) { x } else { y }"#;

        let lexer = Lexer::new(String::from(input));
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap_or_default();
        check_parse_errors(&parser);

        if program.statements.len() != 1 {
            assert!(false, "program.Body does nto contain {} statements. got={}", 1, program.statements.len());
        }

        let stmt = program.statements[0].clone();
        match stmt.stmtKind {
            StatementKind::ExpressionStatement(ref t, ref e) => {
                let ex = e.clone();
                if let Some(exp) = ex {
                    match exp.exprKind {
                        ExpressionKind::If(ref t, ref con, ref cs, ref alt) => {
                            let left = String::from("x");
                            let operator = String::from("<");
                            let right = String::from("y");
                            if !test_infix_expression(con, &left, operator, &right) {
                                return;
                            } 

                            if cs.statements.len() != 1 {
                                assert!(false, "consequence is not 1 statements. got={}", cs.statements.len());
                            }

                            let csStmt = cs.statements[0].clone();
                            match csStmt.stmtKind {
                                StatementKind::ExpressionStatement(ref t, ref e) => {
                                    if let Some(ex) = e.clone() {
                                        if !test_identifier(&ex, String::from("x")) {
                                            return;
                                        }
                                    }
                                },
                                _ => {
                                    assert!(false, "Statements[0] is not ExpressionStatement. got={}", csStmt.stmtKind);
                                }
                            }

                            if let Some(a) = alt.clone() {
                                if a.statements.len() != 1 {
                                    println!("Alternate.statements does not contain 1 statements. got={}", a.statements.len());
                                }

                                let altStmt = a.statements[0].clone();
                                match altStmt.stmtKind {
                                    StatementKind::ExpressionStatement(ref t, ref e) => {
                                        if let Some(ex) = e.clone() {
                                            if !test_identifier(&ex, String::from("y")) {
                                                return;
                                            }
                                        }
                                    },
                                    _ => {
                                        assert!(false, "Statements[0] is not an ExpressionStatement. got={}", altStmt.stmtKind);
                                    }
                                }
                            }
                        },
                        _ => {
                            assert!(false, "Expression is not an IfExpression. got={}", exp.exprKind);
                        }
                    }
                }
            },
            _ => {
                assert!(false, "program.statements[0] is not an ExpressionStatement. got={}", stmt.stmtKind);
            }
        }
    }

    #[test]
    fn test_function_literal() {
        let input = r#"fn(x, y) { x + y; }"#;

        let lexer = Lexer::new(String::from(input));
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap_or_default();
        check_parse_errors(&parser);

        if program.statements.len() != 1 {
            assert!(false, "program.Body does nto contain {} statements. got={}", 1, program.statements.len());
        }

        let stmt = program.statements[0].clone();
        match stmt.stmtKind {
            StatementKind::ExpressionStatement(ref t, ref e) => {
                if let Some(ex) = e.clone() {
                    match ex.exprKind {
                        ExpressionKind::FunctionLiteral(ref t, ref p, ref b) => {
                            if p.len() != 2 {
                                assert!(false, "function litertal parameters wrong. want 2, got={}", p.len());
                            }

                            let p0 = p[0].clone();
                            let exp0 = Expression { exprKind: ExpressionKind::Ident(p0.token, p0.value)};
                            let expected0 = String::from("x");
                            test_literal_expression(&exp0, &expected0);

                            let p1 = p[1].clone();
                            let exp1 = Expression { exprKind: ExpressionKind::Ident(p1.token, p1.value)};
                            let expected1 = String::from("y");
                            test_literal_expression(&exp1, &expected1);


                            if b.statements.len() != 1 {
                                assert!(false, "function.Body.statements has not 1 statements. got={}", b.statements.len());
                            }

                            let bodyStmt = b.statements[0].clone();
                            match bodyStmt.stmtKind {
                                StatementKind::ExpressionStatement(ref t, ref e) => {
                                    if let Some(ex) = e.clone() {
                                        let left = String::from("x");
                                        let op = String::from("+");
                                        let right = String::from("y");
                                        test_infix_expression(&ex, &left, op, &right);
                                    }
                                },
                                _ => {
                                    assert!(false, "function body stmt is not an ExpressionStatement. got={}", bodyStmt.stmtKind);
                                }
                            }
                        },
                        _ => {
                            assert!(false, "expression is not FunctionLiteral. got={}", ex.exprKind);
                        }
                    }
                }
            },
            _ => {
                assert!(false, "program.statements[0] is not an ExpressionStatement. gpot={}", stmt.stmtKind);
            }
        }
    }

    #[test]
    fn test_function_parameter_parsing() {
        struct TestData {
            input: String,
            expected_params: Vec<String>
        }

        let tests = vec![TestData {input: String::from("fn() {};"), expected_params: Vec::new()},
                         TestData {input: String::from("fn(x) {};"), expected_params: vec![String::from("x")]},
                         TestData {input: String::from("fn(x, y, z) {};"), expected_params: vec![String::from("x"), String::from("y"), String::from("z")]}
        ];

        for tt in tests {
            let lexer = Lexer::new(tt.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().unwrap_or_default();
            check_parse_errors(&parser);

            let stmt = program.statements[0].clone();
            match stmt.stmtKind {
                StatementKind::ExpressionStatement(ref t, ref e) => {
                    if let Some(ex) = e.clone() {
                        match ex.exprKind {
                            ExpressionKind::FunctionLiteral(ref t, ref p, ref b) => {
                                assert!(p.len() == tt.expected_params.len(), "Length parameters wrong. want {}, got={}", tt.expected_params.len(), p.len());

                                let mut count = 0;
                                for param in tt.expected_params {
                                    let func_param = p[count].clone();
                                    let func_expected = Expression { exprKind: ExpressionKind::Ident(func_param.token, func_param.value)};
                                    
                                    // test_literal_expression(&func_expected, &test_expected);
                                    test_literal_expression(&func_expected, &param);
                                    count += 1;
                                }
                            },
                            _ => {
                                
                            }
                        }
                    }
                },
                _ => {
                    
                }
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = String::from(r#"add(1, 2 * 3, 4 + 5);"#);

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap_or_default();
        check_parse_errors(&parser);

        if program.statements.len() != 1 {
            assert!(false, "program.statements does not contain {} statements. got={}", 1, program.statements.len());
        }

        let stmt = program.statements[0].clone();
        match stmt.stmtKind {
            StatementKind::ExpressionStatement(ref t, ref e) => {
                if let Some(ex) = e.clone() {
                    match ex.exprKind {
                        ExpressionKind::Call(ref t, ref f, ref a) => {
                            if !test_identifier(&f, String::from("add")) {
                                assert!(false, "Ident does not match. got={}", ex.exprKind);
                            }

                            assert!(a.len() == 3, "Wrong length of arguments. got={}", a.len());

                            test_literal_expression(&a[0], &1);
                            test_infix_expression(&a[1], &2, String::from("*"), &3);
                            test_infix_expression(&a[2], &4, String::from("+"), &5);
                        },
                        _ => {

                        }
                    }
                }
            },
            _ => {
                assert!(false, "stmt is not ExpressionStatement. got={}", stmt.stmtKind);
            }
        }
    }

    #[test]
    fn test_string_literal_expressions() {
        let input = String::from(r#""hello world";"#);

        let lexer = Lexer::new(input.clone());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap_or_default();
        check_parse_errors(&parser);

        let stmt = program.statements[0].clone();
        match stmt.stmtKind{
            StatementKind::ExpressionStatement(ref t, ref e) => {
                if let Some(ex) = e.clone() {
                    match ex.exprKind {
                        ExpressionKind::StringLiteral(ref t, ref s) => {
                            assert!(s == "hello world", "literal.Value not {}. got={}", "hello world", s);
                        },
                        _ => {
                            assert!(false, "exp not StringLiteral. got={}", ex.exprKind);
                        }
                    }
                }
            },
            _ => {
                
            }
        }
    }

    #[test]
    fn test_parsing_array_literals() {
        let input = String::from("[1, 2 * 2, 3 + 3]");

        let lexer = Lexer::new(input.clone());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap_or_default();
        check_parse_errors(&parser);

        let stmt = program.statements[0].clone();
        match stmt.stmtKind {
            StatementKind::ExpressionStatement(ref t, ref e) => {
                if let Some(ex) = e.clone() {
                    match ex.exprKind {
                        ExpressionKind::ArrayLiteral(ref t, ref el) => {
                            if el.len() != 3 {
                                assert!(false, "len(array.Elements) not 3. got={}", el.len());
                            }

                            test_integer_literal(&el[0], 1);
                            test_infix_expression(&el[1], &2, String::from("*"), &2);
                            test_infix_expression(&el[2], &3, String::from("+"), &3);
                        },
                        _ => {
                            assert!(false, "exp not ArrayLiteral. got={}", ex.exprKind);
                        }
                    }
                }
            },
            _ => {
                
            }
        }
    }
}
