use interpreter::token::Token;
use std::ops::Index;
use interpreter::keywords::Keywords;

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
    keywords: Keywords
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input: input,
            position: 0,
            read_position: 0,
            ch: '\0',
            keywords: Keywords::new()
        }; 

        l.read_char();

        return l;
    }

    pub fn next_token(&mut self) -> Token {

        self.skip_whitespace();

        let tok = match self.ch {
            '=' => Token::Assign,
            ';' => Token::Semicolon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '\0' => Token::Eof,
            _ => {
                if Lexer::is_letter(self.ch) {
                    let ident_tok = self.read_identifier();
                    let tok = match ident_tok {
                        Token::Ident(ref s) => self.keywords.lookup_ident(s),
                        _ => ident_tok
                    };

                    return tok;
                }
                else if Lexer::is_digit(self.ch) {
                    return Token::Int(self.read_number());
                }
                else {
                    return Token::Illegal;
                }
            }
        };

        self.read_char();
       return tok; 
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        }
        else {
            self.ch = self.input.chars().nth(self.read_position).unwrap();
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> Token {
        let pos = self.position;

        let mut result = String::new();
        while Lexer::is_letter(self.ch) {
            result.push(self.ch);
            self.read_char();
        }

        return Token::Ident(result);
    }

    fn read_number(&mut self) -> i64 {
        let pos = self.position;

        let mut r = String::new();
        while Lexer::is_digit(self.ch) {
            r.push(self.ch);
            self.read_char();
        }

        return r.parse().unwrap_or(-1);
    }

    fn is_whitespace(&self, ch: char) -> bool {
       return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'; 
    }

    fn skip_whitespace(&mut self) {
        loop {
            if !self.is_whitespace(self.ch) {
                break
            } 

            self.read_char();
        }
    }

    fn is_letter(ch: char) -> bool {
        let result = match ch {
            'a'...'z' => true,
            'A'...'Z' => true,
            '_' => true,
            _ => false
        };

        return result;
    }

    fn is_digit(ch: char) -> bool {
        return match ch {
            '0'...'9' => true,
            _ => false
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct token_test_case {
        expected_token: Token,
        expected_literal: String
    }

    #[test]
    fn test_next_token() {
      let input = "=+(){},;";

        let test_cases = vec![token_test_case{expected_token: Token::Assign, expected_literal: String::from("=")},
                             token_test_case{expected_token: Token::Plus, expected_literal: String::from("+")},
                             token_test_case{expected_token: Token::LParen, expected_literal: String::from("(")},
                             token_test_case{expected_token: Token::RParen, expected_literal: String::from(")")},
                            token_test_case{expected_token: Token::LBrace, expected_literal: String::from("{")},
                            token_test_case{expected_token: Token::RBrace, expected_literal: String::from("}")},
                            token_test_case{expected_token: Token::Comma, expected_literal: String::from(",")},
                            token_test_case{expected_token: Token::Semicolon, expected_literal: String::from(";")},
                            token_test_case{expected_token: Token::Eof, expected_literal: String::from("")}];

        let mut lexer = Lexer::new(String::from(input));

        for t in test_cases {
            let tok = lexer.next_token();

            // println!("{}", tok);
            assert!(tok == t.expected_token);
        }
    }

    #[test]
    fn test_monkey_function_lexer() {
        let input = r#"let five = 5;
                  let ten = 10;
                  let add = fn(x, y) {
                     x + y;
                  };

                  let result = add(five, ten);"#;

        let tests = vec![
            token_test_case{expected_token: Token::Let, expected_literal: String::from("let")},
            token_test_case{expected_token: Token::Ident(String::from("five")), expected_literal: String::from("five")},
            token_test_case{expected_token: Token::Assign, expected_literal: String::from("=")},
            token_test_case{expected_token: Token::Int(5), expected_literal: String::from("5")},
            token_test_case{expected_token: Token::Semicolon, expected_literal: String::from(";")},
            token_test_case{expected_token: Token::Let, expected_literal: String::from("let")},
            token_test_case{expected_token: Token::Ident(String::from("ten")), expected_literal: String::from("ten")},
            token_test_case{expected_token: Token::Assign, expected_literal: String::from("=")},
            token_test_case{expected_token: Token::Int(10), expected_literal: String::from("10")},
            token_test_case{expected_token: Token::Semicolon, expected_literal: String::from(";")},
            token_test_case{expected_token: Token::Let, expected_literal: String::from("let")},
            token_test_case{expected_token: Token::Ident(String::from("add")), expected_literal: String::from("add")},
            token_test_case{expected_token: Token::Assign, expected_literal: String::from("=")},
            token_test_case{expected_token: Token::Function, expected_literal: String::from("fn")},
            token_test_case{expected_token: Token::LParen, expected_literal: String::from("(")},
            token_test_case{expected_token: Token::Ident(String::from("x")), expected_literal: String::from("x")},
            token_test_case{expected_token: Token::Comma, expected_literal: String::from(",")},
            token_test_case{expected_token: Token::Ident(String::from("y")), expected_literal: String::from("y")},
            token_test_case{expected_token: Token::RParen, expected_literal: String::from(")")},
            token_test_case{expected_token: Token::LBrace, expected_literal: String::from("{")},
            token_test_case{expected_token: Token::Ident(String::from("x")), expected_literal: String::from("x")},
            token_test_case{expected_token: Token::Plus, expected_literal: String::from("+")},
            token_test_case{expected_token: Token::Ident(String::from("y")), expected_literal: String::from("y")},
            token_test_case{expected_token: Token::Semicolon, expected_literal: String::from(";")},
            token_test_case{expected_token: Token::RBrace, expected_literal: String::from("}")},
            token_test_case{expected_token: Token::Semicolon, expected_literal: String::from(";")},
            token_test_case{expected_token: Token::Let, expected_literal: String::from("let")},
            token_test_case{expected_token: Token::Ident(String::from("result")), expected_literal: String::from("result")},
            token_test_case{expected_token: Token::Assign, expected_literal: String::from("=")},
            token_test_case{expected_token: Token::Ident(String::from("add")), expected_literal: String::from("add")},
            token_test_case{expected_token: Token::LParen, expected_literal: String::from("(")},
            token_test_case{expected_token: Token::Ident(String::from("five")), expected_literal: String::from("five")},
            token_test_case{expected_token: Token::Comma, expected_literal: String::from(",")},
            token_test_case{expected_token: Token::Ident(String::from("ten")), expected_literal: String::from("ten")},
            token_test_case{expected_token: Token::RParen, expected_literal: String::from(")")},
            token_test_case{expected_token: Token::Semicolon, expected_literal: String::from(";")},
            token_test_case{expected_token: Token::Eof, expected_literal: String::from("")},
        ];

        let mut lexer = Lexer::new(String::from(input));

        for t in tests {
            let tok = lexer.next_token();

            // println!("{}", tok);
            // println!("Expected: {}", t.expected_token);
            assert!(tok == t.expected_token);
        }
    }
}
