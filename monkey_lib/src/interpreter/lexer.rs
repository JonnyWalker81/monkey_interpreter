use interpreter::token::Token;
use std::ops::Index;

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input: input,
            position: 0,
            read_position: 0,
            ch: '\0'
        }; 

        l.read_char();

        return l;
    }

    pub fn next_token(&mut self) -> Token {

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
               _ => Token::Illegal
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

        let testCases = vec![token_test_case{expected_token: Token::Assign, expected_literal: String::from("=")},
                             token_test_case{expected_token: Token::Plus, expected_literal: String::from("+")},
                             token_test_case{expected_token: Token::LParen, expected_literal: String::from("(")},
                             token_test_case{expected_token: Token::RParen, expected_literal: String::from(")")},
                            token_test_case{expected_token: Token::LBrace, expected_literal: String::from("{")},
                            token_test_case{expected_token: Token::RBrace, expected_literal: String::from("}")},
                            token_test_case{expected_token: Token::Comma, expected_literal: String::from(",")},
                            token_test_case{expected_token: Token::Semicolon, expected_literal: String::from(";")},
                            token_test_case{expected_token: Token::Eof, expected_literal: String::from("")}];

        let mut lexer = Lexer::new(String::from(input));

        for t in testCases {
            let tok = lexer.next_token();

            println!("{}", tok);
            assert!(tok == t.expected_token);
        }
    }
}
