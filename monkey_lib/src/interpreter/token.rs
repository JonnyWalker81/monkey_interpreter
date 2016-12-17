/*
 * Copyright © 2002-2016 Bluebeam Software, Inc. All Rights Reserved.
 * Creator: Jonathan Rothberg
 */
use std::fmt;

#[derive(PartialEq)]
pub enum Token {
    Illegal,
    Eof,
    Ident(String),
    Int(i64),
    Assign,
    Plus,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Let
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let printable = match *self {
            Token::Illegal => "ILLEGAL",
            Token::Eof => "EOF",
            Token::Ident(ref id) => "IDENT",
            Token::Int(ref i) => "INT",
            Token::Assign => "=",
            Token::Plus => "+",
            Token::Comma => ",",
            Token::Semicolon => ";",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::LBrace => "{",
            Token::RBrace => "}",
            Token::Function => "FUNCTION",
            Token::Let => "LET"
        };
        write!(f, "{}", printable)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn TestNextToken() {
      let input = "=+(){},;";
    }
}
