/*
 * Copyright © 2002-2016 Bluebeam Software, Inc. All Rights Reserved.
 * Creator: Jonathan Rothberg
 */
use std::fmt;

#[derive(PartialEq, Eq, Clone, Hash)]
pub enum Token {
    Illegal,
    Eof,
    Ident(String),
    Int(i64),
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    EqualEqual,
    NotEqual
}


impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let printable = match *self {
            Token::Illegal => String::from("ILLEGAL"),
            Token::Eof => String::from("EOF"),
            Token::Ident(ref id) => format!("IDENT({})", id),
            Token::Int(ref i) => format!("INT({})", i),
            Token::Assign => String::from("="),
            Token::Plus => String::from("+"),
            Token::Minus => String::from("-"),
            Token::Bang => String::from("!"),
            Token::Asterisk => String::from("*"),
            Token::Slash => String::from("/"),
            Token::Lt => String::from("<"),
            Token::Gt => String::from(">"),
            Token::Comma => String::from(")"),
            Token::Semicolon => String::from(";"),
            Token::LParen => String::from("("),
            Token::RParen => String::from(")"),
            Token::LBrace => String::from("{"),
            Token::RBrace => String::from("}"),
            Token::Function => String::from("fn"),
            Token::Let => String::from("let"),
            Token::True => String::from("true"),
            Token::False => String::from("false"),
            Token::If => String::from("if"),
            Token::Else => String::from("else"),
            Token::Return => String::from("return"),
            Token::EqualEqual => String::from("=="),
            Token::NotEqual => String::from("!=")
        };
        write!(f, "{}", printable)
    }
}

impl Token {
    pub fn id(&self) -> usize {
        match *self {
            Token::Illegal => 0,
            Token::Eof => 1,
            Token::Ident(..) => 2,
            Token::Int(..) => 3,
            Token::Assign => 4,
            Token::Plus => 5,
            Token::Minus => 6,
            Token::Bang => 7,
            Token::Asterisk => 8,
            Token::Slash => 9,
            Token::Lt => 10,
            Token::Gt => 11,
            Token::Comma => 12,
            Token::Semicolon => 13,
            Token::LParen => 14,
            Token::RParen => 15,
            Token::LBrace => 16,
            Token::RBrace => 17,
            Token::Function => 18,
            Token::Let => 19,
            Token::True => 20,
            Token::False => 21,
            Token::If => 22,
            Token::Else => 23,
            Token::Return => 24,
            Token::EqualEqual => 25,
            Token::NotEqual => 26
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn TestNextToken() {
      let input = "=+(){},;";
    }
}
