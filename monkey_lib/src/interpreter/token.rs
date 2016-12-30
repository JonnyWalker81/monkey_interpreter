/*
 * Copyright © 2002-2016 Bluebeam Software, Inc. All Rights Reserved.
 * Creator: Jonathan Rothberg
 */
use std::fmt;

#[derive(PartialEq, Eq, Clone)]
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
            Token::Function => String::from("FUNCTION"),
            Token::Let => String::from("LET"),
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

#[cfg(test)]
mod tests {
    #[test]
    fn TestNextToken() {
      let input = "=+(){},;";
    }
}
