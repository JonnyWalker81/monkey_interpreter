/*
 * Copyright © 2002-2016 Bluebeam Software, Inc. All Rights Reserved.
 * Creator: Jonathan Rothberg
 */
use std::fmt;

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub enum NumberType {
    Integer,
    Float
}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub enum Token {
    Illegal,
    Eof,
    Ident(String),
    // Int(i64),
    // Float(String),
    Number(NumberType, String),
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
    NotEqual,
    StringToken(String),
    LBracket,
    RBracket,
    Colon,
    While,
    For
}


impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let printable = match *self {
            Token::Illegal => String::from("ILLEGAL"),
            Token::Eof => String::from("EOF"),
            Token::Ident(ref id) => format!("IDENT({})", id),
            // Token::Int(ref i) => format!("INT({})", i),
            // Token::Float(ref s) => format!("FLOAT({})", s),
            Token::Number(ref nt, ref s) => format!("NUMBER({})", s),
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
            Token::NotEqual => String::from("!="),
            Token::StringToken(ref s) => format!("{}", s),
            Token::LBracket => String::from("["),
            Token::RBracket => String::from("]"),
            Token::Colon => String::from(":"),
            Token::While => "while".into(),
            Token::For => "for".into()
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
            // Token::Int(..) => 3,
            Token::Number(..) => 3,
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
            Token::NotEqual => 26,
            Token::StringToken(..) => 27,
            Token::LBracket => 28,
            Token::RBracket => 29,
            Token::Colon => 30,
            Token::While => 31,
            Token::For => 32,
            // Token::Float(..) => 33
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
