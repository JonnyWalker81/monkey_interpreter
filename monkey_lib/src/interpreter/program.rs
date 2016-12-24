use interpreter::ast::{ Statement, Node, NodeType };
use interpreter::token::Token;
use std::vec::Vec;

pub struct Program {
   pub statements: Vec<Box<Statement>>
}

impl Node for Program {
    fn node_type(&self) -> NodeType {
        return NodeType::Root;
    }
    fn token_literal(&self) -> Token {
        if self.statements.len() > 0 {
            let first = &self.statements[0];
            return first.token_literal();
        }
        else {
            return Token::Eof;
        }
    }
}
