use interpreter::ast::{ Statement, Node, NodeType };
use interpreter::token::Token;
use std::vec::Vec;
use std::fmt;

pub struct Program {
   pub statements: Vec<Statement>
}

impl Program {
    pub fn new() -> Program {
        Program {
            statements: Vec::new()
        }
    }
//     fn node_type(&self) -> NodeType {
//         return NodeType::Root;
//     }
//     fn token_literal(&self) -> Token {
//         if self.statements.len() > 0 {
//             let first = &self.statements[0];
//             return first.token_literal();
//         }
//         else {
//             return Token::Eof;
//         }
//     }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut result = String::new();

        let statements = self.statements.clone();
        for s in statements {
            let stmtStr = format!("{}", s.stmtKind);
            result.push_str(&stmtStr[..]);
        }

        write!(f, "{}", result)
    }
}

impl Default for Program {
    fn default() -> Program {
        Program {
            statements: Vec::new()
        }
    }
}
