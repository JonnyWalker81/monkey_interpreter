use interpreter::token::Token;
use std::fmt;

#[derive(PartialEq)]
pub enum NodeType {
    Root,
    Stmt(Statement),
    Expr(Expression)
}

pub struct Node {
    nodeKind: NodeType
}

#[derive(PartialEq, Clone)]
pub struct Statement{
    pub stmtKind: StatementKind
}

#[derive(PartialEq, Clone)]
pub enum StatementKind {
    LetStatement(Token, Identifier, Option<Expression>),
    FnStatement
}

impl Statement {
    pub fn new(kind: StatementKind) -> Statement {
        Statement {
            stmtKind: kind
        }
    }
}

impl fmt::Display for StatementKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let printable = match *self {
            StatementKind::LetStatement(_, _, _) => String::from("Let"),
            StatementKind::FnStatement => String::from("fn")
        };

        write!(f, "{}", printable)
    }
}

#[derive(PartialEq, Clone)]
pub struct Expression {
    pub exprKind: ExpressionKind
}

#[derive(PartialEq, Clone)]
pub enum ExpressionKind {
    
}

#[derive(PartialEq, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

pub struct LetStatement {
    token: Token,
    name: Identifier,
    value: Option<Expression>,
}


// impl Node for Identifier {
//     fn token_literal(&self) -> Token {
//         return self.token.clone();
//     }
// }

// impl Expression for Identifier {
//     fn expression_node(&self) {}
// }
