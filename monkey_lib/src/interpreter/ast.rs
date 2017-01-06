use interpreter::token::Token;
use interpreter::program::Program;
use std::fmt;
use std::sync::Arc;

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
    ReturnStatement(Token, Option<Expression>),
    ExpressionStatement(Token, Option<Expression>),
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
            StatementKind::LetStatement(ref t, ref i, ref e) => {
                let ident = match i.token {
                    Token::Ident(ref id) => id.clone(),
                    _ => String::from("")
                };
                let ex = e.clone();
                let expr = match ex {
                    Some(ref exp) => {
                        match exp.exprKind {
                            ExpressionKind::Ident(_, ref ss) => {
                               ss.clone()
                            },
                            _ => String::from("")
                        }
                    },
                    None => {String::from("")}
                };
                format!("{} {} = {};", t, ident, expr)
                // String::from("Let")
            },
            StatementKind::ReturnStatement(ref t, _) => { format!("{};", t) },
            StatementKind::ExpressionStatement(_, ref e) =>{
                let oe = e.clone();
                let exp = oe.unwrap_or_default();
                format!("{}", exp.exprKind)
            },
            StatementKind::FnStatement => String::from("fn")
        };

        write!(f, "{}", printable)
    }
}

#[derive(PartialEq, Clone)]
pub struct Expression {
    pub exprKind: ExpressionKind
}

impl Default for Expression {
    fn default() -> Expression {
        Expression {
            exprKind: ExpressionKind::Empty
        }
    }
}

#[derive(PartialEq, Clone)]
pub enum ExpressionKind {
    Empty,
    Ident(Token, String),
    IntegerLiteral(Token, i64),
    PrefixExpression(Token, String, Arc<Expression>),
    InfixExpression(Token, Arc<Expression>, String, Arc<Expression>),
    Boolean(Token, bool),
    If(Token, Arc<Expression>, BlockStatement, Option<BlockStatement>)
}

impl fmt::Display for ExpressionKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let printable = match *self {
            ExpressionKind::Empty => format!("Empty"),
            ExpressionKind::Ident(ref t, ref v) => {
                format!("{}", *v)
            },
            ExpressionKind::IntegerLiteral(ref t, ref i) => {
                format!("{}", *i)
            },
            ExpressionKind::PrefixExpression(ref t, ref v, ref e) => {
                let ex = e.clone();
                format!("({}{})", *v, ex.exprKind)
            },
            ExpressionKind::InfixExpression(ref t, ref l, ref o, ref r) => {
               format!("({} {} {})", l.exprKind, *o, r.exprKind)
            },
            ExpressionKind::Boolean(ref t, ref b) => {
                format!("{}", *b)
            },
            ExpressionKind::If(ref t, ref c, ref con, ref alt) => {
                format!("if condition")
            }
        };

        write!(f, "{}", printable)
    }
}

#[derive(PartialEq, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(PartialEq, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut result = String::new();

        let stmts = self.statements.clone();
        for s in stmts {
            let stmt = format!("{}", s.stmtKind);
            result.push_str(&stmt[..]);
        }

        write!(f, "{}", result)
    }
}

// pub struct LetStatement {
//     token: Token,
//     name: Identifier,
//     value: Option<Expression>,
// }


// impl Node for Identifier {
//     fn token_literal(&self) -> Token {
//         return self.token.clone();
//     }
// }

// impl Expression for Identifier {
//     fn expression_node(&self) {}
// }


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![
                Statement {
                    stmtKind: StatementKind::LetStatement(Token::Let, Identifier { token: Token::Ident(String::from("myVar")), value: String::from("myVar")}, Some(Expression { exprKind: ExpressionKind::Ident(Token::Ident(String::from("anotherVar")), String::from("anotherVar"))}))
                }
            ]
        };

        println!("{}", program);
        let result = format!("{}", program);
        assert!(result == "let myVar = anotherVar;")
    }
}
