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

#[derive(PartialEq, Eq, Clone)]
pub struct Statement{
    pub stmtKind: StatementKind
}

#[derive(PartialEq, Eq, Clone)]
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
                    Token::Ident(ref id) => {
                        println!("Ident => {}", id);
                        id.clone()
                    },
                    _ => {
                        println!("Not Token::Ident...");
                     String::from("")   
                    }
                };
                let ex = e.clone();
                let expr = match ex {
                    Some(ref exp) => {
                        exp.clone()
                    },
                    None => {Expression::default()}
                };
                format!("{} {} = {};", t, ident, expr.exprKind)
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

#[derive(PartialEq, Eq, Clone)]
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

#[derive(PartialEq, Eq, Clone)]
pub enum ExpressionKind {
    Empty,
    Ident(Token, String),
    IntegerLiteral(Token, i64),
    PrefixExpression(Token, String, Arc<Expression>),
    InfixExpression(Token, Arc<Expression>, String, Arc<Expression>),
    Boolean(Token, bool),
    If(Token, Arc<Expression>, BlockStatement, Option<BlockStatement>),
    FunctionLiteral(Token, Vec<Identifier>, BlockStatement),
    Call(Token, Arc<Expression>, Vec<Expression>),
    StringLiteral(Token, String),
    ArrayLiteral(Token, Vec<Expression>)
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
            },
            ExpressionKind::FunctionLiteral(ref t, ref params, ref b) => {
                let mut p = Vec::new();

                for par in params {
                    p.push(format!("{}", par));
                }

                let mut result = String::new();
                let fl = format!("{}", t);
                result.push_str(&fl[..]);
                result.push_str("(");
                result.push_str(p.join(", ").as_str());
                result.push_str(")");
                let bs = format!("{}", b);
                result.push_str(bs.as_str());
                result
            },
            ExpressionKind::Call(ref t, ref f, ref a) => {
                let mut args = Vec::new();

                for arg in a {
                    args.push(format!("{}", arg.exprKind));
                }

                let mut result = String::new();
                let func = format!("{}", f.exprKind);
                result.push_str(&func[..]);
                result.push_str("(");
                result.push_str(args.join(", ").as_str());
                result.push_str(")");

                result
            },
            ExpressionKind::StringLiteral(ref t, ref v) => {
                format!("{}", v)
            },
            ExpressionKind::ArrayLiteral(ref t, ref el) => {
                let mut elements = Vec::new();

                for e in el {
                    elements.push(format!("{}", e.exprKind));
                }

                let mut result = String::new();
                result.push_str("[");
                result.push_str(elements.join(", ").as_str());
                result.push_str("]");

                result
            }
        };

        write!(f, "{}", printable)
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Default for Identifier {
    fn default() -> Identifier {
        Identifier {
            token: Token::Illegal,
            value: String::new()
        }
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(PartialEq, Eq, Clone)]
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
