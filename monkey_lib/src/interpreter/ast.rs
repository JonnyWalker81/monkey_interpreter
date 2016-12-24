use interpreter::token::Token;

#[derive(PartialEq)]
pub enum NodeType {
    Root,
    LetStatement
}

pub trait Node {
    fn node_type(&self) -> NodeType;
    fn token_literal(&self) -> Token;
}

pub trait Statement: Node {
    fn statement_node(&self);
}

pub trait Expression: Node {
    fn expression_node(&self);
}

pub struct Identifier {
    token: Token,
    value: String,
}

pub struct LetStatement {
    token: Token,
    name: Identifier,
    value: Expression,
}

impl Node for Identifier {
    fn node_type(&self) -> NodeType {
        return NodeType::LetStatement;
    }

    fn token_literal(&self) -> Token {
        return self.token.clone();
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {}
}
