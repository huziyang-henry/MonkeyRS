use std::fmt::{Display, Formatter};
use crate::lexer::token::Token;
use crate::parser::expression::Expression;
use crate::parser::node::Node;

pub trait Statement: Node {
    fn statement_node(&self) -> Box<dyn Node>;
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl Statement for Identifier {
    fn statement_node(&self) -> Box<dyn Node> {
        todo!()
    }
}

impl Expression for Identifier {
    fn expression_node(&self) -> Box<dyn Node> {
        todo!()
    }
}

pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "IntegerLiteral {}", self.token.literal)
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl Statement for IntegerLiteral {
    fn statement_node(&self) -> Box<dyn Node> {
        todo!()
    }
}

impl Expression for IntegerLiteral {
    fn expression_node(&self) -> Box<dyn Node> {
        todo!()
    }
}

pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<dyn Expression>,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl Statement for PrefixExpression {
    fn statement_node(&self) -> Box<dyn Node> {
        todo!()
    }
}

impl Expression for PrefixExpression {
    fn expression_node(&self) -> Box<dyn Node> {
        todo!()
    }
}

pub struct InfixExpression {
    pub token: Token,
    pub left: Box<dyn Expression>,
    pub operator: String,
    pub right: Box<dyn Expression>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

impl Statement for InfixExpression {
    fn statement_node(&self) -> Box<dyn Node> {
        todo!()
    }
}

impl Expression for InfixExpression {
    fn expression_node(&self) -> Box<dyn Node> {
        todo!()
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Box<dyn Expression>>,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            None => { write!(f, "{} {};", self.token.literal, self.name) }
            Some(e) => { write!(f, "{} {} = {};", self.token.literal, self.name, e.to_string()) }
        }
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) -> Box<dyn Node> {
        todo!()
    }
}

pub struct ReturnStatement {
    pub token: Token,
    pub value: Option<Box<dyn Expression>>,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            None => { write!(f, "{};", self.token.literal) }
            Some(e) => { write!(f, "{} {};", self.token.literal, e.to_string()) }
        }
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) -> Box<dyn Node> {
        todo!()
    }
}


pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Box<dyn Expression>>,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.expression {
            None => { write!(f, "") }
            Some(e) => { write!(f, "{}", e.to_string()) }
        }
    }
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) -> Box<dyn Node> {
        todo!()
    }
}
