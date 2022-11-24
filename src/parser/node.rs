use std::fmt::Display;
use downcast_rs::Downcast;
use crate::parser::expression::Expression;
use crate::parser::statement::Statement;

pub enum NodeEnum {
    StatementNode(Statement),
    ExpressionNode(Expression),
}

pub trait Node: Display + Downcast {
    fn token_literal(&self) -> String;
}