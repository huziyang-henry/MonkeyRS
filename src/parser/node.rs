use std::fmt::Display;
use crate::object::Object;
use crate::parser::expression::Expression;
use crate::parser::program::Program;
use crate::parser::statement::Statement;

pub enum Node {
    ProgramNode(Program),
    StatementNode(Statement),
    ExpressionNode(Expression),
}

pub trait NodeOp: Display {
    fn token_literal(&self) -> String;
    fn eval(&self) -> Object;
}