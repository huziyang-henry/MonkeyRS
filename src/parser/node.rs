use crate::parser::expression::Expression;
use crate::parser::program::Program;
use crate::parser::statement::Statement;

pub enum Node {
    ProgramNode(Program),
    StatementNode(Statement),
    ExpressionNode(Expression),
}
