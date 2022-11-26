use std::fmt::{Display, Formatter};
use crate::token::Token;
use crate::object::{Object};
use crate::evaluator::Evaluator;
use crate::parser::statement::{BlockStatement};

pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    BooleanLiteral(BooleanLiteral),
    FunctionLiteral(FunctionLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    IfExpression(IfExpression),
    CallExpression(CallExpression),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(s) => { s.fmt(f) }
            Expression::IntegerLiteral(s) => { s.fmt(f) }
            Expression::BooleanLiteral(s) => { s.fmt(f) }
            Expression::FunctionLiteral(s) => { s.fmt(f) }
            Expression::PrefixExpression(s) => { s.fmt(f) }
            Expression::InfixExpression(s) => { s.fmt(f) }
            Expression::IfExpression(s) => { s.fmt(f) }
            Expression::CallExpression(s) => { s.fmt(f) }
        }
    }
}

impl Evaluator for Expression {

    fn eval(&self) -> Object {
        match self {
            Expression::Identifier(e) => { e.eval() }
            Expression::IntegerLiteral(e) => { e.eval() }
            Expression::BooleanLiteral(e) => { e.eval() }
            Expression::FunctionLiteral(e) => { e.eval() }
            Expression::PrefixExpression(e) => { e.eval() }
            Expression::InfixExpression(e) => { e.eval() }
            Expression::IfExpression(e) => { e.eval() }
            Expression::CallExpression(e) => { e.eval() }
        }
    }
}

pub struct Identifier {
    pub token: Token,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token)
    }
}

impl Evaluator for Identifier {
    fn eval(&self) -> Object {
        todo!()
    }
}

pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token)
    }
}

impl Evaluator for IntegerLiteral {
    fn eval(&self) -> Object {
        Object::Integer(self.value)
    }
}

pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl Evaluator for BooleanLiteral {
    fn eval(&self) -> Object {
        Object::Boolean(self.value)
    }
}

impl Display for BooleanLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token)
    }
}

pub struct PrefixExpression {
    pub token: Token,
    pub right: Box<Expression>,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.token, self.right)
    }
}

impl Evaluator for PrefixExpression {
    fn eval(&self) -> Object {
        let right = self.right.eval();

        match &self.token {
            Token::BANG => {
                match right {
                    Object::Boolean(b) => { Object::Boolean(!b) }
                    Object::Null => { Object::Boolean(true) }
                    _ => { Object::Boolean(false) }
                }
            }
            Token::MINUS => {
                match right {
                    Object::Integer(i) => { Object::Integer(-i) }
                    _ => { Object::Null }
                }
            }
            _ => { Object::Null }
        }
    }
}

pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl Evaluator for InfixExpression {

    fn eval(&self) -> Object {
        let left = self.left.eval();
        let right = self.right.eval();

        match (left, right) {
            (Object::Integer(left_i), Object::Integer(right_i)) => {
                match self.token {
                    Token::PLUS => { Object::Integer(left_i + right_i) }
                    Token::MINUS => { Object::Integer(left_i - right_i) }
                    Token::ASTERISK => { Object::Integer(left_i * right_i) }
                    Token::SLASH => { Object::Integer(left_i / right_i) }
                    Token::LT => { Object::Boolean(left_i < right_i) }
                    Token::GT => { Object::Boolean(left_i > right_i) }
                    Token::EQ => { Object::Boolean(left_i == right_i) }
                    Token::NEQ => { Object::Boolean(left_i != right_i) }
                    _ => { Object::Null }
                }
            }
            (Object::Boolean(left_b), Object::Boolean(right_b)) => {
                match self.token {
                    Token::EQ => { Object::Boolean(left_b == right_b) }
                    Token::NEQ => { Object::Boolean(left_b != right_b) }
                    _ => { Object::Null }
                }
            }
            _ => { Object::Null }
        }
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.token, self.right)
    }
}

pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Evaluator for IfExpression {
    fn eval(&self) -> Object {
        let condition = self.condition.eval();
        let is_true = match condition {
            Object::Boolean(b) => { b }
            Object::Null => { false }
            _ => { true }
        };

        if is_true {
            self.consequence.eval()
        } else {
            match &self.alternative {
                None => { Object::Null }
                Some(e) => { e.eval() }
            }
        }
    }
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {} {}", self.condition, self.consequence)?;
        if let Some(alternative) = &self.alternative {
            write!(f, "else {}", alternative)?;
        }

        Ok(())
    }
}

pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Evaluator for FunctionLiteral {

    fn eval(&self) -> Object {
        todo!()
    }
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let params = self.parameters
            .iter()
            .map(|a| a.to_string())
            .reduce(|a, b| format!("{}, {}", a, b));

        match params {
            None => { write!(f, "{} () {}", self.token, self.body) }
            Some(p) => { write!(f, "{} ({}) {}", self.token, p, self.body) }
        }
    }
}

pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub args: Vec<Box<Expression>>,
}

impl Evaluator for CallExpression {

    fn eval(&self) -> Object {
        todo!()
    }
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let args_option = self.args.iter()
            .map(|a| a.to_string())
            .reduce(|a, b| format!("{}, {}", a, b));

        match args_option {
            None => { write!(f, "{}()", self.function) }
            Some(args) => { write!(f, "{}({})", self.function, args) }
        }
    }
}
