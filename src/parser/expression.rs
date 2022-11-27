use std::fmt::{Display, Formatter};
use crate::token::Token;
use crate::object::{Object, ObjectError};
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
    fn eval(&self) -> Result<Object, ObjectError> {
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
    fn eval(&self) -> Result<Object, ObjectError> {
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
    fn eval(&self) -> Result<Object, ObjectError> {
        Ok(Object::Integer(self.value))
    }
}

pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl Evaluator for BooleanLiteral {
    fn eval(&self) -> Result<Object, ObjectError> {
        Ok(Object::Boolean(self.value))
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
    fn eval(&self) -> Result<Object, ObjectError> {
        let right = self.right.eval()?;

        match &self.token {
            Token::BANG => {
                Ok(match right {
                    Object::Boolean(b) => { Object::Boolean(!b) }
                    Object::Null => { Object::Boolean(true) }
                    _ => { Object::Boolean(false) }
                })
            }
            Token::MINUS => {
                match right {
                    Object::Integer(i) => { Ok(Object::Integer(-i)) }
                    _ => { Err(ObjectError::new(format!("unknown operator: -{}", right))) }
                }
            }
            _ => { Err(ObjectError::new(format!("unknown operator: {}{}", self.token, right))) }
        }
    }
}

pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl Evaluator for InfixExpression {
    fn eval(&self) -> Result<Object, ObjectError> {
        let left = self.left.eval()?;
        let right = self.right.eval()?;

        match (&left, &right) {
            (Object::Integer(left_i), Object::Integer(right_i)) => {
                match self.token {
                    Token::PLUS => { Ok(Object::Integer(left_i + right_i)) }
                    Token::MINUS => { Ok(Object::Integer(left_i - right_i)) }
                    Token::ASTERISK => { Ok(Object::Integer(left_i * right_i)) }
                    Token::SLASH => { Ok(Object::Integer(left_i / right_i)) }
                    Token::LT => { Ok(Object::Boolean(left_i < right_i)) }
                    Token::GT => { Ok(Object::Boolean(left_i > right_i)) }
                    Token::EQ => { Ok(Object::Boolean(left_i == right_i)) }
                    Token::NEQ => { Ok(Object::Boolean(left_i != right_i)) }
                    _ => { Err(ObjectError::new(format!("unknown operator: {} {} {}", left, self.token, right))) }
                }
            }
            (Object::Boolean(left_b), Object::Boolean(right_b)) => {
                match self.token {
                    Token::EQ => { Ok(Object::Boolean(left_b == right_b)) }
                    Token::NEQ => { Ok(Object::Boolean(left_b != right_b)) }
                    _ => { Err(ObjectError::new(format!("unknown operator: {} {} {}", left, self.token, right))) }
                }
            }
            _ => { Err(ObjectError::new(format!("type mismatch: {} {} {}", left, self.token, right))) }
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
    fn eval(&self) -> Result<Object, ObjectError> {
        let condition = self.condition.eval()?;
        let is_true = match condition {
            Object::Boolean(b) => { b }
            Object::Null => { false }
            _ => { true }
        };

        if is_true {
            self.consequence.eval()
        } else {
            let result = match &self.alternative {
                None => { Object::Null }
                Some(e) => { e.eval()? }
            };

            Ok(result)
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
    fn eval(&self) -> Result<Object, ObjectError> {
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
    fn eval(&self) -> Result<Object, ObjectError> {
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
