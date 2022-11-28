use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use crate::token::Token;
use crate::object::{Function, Object, ObjectError};
use crate::evaluator::Evaluator;
use crate::object::environment::Environment;
use crate::parser::statement::{BlockStatement};

#[derive(PartialEq, Debug, Clone)]
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
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, ObjectError> {
        match self {
            Expression::Identifier(e) => { e.eval(env) }
            Expression::IntegerLiteral(e) => { e.eval(env) }
            Expression::BooleanLiteral(e) => { e.eval(env) }
            Expression::FunctionLiteral(e) => { e.eval(env) }
            Expression::PrefixExpression(e) => { e.eval(env) }
            Expression::InfixExpression(e) => { e.eval(env) }
            Expression::IfExpression(e) => { e.eval(env) }
            Expression::CallExpression(e) => { e.eval(env) }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub token: Token,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token)
    }
}

impl Evaluator for Identifier {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, ObjectError> {
        let env_borrow = env.borrow();
        let value = env_borrow.get(self.token.literal());
        match value {
            None => { Err(ObjectError::new(format!("identifier not found: {}", self.token))) }
            Some(v) => { Ok(v.clone()) }
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
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
    fn eval(&self, _env: Rc<RefCell<Environment>>) -> Result<Object, ObjectError> {
        Ok(Object::Integer(self.value))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl Evaluator for BooleanLiteral {
    fn eval(&self, _env: Rc<RefCell<Environment>>) -> Result<Object, ObjectError> {
        Ok(Object::Boolean(self.value))
    }
}

impl Display for BooleanLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token)
    }
}

#[derive(PartialEq, Debug, Clone)]
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
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, ObjectError> {
        let right = self.right.eval(env)?;

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

#[derive(PartialEq, Debug, Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl Evaluator for InfixExpression {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, ObjectError> {
        let left = self.left.eval(Rc::clone(&env))?;
        let right = self.right.eval(Rc::clone(&env))?;

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

#[derive(PartialEq, Debug, Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Evaluator for IfExpression {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, ObjectError> {
        let condition = self.condition.eval(Rc::clone(&env))?;
        let is_true = match condition {
            Object::Boolean(b) => { b }
            Object::Null => { false }
            _ => { true }
        };

        if is_true {
            self.consequence.eval(Rc::clone(&env))
        } else {
            let result = match &self.alternative {
                None => { Object::Null }
                Some(e) => { e.eval(Rc::clone(&env))? }
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

#[derive(PartialEq, Debug, Clone)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Evaluator for FunctionLiteral {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, ObjectError> {
        Ok(Object::Function(Rc::new(Function {
            parameters: self.parameters.clone(),
            body: self.body.clone(),
            env: Rc::clone(&env),
        })))
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

#[derive(PartialEq, Debug, Clone)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub args: Vec<Box<Expression>>,
}

impl Evaluator for CallExpression {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, ObjectError> {
        let function_obj = self.function.eval(Rc::clone(&env))?;
        let mut arg_objs = vec![];
        for arg in &self.args {
            let arg_result = arg.eval(Rc::clone(&env))?;
            arg_objs.push(arg_result);
        }

        match function_obj {
            Object::Function(func) => {
                let mut enclosed_env = Environment::new_enclosed(Rc::clone(&func.env));
                for (i, para) in (&func.parameters).iter().enumerate() {
                    enclosed_env.set(para.token.literal(), arg_objs[i].clone());
                }
                
                let result = func.body.eval(Rc::new(RefCell::new(enclosed_env)))?;
                Ok(match result {
                    Object::Return(r) => { *r }
                    _ => { result }
                })
            }
            _ => { Err(ObjectError::new(format!("not a function: {}", function_obj))) }
        }
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
