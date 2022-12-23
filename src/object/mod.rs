use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::object::environment::Environment;
use crate::parser::expression::Identifier;
use crate::parser::statement::BlockStatement;

pub mod environment;

#[derive(PartialEq, Debug, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Null,
    Return(Box<Object>),
    Function(Rc<Function>),
    BuiltinFunction(BuiltinFunction),
    Array(Array),
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            Object::Integer(i) => {
                write!(f, "Integer({})", i)
            }
            Object::Boolean(b) => {
                write!(f, "Boolean({})", b)
            }
            Object::Null => {
                write!(f, "Null")
            }
            Object::Return(r) => {
                write!(f, "{}", r)
            }
            Object::Function(func) => {
                write!(f, "{}", func)
            }
            Object::String(s) => {
                write!(f, "{}", s)
            }
            Object::BuiltinFunction(_) => {
                write!(f, "builtin function")
            }
            Object::Array(arr) => {
                write!(f, "{}", arr)
            }
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct ObjectError {
    message: String,
}

impl Display for ObjectError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error: {}", self.message)
    }
}

impl ObjectError {
    pub fn new(message: String) -> Self {
        ObjectError { message }
    }
}

#[derive(Debug)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Rc<RefCell<Environment>>,
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let paras = self
            .parameters
            .iter()
            .map(|i| format!("{}", i))
            .reduce(|a, b| format!("{}, {}", a, b));

        match paras {
            None => {
                write!(f, "fn(){{\n{}\n}}", self.body)
            }
            Some(s) => {
                write!(f, "fn({}){{\n{}\n}}", s, self.body)
            }
        }
    }
}

impl Clone for Function {
    fn clone(&self) -> Self {
        Function {
            parameters: self.parameters.clone(),
            body: self.body.clone(),
            env: Rc::clone(&self.env),
        }
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.parameters == other.parameters && self.body == other.body
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum BuiltinFunction {
    Len,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Array {
    pub elements: Vec<Object>,
}

impl Display for Array {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let option_str = self
            .elements
            .iter()
            .map(|i| format!("{}", i))
            .reduce(|a, b| format!("{}, {}", a, b));

        match option_str {
            None => {
                write!(f, "[]")
            }
            Some(s) => {
                write!(f, "[{}]", s)
            }
        }
    }
}
