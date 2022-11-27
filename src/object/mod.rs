use std::fmt::{Display, Formatter, write};

#[derive(PartialEq, Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Box<Object>),
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            Object::Integer(i) => { write!(f, "Integer({})", i) }
            Object::Boolean(b) => { write!(f, "Boolean({})", b) }
            Object::Null => { write!(f, "Null") }
            Object::Return(r) => { write!(f, "{}", r) }
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