#[derive(PartialEq, Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Box<Object>),
}

impl Object {
    fn inspect(&self) -> String {
        match self {
            Object::Integer(i) => { format!("{}", i) }
            Object::Boolean(b) => { format!("{}", b) }
            Object::Null => { String::from("null") }
            Object::Return(r) => { r.inspect() }
            _ => { panic!() }
        }
    }
}