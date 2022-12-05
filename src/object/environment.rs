use super::ObjectError;
use crate::object::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Environment {
    map: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            map: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            map: HashMap::new(),
            outer: Some(Rc::clone(&outer)),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Object> {
        let value = self.map.get(name);
        match value {
            None => match &self.outer {
                None => None,
                Some(env) => unsafe { env.as_ptr().as_ref()?.get(name) },
            },
            _ => value,
        }
    }

    pub fn set(&mut self, name: &str, value: Object) -> Option<Object> {
        self.map.insert(name.to_string(), value)
    }
}

pub fn len(arg: &Object) -> Result<Object, ObjectError> {
    match arg {
        Object::String(s) => Ok(Object::Integer(s.len() as i64)),
        _ => Err(ObjectError::new(format!(
            "argument to `len` not supported, got {}",
            arg
        ))),
    }
}
