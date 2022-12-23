use super::ObjectError;
use crate::object::{Array, Object};
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

pub fn len(arg_objs: Vec<Object>) -> Result<Object, ObjectError> {
    if arg_objs.len() != 1 {
        return Err(ObjectError::new(format!(
            "wrong number of arguments. got={}, want=1",
            arg_objs.len()
        )));
    }

    let arg = &arg_objs[0];
    match arg {
        Object::String(s) => Ok(Object::Integer(s.len() as i64)),
        Object::Array(arr) => Ok(Object::Integer(arr.elements.len() as i64)),
        _ => Err(ObjectError::new(format!(
            "argument to `len` not supported, got {}",
            arg
        ))),
    }
}

pub fn first(arg_objs: Vec<Object>) -> Result<Object, ObjectError> {
    if arg_objs.len() != 1 {
        return Err(ObjectError::new(format!(
            "wrong number of arguments. got={}, want=1",
            arg_objs.len()
        )));
    }

    let arg = &arg_objs[0];
    match arg {
        Object::Array(arr) => {
            if arr.elements.is_empty() {
                Err(ObjectError::new("ARRAY is empty".to_string()))
            } else {
                Ok(arr.elements[0].clone())
            }
        }
        _ => Err(ObjectError::new(format!(
            "argument to `first` must be ARRAY, got {}",
            arg
        ))),
    }
}

pub fn last(arg_objs: Vec<Object>) -> Result<Object, ObjectError> {
    if arg_objs.len() != 1 {
        return Err(ObjectError::new(format!(
            "wrong number of arguments. got={}, want=1",
            arg_objs.len()
        )));
    }

    let arg = &arg_objs[0];
    match arg {
        Object::Array(arr) => {
            let len = arr.elements.len();
            if len == 0 {
                Err(ObjectError::new("ARRAY is empty".to_string()))
            } else {
                Ok(arr.elements[len - 1].clone())
            }
        }
        _ => Err(ObjectError::new(format!(
            "argument to `first` must be ARRAY, got {}",
            arg
        ))),
    }
}

pub fn rest(arg_objs: Vec<Object>) -> Result<Object, ObjectError> {
    if arg_objs.len() != 1 {
        return Err(ObjectError::new(format!(
            "wrong number of arguments. got={}, want=1",
            arg_objs.len()
        )));
    }

    let arg = &arg_objs[0];
    match arg {
        Object::Array(arr) => {
            let len = arr.elements.len();
            if len == 0 {
                Err(ObjectError::new("ARRAY is empty".to_string()))
            } else {
                let mut new_arr = Vec::with_capacity(len - 1);
                for i in 1..len {
                    new_arr.push(arr.elements[i].clone());
                }

                Ok(Object::Array(Array { elements: new_arr }))
            }
        }
        _ => Err(ObjectError::new(format!(
            "argument to `first` must be ARRAY, got {}",
            arg
        ))),
    }
}

pub fn push(arg_objs: Vec<Object>) -> Result<Object, ObjectError> {
    if arg_objs.len() != 2 {
        return Err(ObjectError::new(format!(
            "wrong number of arguments. got={}, want=2",
            arg_objs.len()
        )));
    }

    let arg = &arg_objs[0];
    match arg {
        Object::Array(arr) => {
            let len = arr.elements.len();
            let mut new_arr = Vec::with_capacity(len + 1);
            for i in 0..len {
                new_arr.push(arr.elements[i].clone());
            }

            new_arr.push(arg_objs[1].clone());
            Ok(Object::Array(Array { elements: new_arr }))
        }
        _ => Err(ObjectError::new(format!(
            "argument to `first` must be ARRAY, got {}",
            arg
        ))),
    }
}
