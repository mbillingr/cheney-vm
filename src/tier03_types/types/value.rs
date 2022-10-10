use crate::tier03_types::{Env, Type};
use std::any::Any;
use std::rc::Rc;

/// The Empty type, which can't have a value
#[derive(Debug)]
pub struct Value;

impl Value {
    pub fn new() -> Rc<Value> {
        Rc::new(Value)
    }
}

impl Type for Value {
    fn is_value(&self, _env: &Env) -> bool {
        true
    }

    fn is_pointer(&self, _env: &Env) -> bool {
        false
    }

    fn is_equal(&self, other: &dyn Type, _env: &Env) -> bool {
        match other.as_any().downcast_ref::<Self>() {
            None => false,
            Some(_) => true,
        }
    }

    fn resolve<'a>(&'a self, _env: &'a Env) -> &'a dyn Type {
        self
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
