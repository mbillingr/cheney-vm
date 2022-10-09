use crate::tier03_types::{Env, Type};
use std::any::Any;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct RecordType {
    pub fields: Vec<Rc<dyn Type>>,
}

impl RecordType {
    pub fn new(fields: Vec<Rc<dyn Type>>) -> Rc<Self> {
        Rc::new(RecordType { fields })
    }

    pub fn equal(&self, other: &Self, env: &Env) -> bool {
        self.fields.len() == other.fields.len()
            && self
                .fields
                .iter()
                .zip(&other.fields)
                .all(|(a, b)| a.is_equal(&**b, env))
    }
}

impl Type for RecordType {
    fn is_value(&self, _env: &Env) -> bool {
        false
    }

    fn is_pointer(&self, _env: &Env) -> bool {
        true
    }

    fn is_equal(&self, other: &dyn Type, env: &Env) -> bool {
        match other.as_any().downcast_ref::<Self>() {
            None => false,
            Some(o) => self.equal(o, env),
        }
    }

    fn resolve<'a>(&'a self, _env: &'a Env) -> Option<&'a dyn Type> {
        Some(self)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
