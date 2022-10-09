use crate::tier03_types::{Env, Type};
use std::any::Any;

/// The Empty type, which can't have a value
#[derive(Debug)]
pub struct Empty;

impl Type for Empty {
    fn is_value(&self, _env: &Env) -> bool {
        false
    }

    fn is_pointer(&self, _env: &Env) -> bool {
        false
    }

    fn is_equal(&self, other: &dyn Type, env: &Env) -> bool {
        match other.as_any(env).downcast_ref::<Self>() {
            None => false,
            Some(_) => true,
        }
    }

    fn as_any(&self, _env: &Env) -> &dyn Any {
        self
    }
}
