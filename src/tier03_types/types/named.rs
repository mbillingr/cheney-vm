use crate::str::Str;
use crate::tier03_types::{Env, Type};
use std::any::Any;
use std::rc::Rc;

/// The Empty type, which can't have a value
#[derive(Debug)]
pub struct Named(Str);

impl Named {
    pub fn new(name: impl Into<Str>) -> Rc<Named> {
        Rc::new(Named(name.into()))
    }

    fn resolve<'a>(&'a self, env: &'a Env) -> &'a dyn Type {
        &**env.lookup(&self.0).expect("unable to resolve type")
    }
}

impl Type for Named {
    fn is_value(&self, env: &Env) -> bool {
        self.resolve(env).is_value(env)
    }

    fn is_pointer(&self, env: &Env) -> bool {
        self.resolve(env).is_pointer(env)
    }

    fn is_equal(&self, other: &dyn Type, env: &Env) -> bool {
        match other.as_any(env).downcast_ref::<Self>() {
            None => self.resolve(env).is_equal(other, env),
            Some(Named(name)) => return &self.0 == name,
        }
    }

    fn as_any<'a>(&'a self, env: &'a Env) -> &'a dyn Any {
        self.resolve(env).as_any(env)
    }
}
