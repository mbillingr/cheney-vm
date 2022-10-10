use crate::str::Str;
use crate::tier03_types::{Env, Type};
use std::any::Any;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct Builtin(pub FunctionSignature);

#[derive(Debug)]
pub struct Function(pub FunctionSignature);

#[derive(Debug)]
pub struct Closure(pub FunctionSignature, pub HashMap<Str, Rc<dyn Type>>);

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub ptypes: Vec<Rc<dyn Type>>,
    pub returns: Rc<dyn Type>,
}

impl FunctionSignature {
    pub fn equal(&self, other: &Self, env: &Env) -> bool {
        self.ptypes.len() == other.ptypes.len()
            && self.returns.is_equal(&*other.returns, env)
            && self
                .ptypes
                .iter()
                .zip(&other.ptypes)
                .all(|(a, b)| a.is_equal(&**b, env))
    }
}

impl Builtin {
    pub fn new(ptypes: Vec<Rc<dyn Type>>, returns: Rc<dyn Type>) -> Rc<Self> {
        Rc::new(Builtin(FunctionSignature { ptypes, returns }))
    }
}

impl Type for Builtin {
    fn is_value(&self, _env: &Env) -> bool {
        false
    }

    fn is_pointer(&self, _env: &Env) -> bool {
        false
    }

    fn is_equal(&self, other: &dyn Type, env: &Env) -> bool {
        match other.as_any().downcast_ref::<Self>() {
            None => false,
            Some(Builtin(sig)) => self.0.equal(sig, env),
        }
    }

    fn resolve<'a>(&'a self, _env: &'a Env) -> &'a dyn Type {
        self
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn callable_signature(&self) -> Option<(&[Rc<dyn Type>], &dyn Type)> {
        let FunctionSignature { ptypes, returns } = &self.0;
        Some((ptypes, &**returns))
    }
}

impl Function {
    pub fn new(ptypes: Vec<Rc<dyn Type>>, returns: Rc<dyn Type>) -> Rc<Self> {
        Rc::new(Function(FunctionSignature { ptypes, returns }))
    }
}

impl Type for Function {
    fn is_value(&self, _env: &Env) -> bool {
        true
    }

    fn is_pointer(&self, _env: &Env) -> bool {
        false
    }

    fn is_equal(&self, other: &dyn Type, env: &Env) -> bool {
        match other.as_any().downcast_ref::<Self>() {
            None => false,
            Some(Function(sig)) => self.0.equal(sig, env),
        }
    }

    fn resolve<'a>(&'a self, _env: &'a Env) -> &'a dyn Type {
        self
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn callable_signature(&self) -> Option<(&[Rc<dyn Type>], &dyn Type)> {
        let FunctionSignature { ptypes, returns } = &self.0;
        Some((ptypes, &**returns))
    }
}

impl Closure {
    pub fn new(
        ptypes: Vec<Rc<dyn Type>>,
        returns: Rc<dyn Type>,
        cls: HashMap<Str, Rc<dyn Type>>,
    ) -> Rc<Self> {
        Rc::new(Closure(FunctionSignature { ptypes, returns }, cls))
    }
}

impl Type for Closure {
    fn is_value(&self, _env: &Env) -> bool {
        false
    }

    fn is_pointer(&self, _env: &Env) -> bool {
        true
    }

    fn is_equal(&self, other: &dyn Type, env: &Env) -> bool {
        match other.as_any().downcast_ref::<Self>() {
            None => false,
            Some(Closure(sig, cls)) => self.0.equal(sig, env) && closure_equal(&self.1, cls, env),
        }
    }

    fn resolve<'a>(&'a self, _env: &'a Env) -> &'a dyn Type {
        self
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn callable_signature(&self) -> Option<(&[Rc<dyn Type>], &dyn Type)> {
        let FunctionSignature { ptypes, returns } = &self.0;
        Some((ptypes, &**returns))
    }
}

fn closure_equal(
    x: &HashMap<Str, Rc<dyn Type>>,
    y: &HashMap<Str, Rc<dyn Type>>,
    env: &Env,
) -> bool {
    if x.len() != y.len() {
        return false;
    }
    for (a, ta) in x {
        match y.get(a) {
            None => return false,
            Some(tb) => {
                if !ta.is_equal(&**tb, env) {
                    return false;
                }
            }
        }
    }
    true
}
