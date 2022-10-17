use crate::str::Str;
use crate::tier03_typed::{gensym, Env, ExprEnum, Expression, Type};
use std::any::Any;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct Builtin(pub FunctionSignature);

#[derive(Debug)]
pub struct Function(pub FunctionSignature);

#[derive(Debug)]
pub struct Closure(pub FunctionSignature, pub HashMap<Str, Rc<dyn Type>>);

pub fn get_fnsignature(t: &dyn Type) -> Option<&FunctionSignature> {
    let tany = t.as_any();

    if let Some(Builtin(sig)) = tany.downcast_ref() {
        return Some(sig);
    }

    if let Some(Function(sig)) = tany.downcast_ref() {
        return Some(sig);
    }

    if let Some(Closure(sig, _)) = tany.downcast_ref() {
        return Some(sig);
    }

    None
}

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
        match other.as_any().downcast_ref() {
            None => false,
            Some(Self(sig)) => self.0.equal(sig, env),
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
        match other.as_any().downcast_ref() {
            None => false,
            Some(Self(sig)) => self.0.equal(sig, env),
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

    fn cast(
        &self,
        t: &dyn Type,
        env: &Env,
    ) -> Option<Box<dyn FnOnce(Rc<dyn Expression>) -> Rc<dyn Expression>>> {
        if let Some(Closure(sig, _)) = t.as_any().downcast_ref() {
            if !self.0.equal(sig, env) {
                return None;
            }

            // casting to (an empty) closure with the following transformation:
            //   callable  -->  (lambda (p ...) (callable p ...))
            let params: Vec<_> = sig.ptypes.iter().map(|_| gensym("x")).collect();
            let refs: Vec<_> = params
                .iter()
                .cloned()
                .map(|x| -> Rc<dyn Expression> { Rc::new(ExprEnum::Ref(x)) })
                .collect();
            let sig = sig.clone();

            return Some(Box::new(move |expr| {
                Rc::new(ExprEnum::Lambda(
                    params,
                    sig.ptypes.clone(),
                    Rc::new(ExprEnum::Call(expr, refs)),
                ))
            }));
        }

        None
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

    pub fn opaque(ptypes: Vec<Rc<dyn Type>>, returns: Rc<dyn Type>) -> Rc<Self> {
        Self::new(ptypes, returns, HashMap::new())
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
        match other.as_any().downcast_ref() {
            None => false,
            Some(Self(sig, _)) => self.0.equal(sig, env), // not sure if the closed environment should contribute to equality
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
