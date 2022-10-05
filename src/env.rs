use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Environment<T: Clone> {
    Empty,
    Entry(Rc<(String, T, Environment<T>)>),
}

impl<T: Clone> Environment<T> {
    pub fn assoc(&self, name: impl ToString, thing: T) -> Self {
        Environment::Entry(Rc::new((name.to_string(), thing, self.clone())))
    }

    pub fn lookup(&self, name: &str) -> Option<&T> {
        match self {
            Environment::Empty => None,
            Environment::Entry(e) => {
                if e.0 == name {
                    Some(&e.1)
                } else {
                    e.2.lookup(name)
                }
            }
        }
    }
}
