use std::rc::Rc;

#[derive(Debug)]
pub enum Environment<T> {
    Empty,
    Entry(Rc<(String, T, Environment<T>)>),
}

impl<T> Clone for Environment<T> {
    fn clone(&self) -> Self {
        match self {
            Environment::Empty => Environment::Empty,
            Environment::Entry(e) => Environment::Entry(e.clone()),
        }
    }
}

impl<T> Environment<T> {
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

impl<T: Clone> Environment<T> {
    pub fn extend<'a, S>(&self, names: &'a [S], things: &[T]) -> Self
    where
        &'a S: ToString,
    {
        assert_eq!(names.len(), things.len());
        let mut env = self.clone();
        for (name, thing) in names.iter().zip(things) {
            env = env.assoc(name, thing.clone());
        }
        env
    }
}
