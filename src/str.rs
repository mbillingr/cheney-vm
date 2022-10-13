use std::borrow::Borrow;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;
use std::rc::Rc;

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Str(Rc<str>);

impl Str {
    pub fn interned(s: &str) -> Self {
        Str(s.into())
    }
}

impl PartialEq<str> for Str {
    fn eq(&self, other: &str) -> bool {
        &*self.0 == other
    }
}

impl PartialEq<&str> for Str {
    fn eq(&self, other: &&str) -> bool {
        &*self.0 == *other
    }
}

impl Debug for Str {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl Display for Str {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Deref for Str {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Borrow<str> for Str {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl From<&Str> for Str {
    fn from(s: &Str) -> Self {
        s.clone()
    }
}

impl<T: Into<Rc<str>>> From<T> for Str {
    fn from(s: T) -> Self {
        Str(s.into())
    }
}
