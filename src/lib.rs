#![recursion_limit = "256"]

#[macro_export]
macro_rules! mark {
    ($trait:path: $($t:ty),*) => {
        $(
            impl $trait for $t {}
        )*
    }
}

#[macro_export]
macro_rules! boxvec {
    ($($x:expr),*) => {
        vec![$(Box::new($x)),*]
    }
}

#[macro_export]
macro_rules! intovec {
    ($($x:expr),*) => {
        vec![$($x.into()),*]
    }
}

#[macro_export]
macro_rules! join {
    () => { vec![] };
    ($x:expr) => { $x };
    ($first:expr, $($more:expr),*) => {
        {
            let mut items = $first;
            $(
                items.extend($more);
            )*
            items
        }
    };
}

#[macro_export]
macro_rules! strx {
    (( $($x:expr),* $(,)? )) => { $crate::StrStruct::List(vec![$(strx!($x)),*]) };
    ($x:expr) => { $x.serialize() };
}

mod env;
mod memory;
//pub mod simple_type_lang;
mod str;
pub mod tier02_vmlang;
pub mod tier03_types;
//pub mod typelang;
pub mod vm;
//pub mod vmlang;

pub trait Serialize {
    fn serialize(&self) -> StrStruct;
}

#[derive(Debug, Eq, PartialEq)]
pub enum StrStruct {
    Leaf(String),
    List(Vec<StrStruct>),
}

impl Serialize for u64 {
    fn serialize(&self) -> StrStruct {
        StrStruct::Leaf(self.to_string())
    }
}

impl Serialize for str {
    fn serialize(&self) -> StrStruct {
        StrStruct::Leaf(self.to_string())
    }
}

impl Serialize for String {
    fn serialize(&self) -> StrStruct {
        StrStruct::Leaf(self.clone())
    }
}

impl<T: Serialize> Serialize for [T] {
    fn serialize(&self) -> StrStruct {
        StrStruct::List(self.iter().map(T::serialize).collect())
    }
}

impl<T: Serialize> Serialize for Box<T> {
    fn serialize(&self) -> StrStruct {
        (**self).serialize()
    }
}
