#![recursion_limit = "256"]

macro_rules! mark {
    ($trait:path: $($t:ty),*) => {
        $(
            impl $trait for $t {}
        )*
    }
}

macro_rules! boxvec {
    ($($x:expr),*) => {
        vec![$(Box::new($x)),*]
    }
}

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

mod memory;
pub mod rtlang;
pub mod typelang;
pub mod vm;
pub mod vmlang;
