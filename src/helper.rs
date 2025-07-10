//! A helper mock to hack doctests

#![doc(hidden)]
#![allow(unused)]

use std::cell::RefCell;
thread_local! {
    pub static DEPTH: RefCell<usize> = RefCell::new(0);
}
