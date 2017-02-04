#[macro_use] extern crate maplit;
extern crate seahash;
extern crate float_cmp;
extern crate readline;
extern crate libc;


pub mod interpreter;
use std::slice;
use libc::size_t;

pub extern fn evaluate_input(bytes: *const u8, len: size_t)  {
    
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
