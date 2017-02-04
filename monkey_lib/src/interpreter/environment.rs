use interpreter::object::ObjectType;
use std::collections::HashMap;
use std::sync::{ Arc, RwLock };
use std::cell::Cell;
use std::cell::RefCell;

#[derive(PartialEq, Eq, Clone)]
pub struct Environment {
    pub store: HashMap<String, ObjectType>,
    pub outer: Option<Arc<RefCell<Environment>>>
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
            outer: None
        }
    }

    pub fn new_enclosed(outer: Option<Arc<RefCell<Environment>>>) -> Environment {
        Environment {
            store: HashMap::new(),
            outer: outer
        }
    }

    pub fn get(&self, name: String) -> Option<ObjectType> {
        let mut obj = self.store.get(&name);
        match obj {
            Some(o) => Some(o.clone()),
            None => {
                if let Some(ref outer) = self.outer {
                    let ref store = outer.borrow().store;
                    let result = store.get(&name.clone());
                    match result {
                        Some(ot) => return Some(ot.clone()),
                        None => return None
                    }
                }

                None
            }
        }
    }

    pub fn set(&mut self, name: &String, val: &ObjectType) -> ObjectType {
        self.store.insert(name.clone(), val.clone());
        val.clone()
    }

    pub fn dump(&self) {
        let c = self.store.clone();
        for (k, v) in c {
            println!("{}: {}", k, v);
        }
    }
}
