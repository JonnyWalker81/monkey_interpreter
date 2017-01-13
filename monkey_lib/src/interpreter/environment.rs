use interpreter::object::ObjectType;
use std::collections::HashMap;

pub struct Environment {
    pub store: HashMap<String, ObjectType>
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new()
        }
    }

    pub fn get(&self, name: String) -> Option<ObjectType> {
        let obj = self.store.get(&name);
        match obj {
            Some(o) => Some(o.clone()),
            None => None
        }
    }

    pub fn set(&mut self, name: String, val: &ObjectType) -> ObjectType {
        self.store.insert(name.clone(), val.clone());
        val.clone()
    }
}
