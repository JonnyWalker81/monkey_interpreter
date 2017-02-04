use std::fmt;
use std::vec::Vec;
use std::sync::Arc;
use std::cell::RefCell;
use std::hash;
use std::collections::hash_map::RandomState;
use std::collections::HashMap;
use std::hash::{ Hash, Hasher, SipHasher };
use interpreter::ast::{Identifier, BlockStatement};
use interpreter::environment::Environment;
use interpreter::builtins::{ BuiltInKind, BuiltInIdentifier};
use interpreter::token::{ Token, NumberType };
use seahash::SeaHasher;

#[derive(PartialEq, Eq, Clone)]
pub enum ObjectType {
    // Integer(i64),
    // Float(String),
    Number(NumberType, String),
    Null,
    Booleans(bool),
    Return(Box<ObjectType>),
    Function(Vec<Identifier>, BlockStatement, Arc<RefCell<Environment>>),
    String(String),
    BuiltIn(BuiltInKind),
    BuiltInIdentifier(BuiltInIdentifier),
    Array(Vec<ObjectType>),
    Hash(HashMap<HashKey, HashPair>),
    Import(String),
    Error(String)
}

pub trait Hashable {
    fn hash(&self) -> HashKey;
}

#[derive(PartialEq, Eq, Clone)]
pub struct HashPair {
    pub key: ObjectType,
    pub value: ObjectType
}

#[derive(PartialEq, Eq, Clone, Hash)]
pub struct HashKey {
    obj_type: String,
    value: usize
}

impl Default for HashKey {
    fn default() -> HashKey {
        HashKey { obj_type: "".into(), value: 0}
    }
}

impl ObjectType {
    pub fn get_type(&self) -> &str {
        match *self {
            // ObjectType::Integer(..) => {
            //     "INTEGER"
            // },
            // ObjectType::Float(..) => {
            //     "FLOAT"
            // },
            ObjectType::Number(ref nt, _) => {
                match *nt {
                    NumberType::Integer => "INTEGER",
                    NumberType::Float => "FLOAT"
                }
            }
            ObjectType::Null => {
                "Null"
            },
            ObjectType::Booleans(..) => {
                "BOOLEAN"
            },
            ObjectType::Return(..) => {
                "Return"
            },
            ObjectType::Error(..) => {
                "Error"
            }
            ObjectType::Function(..) => {
                "fn"
            },
            ObjectType::String(..) => {
                "STRING"
            }, 
            ObjectType::BuiltIn(..) => {
                "builtin function"
            },
            ObjectType::BuiltInIdentifier(..) => {
                "builtin identifier"
            },
            ObjectType::Array(..) => {
                "ARRAY"
            },
            ObjectType::Hash(..) => {
                "HASH"
            },
            ObjectType::Import(..) => {
                "IMPORT"
            }
        }
    }
}

impl Hashable for ObjectType {
    fn hash(&self) -> HashKey {
        match *self {
            ObjectType::Booleans(ref b) => {
                let value = match *b {
                    true => 1,
                    false => 2
                };

                HashKey{ obj_type: self.get_type().into(), value: value}
            },
            ObjectType::Number(ref t, ref i) => {
                match *t {
                    NumberType::Integer => {
                        let val: usize = i.parse().unwrap_or(0);
                        HashKey{ obj_type: self.get_type().into(), value: val}
                    },
                    _ => {
                        panic!("Unsupported HashMap type -> {}", *self);
                        HashKey::default();
                    }
                }
            },
            ObjectType::String(ref s) => {
                // let mut hasher = RandomState::new();
                // let hash = Hash::hash(&(*s.into_bytes()), &mut hasher);
                // return hash;

                let mut hasher = SeaHasher::new();
                hasher.write(&(*s.as_bytes()));
                HashKey{obj_type: self.get_type().into(), value: hasher.finish() as usize}
            },
            _ => {
                panic!("Unsupported HashMap type -> {}", *self);
                HashKey::default();
            }
        }
    }
}

impl fmt::Display for ObjectType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let printable = match *self {
            // ObjectType::Integer(ref i) => {
            //     format!("{}", i)
            // },
            // ObjectType::Float(ref s) => {
            //     format!("{}", s)
            // },
            ObjectType::Number(_, ref n) => {
              format!("{}", n)
            },
            ObjectType::Null => {
                format!("null")
            },
            ObjectType::Booleans(ref b) => {
                format!("{}", b)
            },
            ObjectType::Return(ref v) => {
                format!("{}", *v)
            },
            ObjectType::Function(ref ids, ref b, ref e) => {
                let mut params = Vec::new();

                for p in ids {
                    let param_str = format!("{}", p);
                    params.push(param_str);
                }

                let mut result = String::new();

                result.push_str("fn");
                result.push_str("(");
                result.push_str(params.join(", ").as_str());
                result.push_str(") {\n");
                result.push_str(format!("{}", b).as_str());
                result.push_str("\n");

                result
            },
            ObjectType::Error(ref s) => {
                format!("{}", *s)
            },
            ObjectType::String(ref s) => {
                format!("{}", s)
            },
            ObjectType::BuiltIn(..) => {
                format!("builtin function")
            },
            ObjectType::BuiltInIdentifier(ref s) => {
                format!("builtin identifier")
            },
            ObjectType::Array(ref a) => {
                let mut elements = Vec::new();

                for e in a {
                    elements.push(format!("{}", e));
                }

                let mut result = String::new();

                result.push_str("[");
                result.push_str(elements.join(", ").as_str());
                result.push_str("]");

                result
            },
            ObjectType::Hash(ref hm) => {
                let mut pairs = Vec::new();

                for (ref key, ref val) in hm {
                    pairs.push(format!("{}: {}", val.key, val.value));
                }

                let mut result = String::new();
                result.push_str("{");
                result.push_str(pairs.join(", ").as_str());
                result.push_str("}");

                result
            },
            ObjectType::Import(ref s) => {
                format!("import {}", s)
            }
        };

        write!(f, "{}", printable)
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_hash_key() {
        let hello1 = ObjectType::String("Hello World".into());
        let hello2 = ObjectType::String("Hello World".into());
        let diff1 = ObjectType::String("My name is johnny".into());
        let diff2 = ObjectType::String("My name is johnny".into());

        let hash1 = hello1.hash();
        let hash2 = hello2.hash();     

        assert!(hash1 == hash2, "strings with the same content have differnt hash keys");

        let diff_hash1 = diff1.hash();
        let diff_hash2 = diff2.hash();

        assert!(diff_hash1 == diff_hash2, "strings with same content have differnt hash keys");

        assert!(hash1 != diff_hash1, "string with different content have same hash keys");
    }
}
