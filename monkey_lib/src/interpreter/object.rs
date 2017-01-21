use std::fmt;
use std::vec::Vec;
use std::sync::Arc;
use std::cell::RefCell;
use std::hash;
use std::collections::hash_map::RandomState;
use std::hash::{ Hash, Hasher, SipHasher };
use interpreter::ast::{Identifier, BlockStatement};
use interpreter::environment::Environment;
use interpreter::builtins::{ BuiltInKind, BuiltInIdentifier};

#[derive(PartialEq, Eq, Clone)]
pub enum ObjectType {
    Integer(i64),
    Null,
    Booleans(bool),
    Return(Box<ObjectType>),
    Function(Vec<Identifier>, BlockStatement, Arc<RefCell<Environment>>),
    String(String),
    BuiltIn(BuiltInKind),
    BuiltInIdentifier(BuiltInIdentifier),
    Array(Vec<ObjectType>),
    Error(String)
}

impl ObjectType {
    pub fn get_type(&self) -> &str {
        match *self {
            ObjectType::Integer(..) => {
                "INTEGER"
            },
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
            }
        }
    }

    fn hash<T: Hash>(t: &T) -> usize {
        let mut s = SipHasher::new();
        t.hash(&mut s);
        s.finish() as usize
    }
    
    pub fn get_hash(&self) -> usize {
        match *self {
            ObjectType::Booleans(ref b) => {
                let value = match *b {
                    true => 1,
                    false => 2
                };

                return value;
            },
            ObjectType::Integer(ref i) => {
                return *i as usize;
            },
            ObjectType::String(ref s) => {
                let mut hasher = RandomState::new();
                let hash = Hash::hash(&(*s.into_bytes()), &mut hasher);
                return hash;
            }
        } 
    }
}

impl fmt::Display for ObjectType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let printable = match *self {
            ObjectType::Integer(ref i) => {
                format!("{}", i)
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

        let hash1 = hello1.get_hash();
        let hash2 = hello2.get_hash();     

        assert!(hash1 == hash2, "strings with the same content have differnt hash keys");

        let diff_hash1 = diff1.get_hash();
        let diff_hash2 = diff2.get_hash();

        assert!(diff_hash1 == diff_hash2, "strings with same content have differnt hash keys");

        assert!(hash1 != diff_hash1, "string with different content have same hash keys");
    }
}
