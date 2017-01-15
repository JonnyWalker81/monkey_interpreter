use std::fmt;
use std::vec::Vec;
use std::sync::Arc;
use std::cell::RefCell;
use interpreter::ast::{Identifier, BlockStatement};
use interpreter::environment::Environment;

#[derive(PartialEq, Eq, Clone)]
pub enum ObjectType {
    Integer(i64),
    Null,
    Booleans(bool),
    Return(Box<ObjectType>),
    Function(Vec<Identifier>, BlockStatement, Arc<RefCell<Environment>>),
    String(String),
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
            }
        };

        write!(f, "{}", printable)
    }
}
