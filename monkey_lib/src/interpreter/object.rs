use std::fmt;
use std::sync::Arc;

#[derive(PartialEq, Eq, Clone)]
pub enum ObjectType {
    Integer(i64),
    Null,
    Booleans(bool),
    Return(Box<ObjectType>),
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
            ObjectType::Error(ref s) => {
                format!("{}", *s)
            }
        };

        write!(f, "{}", printable)
    }
}
