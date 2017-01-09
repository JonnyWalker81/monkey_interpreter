use std::fmt;

#[derive(PartialEq, Eq)]
pub enum ObjectType {
    Integer(i64),
    Null,
    Booleans(bool)
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
            }
        };

        write!(f, "{}", printable)
    }
}
