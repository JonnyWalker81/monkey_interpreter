use interpreter::object::ObjectType;
#[macro_use]
use interpreter::evaluator::Evaluator;
use std::sync::Arc;
use std::fmt;

#[derive(PartialEq, Eq, Clone)]
pub enum BuiltInKind {
    Len(Arc<ObjectType>)
}

#[derive(PartialEq, Eq, Clone)]
pub enum BuiltInIdentifier {
    Len
}

impl BuiltInKind {
    pub fn execute(ident: &BuiltInIdentifier, args: &Vec<ObjectType>) -> ObjectType {
        println!("Executing builtin");
        if args.len() != 1 {
            return ObjectType::Error(format!("wrong number of arguments. got={}, want=1", args.len()));
        }
        match *ident {
            BuiltInIdentifier::Len => {
                let input = args[0].clone();
                match input {
                    ObjectType::String(ref s) => {
                        return ObjectType::Integer(s.len() as i64);
                    },
                    _ => {
                        return ObjectType::Error(format!("argument to 'len' not supported, got {}", input.get_type()));
                    }
                }
            }
        }
    }
    
    pub fn get_identifier(name: &String) -> ObjectType {
        match name.as_str() {
            "len" => ObjectType::BuiltInIdentifier(BuiltInIdentifier::Len),
            _ => ObjectType::Null
        }
    }
}

impl fmt::Display for BuiltInIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let printable = match *self {
            BuiltInIdentifier::Len => {
                format!("Len")
            }
        };

        write!(f, "{}", printable)
    }
}

impl fmt::Display for BuiltInKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let printable = match *self {
            BuiltInKind::Len(ref o) => {
                format!("Len buildin")
            }
        };

        write!(f, "{}", printable)
    }
}
