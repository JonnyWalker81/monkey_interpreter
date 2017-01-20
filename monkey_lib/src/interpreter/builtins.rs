use interpreter::object::ObjectType;
#[macro_use]
use interpreter::evaluator::Evaluator;
use std::sync::Arc;
use std::fmt;

#[derive(PartialEq, Eq, Clone)]
pub enum BuiltInKind {
    Len(Arc<ObjectType>),
    First(Arc<ObjectType>),
    Last(Arc<ObjectType>),
    Rest(Arc<ObjectType>),
    Push(Arc<ObjectType>)
}

#[derive(PartialEq, Eq, Clone)]
pub enum BuiltInIdentifier {
    Len,
    First,
    Last,
    Rest,
    Push
}

impl BuiltInKind {
    pub fn execute(ident: &BuiltInIdentifier, args: &Vec<ObjectType>) -> ObjectType {
        println!("Executing builtin -> {}", ident);
        match *ident {
            BuiltInIdentifier::Len => {
                if args.len() != 1 {
                    return ObjectType::Error(format!("wrong number of arguments. got={}, want=1", args.len()));
                }

                let input = args[0].clone();
                match input {
                    ObjectType::String(ref s) => {
                        return ObjectType::Integer(s.len() as i64);
                    },
                    ObjectType::Array(ref e) => {
                        return ObjectType::Integer(e.len() as i64);
                    },
                    _ => {
                        return ObjectType::Error(format!("argument to 'len' not supported, got {}", input.get_type()));
                    }
                }
            },
            BuiltInIdentifier::First => {
                if args.len() != 1 {
                    return ObjectType::Error(format!("wrong number of arguments. got={}, want=1", args.len()));
                }

                let arg0 = args[0].clone();
                match arg0 {
                    ObjectType::Array(ref a) => {
                        if a.len() > 0 {
                            return a[0].clone();
                        }

                        return ObjectType::Null;
                    },
                    _ => {
                        return ObjectType::Null;
                    }
                }
            },
            BuiltInIdentifier::Last => {
                if args.len() != 1 {
                    return ObjectType::Error(format!("wrong number of arguments. got={}, want=1", args.len()));
                }

                let arg0 = args[0].clone();
                match arg0 {
                    ObjectType::Array(ref a) => {
                        let length = a.len();
                        if length > 0 {
                            return a[length - 1].clone();
                        }

                        return ObjectType::Null;
                    },
                    _ => {
                        return ObjectType::Null;
                    }
                }
            },
            BuiltInIdentifier::Rest => {
                if args.len() != 1 {
                    return ObjectType::Error(format!("wrong number of arguments. got={}", args.len()))
                }

                let arg0 = args[0].clone();
                match arg0 {
                    ObjectType::Array(mut a) => {
                        let length = a.len();
                        if length > 0 {
                            let new_elements = a.split_off(1);
                            return ObjectType::Array(new_elements);
                        }

                        return ObjectType::Null;

                    },
                    _ => {
                        return ObjectType::Error(format!("argument to 'rest' must be ARRAY, got={}", arg0));
                    }
                }
            },
            BuiltInIdentifier::Push => {
                if args.len() != 2 {
                    return ObjectType::Error(format!("wrong number of aguments. got={}, want=2", args.len()));
                }

                let arg0 = args[0].clone();
                match arg0 {
                    ObjectType::Array(ref a) => {
                        let length = a.len();
                        let mut new_elements = a.clone();
                        let arg1 = args[1].clone();
                        new_elements.push(arg1);
                        return ObjectType::Array(new_elements);
                    },
                    _ => {
                        return ObjectType::Error(format!("argument to 'push' must be ARRAY, got={}", arg0));
                    }
                }
            }
        }
    }
    
    pub fn get_identifier(name: &String) -> ObjectType {
        match name.as_str() {
            "len" => ObjectType::BuiltInIdentifier(BuiltInIdentifier::Len),
            "first" => ObjectType::BuiltInIdentifier(BuiltInIdentifier::First),
            "last" => ObjectType::BuiltInIdentifier(BuiltInIdentifier::Last),
            "rest" => ObjectType::BuiltInIdentifier(BuiltInIdentifier::Rest),
            "push" => ObjectType::BuiltInIdentifier(BuiltInIdentifier::Push),
            _ => ObjectType::Null
        }
    }
}

impl fmt::Display for BuiltInIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let printable = match *self {
            BuiltInIdentifier::Len => {
                format!("Len")
            },
            BuiltInIdentifier::First => {
                format!("first")
            },
            BuiltInIdentifier::Last => {
                format!("last")
            },
            BuiltInIdentifier::Rest => {
                format!("rest")
            },
            BuiltInIdentifier::Push => {
                format!("push")
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
            },
            BuiltInKind::First(..) => {
                format!("first builtin")
            },
            BuiltInKind::Last(..) => {
                format!("last builtin")
            },
            BuiltInKind::Rest(..) => {
                format!("rest builtin")
            },
            BuiltInKind::Push(..) => {
                format!("push builtin")
            }
        };

        write!(f, "{}", printable)
    }
}
