use interpreter::parser::Parser;
use interpreter::lexer::Lexer;
use interpreter::program::Program;
use interpreter::object::{ObjectType};
use interpreter::ast::{Statement, StatementKind, Expression, ExpressionKind};

const TRUE: ObjectType = ObjectType::Booleans(true);
const FALSE: ObjectType = ObjectType::Booleans(false);
const NULL: ObjectType = ObjectType::Null;

pub struct Evaluator {
    
}

impl Evaluator {
    pub fn eval_program(program: &Program) -> ObjectType {
        return Evaluator::eval_statements(&program.statements);
    }

    pub fn eval_statements(statements: &Vec<Statement>) -> ObjectType {
        let mut result = ObjectType::Null;
        for stmt in statements {
            result = Evaluator::eval_statement(&stmt);
        }

        return result;
    }

    pub fn eval_statement(statement: &Statement) -> ObjectType {
        match statement.stmtKind {
            StatementKind::ExpressionStatement(ref t, ref e) => {
                if let Some(ex) = e.clone() {
                    return Evaluator::eval_expression(ex);
                }
                else {
                    return ObjectType::Null;
                }
            },
            _ => {
                return ObjectType::Null;
            }
        }
    }

    pub fn eval_expression(expression: Expression) -> ObjectType {
        match expression.exprKind {
            ExpressionKind::IntegerLiteral(ref t, ref i) => {
                return ObjectType::Integer(*i);
            },
            ExpressionKind::Boolean(ref t, ref b) => {
                return Evaluator::native_bool_to_boolean_object(*b)
            }
            _ => {
                return NULL
            }
        }
    }

    fn native_bool_to_boolean_object(input: bool) -> ObjectType {
        match input {
            true => TRUE,
            false => FALSE
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval_integer_expression() {
        struct TestData {
            input: String,
            expected: i64
        }

        let tests = vec![
            TestData{ input: String::from("5"), expected: 5},
            TestData{ input: String::from("10"), expected: 10}
        ];

        for tt in tests {
            let evaluated = test_eval(tt.input.clone());
            test_integer_object(&evaluated, &tt.expected);
        }
    }

    fn test_eval(input: String) -> ObjectType {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap_or_default();

        return Evaluator::eval_program(&program);
    }

    fn test_integer_object(object: &ObjectType, expected: &i64) -> bool {
        match *object {
            ObjectType::Integer(ref i) => {
                if *i != *expected {
                    println!("object has wrong value. got={}, want={}", *i, expected);
                    return false;
                }
            },
            _ => {
                println!("object is not integer. got={}", object);
                return false;
            }
        }

        return true;
    }

    #[test]
    fn test_eval_boolean_expression() {
        struct TestData {
            input: String,
            expected: bool
        }

        let tests = vec![
            TestData{ input: String::from("true"), expected: true},
            TestData{ input: String::from("false"), expected: false}
        ];

        for tt in tests {
            let evaluated = test_eval(tt.input.clone());
            test_boolean_object(&evaluated, &tt.expected);
        }
    }

    fn test_boolean_object(obj: &ObjectType, expected: &bool) -> bool {
        match *obj {
            ObjectType::Booleans(ref b) => {
                if *b != *expected {
                    println!("object has wrong value. got={}, want={}", *b, expected);
                    return false;
                }
            },
            _ => {
                println!("object is not integer. got={}", obj);
                return false;
            }
        }

        return true;
    }
}
