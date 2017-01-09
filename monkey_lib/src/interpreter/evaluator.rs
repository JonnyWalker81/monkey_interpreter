use interpreter::parser::Parser;
use interpreter::lexer::Lexer;
use interpreter::program::Program;
use interpreter::object::{ObjectType};
use interpreter::ast::{Statement, StatementKind, Expression, ExpressionKind};
use std::sync::Arc;

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
                    return Evaluator::eval_expression(&ex);
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

    pub fn eval_expression(expression: &Expression) -> ObjectType {
        match expression.exprKind {
            ExpressionKind::IntegerLiteral(ref t, ref i) => {
                return ObjectType::Integer(*i);
            },
            ExpressionKind::Boolean(ref t, ref b) => {
                return Evaluator::native_bool_to_boolean_object(*b)
            },
            ExpressionKind::PrefixExpression(ref t, ref o, ref e) => {

                // let ee = e.clone();
                let ref ex = *e.clone();
                    let right = Evaluator::eval_expression(ex);
                    return Evaluator::eval_prefix_expression(o, right);
            },
            ExpressionKind::InfixExpression(ref t, ref l, ref o, ref r) => {
                let ref le = *l.clone();
                let left = Evaluator::eval_expression(le);

                let ref re = *r.clone();
                let right = Evaluator::eval_expression(re);

                return Evaluator::eval_infix_expression(o, left, right);
            },
            _ => {
                return NULL
            }
        }
    }

    fn eval_prefix_expression(operator: &String, right: ObjectType) -> ObjectType {
        match operator.as_str() {
            "!" => {
                return Evaluator::eval_bang_operator_expression(right)
            },
            "-" => {
                return Evaluator::eval_minus_prefix_operator_expression(right)  
            },
            _ => {
                NULL
            }
        }
    }

    fn eval_infix_expression(operator: &String, left: ObjectType, right: ObjectType) -> ObjectType {
        match left {
            ObjectType::Integer(..) => {
                match right {
                    ObjectType::Integer(..) => {
                        return Evaluator::eval_integer_infix_expression(operator.clone(), left, right);
                    },
                    _ => {
                        return NULL;
                    }
                }
            },
            _ => {
                return NULL;
            }
        }
    }

    fn eval_integer_infix_expression(operator: String, left: ObjectType, right: ObjectType) -> ObjectType {
        let leftVal = match left {
            ObjectType::Integer(ref i) => {
                *i
            },
            _ => {
                -1
            }
        };

        let rightVal = match right {
            ObjectType::Integer(ref i) => {
                *i
            },
            _ => {
                -1
            }
        };
        
        match operator.as_str() {
            "+" => {
                return ObjectType::Integer(leftVal + rightVal);
            },
            "-" => {
                return ObjectType::Integer(leftVal - rightVal);
            }
            "*" => {
                return ObjectType::Integer(leftVal * rightVal);
            }
            "/" => {
                return ObjectType::Integer(leftVal / rightVal);
            },
            _ => {
                return NULL;
            }
        } 
    }

    fn eval_minus_prefix_operator_expression(right: ObjectType) -> ObjectType {
        match right {
            ObjectType::Integer(ref i) => {
                let value = *i;
                return ObjectType::Integer(-value);
            },
            _ => {
                return NULL;
            }
        } 
    }

    fn eval_bang_operator_expression(right: ObjectType) -> ObjectType {
        match right {
            TRUE => {
                FALSE
            },
            FALSE => {
                TRUE
            },
            NULL => {
                TRUE
            },
            _ => {
                FALSE
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
            TestData{ input: String::from("10"), expected: 10},
            TestData{ input: String::from("-5"), expected: -5},
            TestData{ input: String::from("-10"), expected: -10},
            TestData{ input: String::from("5 + 5 + 5 + 5 - 10"), expected: 10},
            TestData{ input: String::from("2 * 2 * 2 * 2 * 2"), expected: 32},
            TestData{ input: String::from("-50 + 100 + -50"), expected: 0},
            TestData{ input: String::from("5 * 2 + 10"), expected: 20},
            TestData{ input: String::from("5 + 2 * 10"), expected: 25},
            TestData{ input: String::from("20 + 2 * -10"), expected: 0},
            TestData{ input: String::from("50 / 2 * 2 + 10"), expected: 60},
            TestData{ input: String::from("2 * (5 + 10)"), expected: 30}, 
            TestData{ input: String::from("3 * 3 * 3 + 10"), expected: 37},
            TestData{ input: String::from("3 * (3 * 3) + 10"), expected: 37},
            TestData{ input: String::from("(5 + 10 * 2 + 15 / 3) * 2 + -10"), expected: 50}
        ];

        for tt in tests {
            let evaluated = test_eval(tt.input.clone());
            assert!(test_integer_object(&evaluated, &tt.expected));
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
            assert!( test_boolean_object(&evaluated, &tt.expected) );
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

    #[test]
    fn test_bang_operator() {
        struct TestData {
            input: String,
            expected: bool
        }

        let tests = vec![
            TestData { input: String::from("!true"), expected: false },
            TestData { input: String::from("!false"), expected: true },
            TestData { input: String::from("!5"), expected: false },
            TestData { input: String::from("!!true"), expected: true },
            TestData { input: String::from("!!false"), expected: false },
            TestData { input: String::from("!!5"), expected: true }
        ];

        for tt in tests {
            let evaluated = test_eval(tt.input.clone());
            assert!( test_boolean_object(&evaluated, &tt.expected) );
        }
    }
}
