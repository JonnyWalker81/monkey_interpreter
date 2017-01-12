use interpreter::parser::Parser;
use interpreter::lexer::Lexer;
use interpreter::program::Program;
use interpreter::object::{ObjectType};
use interpreter::ast::{Statement, StatementKind, BlockStatement, Expression, ExpressionKind};
use std::sync::Arc;
use std::fmt;

const TRUE: ObjectType = ObjectType::Booleans(true);
const FALSE: ObjectType = ObjectType::Booleans(false);
const NULL: ObjectType = ObjectType::Null;

pub struct Evaluator {
    
}

    macro_rules! new_error {
        ($($arg:tt)*) => (ObjectType::Error(format!($($arg)*)))
    }

impl Evaluator {
    pub fn eval_program(program: &Program) -> ObjectType {
        return Evaluator::eval_statements(&program.statements);
    }

    pub fn eval_statements(statements: &Vec<Statement>) -> ObjectType {
        let mut result = ObjectType::Null;
        for stmt in statements {
            result = Evaluator::eval_statement(&stmt);
            match result {
                ObjectType::Return(ref v) => {
                    let val = v.clone();
                    return *val;
                },
                ObjectType::Error(..) => {
                    return result;
                }
                _ => {
                }
            }
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
                    return NULL;
                }
            },
            StatementKind::ReturnStatement(ref t, ref e) => {
                if let Some(ex) = e.clone() {
                    let result = Evaluator::eval_expression(&ex);
                    return ObjectType::Return(Box::new(result.clone()));
                }
                else {
                    return NULL;
                }
            },
            _ => {
                return NULL;
            }
        }
    }

    fn eval_block_statement(block: &BlockStatement) -> ObjectType {
        let mut result = ObjectType::Null;
        let stmts = block.statements.clone();
        for stmt in stmts {
            result = Evaluator::eval_statement(&stmt);
            match result {
                ObjectType::Return(..) => {
                    return result;
                },
                ObjectType::Error(..) => {
                    return result;
                }
                _ => {
                }
            }
        }

        return result;
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
            ExpressionKind::If(..) => {
                return Evaluator::eval_if_expression(&expression.clone());
            },
            _ => {
                return NULL
            }
        }
    }

    fn eval_if_expression(exp: &Expression) -> ObjectType {
        match exp.exprKind{
            ExpressionKind::If(ref t, ref c, ref cs, ref alt) => {
                let condition = Evaluator::eval_expression(c);
                if Evaluator::is_truthy(condition) {
                    return Evaluator::eval_block_statement(&cs);
                }

                if let Some(a) = alt.clone() {
                    return Evaluator::eval_block_statement(&a);
                }
                else {
                    return NULL;
                }
            },
            _ => {
                return NULL;
            }
        }
    }

    fn is_truthy(obj: ObjectType) -> bool {
        match obj {
            ObjectType::Null => {
                return false;
            },
            ObjectType::Booleans(ref b) => {
                return *b;
            },
            _ => {
                return true;
            }
        }
    }

    fn is_error(obj: ObjectType) -> bool {
        match obj {
            ObjectType::Error(..) => true,
            _ => false
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
                return new_error!("unknown operator: {}{}", operator, right.get_type())
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
                        return new_error!("type mismatch: {} {} {}", left.get_type(), operator, right.get_type())
                    }
                }
            },
            _ => {
                match operator.as_str() {
                    "==" => {
                        return Evaluator::native_bool_to_boolean_object(left == right);
                    },
                    "!=" => {
                        return Evaluator::native_bool_to_boolean_object(left != right);
                    },
                    _ => {
                        return new_error!("unknown operator: {} {} {}", left.get_type(), operator, right.get_type());
                    }
                }
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
            "<" => {
                return Evaluator::native_bool_to_boolean_object(leftVal < rightVal);
            },
            ">" => {
                return Evaluator::native_bool_to_boolean_object(leftVal > rightVal);
            },
            "==" => {
                return Evaluator::native_bool_to_boolean_object(leftVal == rightVal);
            },
            "!=" => {
                return Evaluator::native_bool_to_boolean_object(leftVal != rightVal);
            }
            _ => {
                return new_error!("unknown operator: {} {} {}", left.get_type(), operator, right.get_type())
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
                return new_error!("unknown operator: -{}", right.get_type())
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
            TestData{ input: String::from("false"), expected: false},
            TestData{ input: String::from("1 < 2"), expected: true},
            TestData{ input: String::from("1 > 2"), expected: false},
            TestData{ input: String::from("1 < 1"), expected: false},
            TestData{ input: String::from("1 > 1"), expected: false},
            TestData{ input: String::from("1 == 1"), expected: true},
            TestData{ input: String::from("1 != 1"), expected: false},
            TestData{ input: String::from("1 == 2"), expected: false},
            TestData{ input: String::from("1 != 2"), expected: true},
            TestData{ input: String::from("true == true"), expected: true},
            TestData{ input: String::from("false == false"), expected: true},
            TestData{ input: String::from("true == false"), expected: false},
            TestData{ input: String::from("true != false"), expected: true},
            TestData{ input: String::from("false != true"), expected: true},
            TestData{ input: String::from("( 1 < 2 ) == true"), expected: true},
            TestData{ input: String::from("( 1 < 2 ) == false"), expected: false},
            TestData{ input: String::from("( 1 > 2 ) == true"), expected: false},
            TestData{ input: String::from("( 1 > 2 ) == false"), expected: true}
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

    #[test]
    fn test_if_else_expressions() {
        struct TestData {
            input: String,
            expected: Option<i32>
        }

        let tests = vec![
            TestData {input: String::from("if (true) { 10 }"), expected: Some(10)},
            TestData {input: String::from("if (false) { 10 }"), expected: None},
            TestData {input: String::from("if (1) { 10 }"), expected: Some(10)},
            TestData {input: String::from("if (1 < 2) { 10 }"), expected: Some(10)},
            TestData {input: String::from("if (1 > 2) { 10 }"), expected: None},
            TestData {input: String::from("if (1 > 2) { 10 } else { 20 }"), expected: Some(20)},
            TestData {input: String::from("if (1 < 2) { 10 } else { 20 }"), expected: Some(10)}
        ];

        for tt in tests {
            let evaluated = test_eval(tt.input.clone());
            if let Some(i) = tt.expected {
                let ii = i as i64;
                test_integer_object(&evaluated, &ii);
            }
            else {
                test_null_object(&evaluated);
            }
        }
    }

    fn test_null_object(obj: &ObjectType) -> bool {
        if *obj != NULL {
            println!("object is not NULL. got={}", obj);
            return false;
        }

        return true;
    }

    #[test]
    fn test_return_statements() {
        struct TestData {
            input: String,
            expected: i64
        }

        let tests = vec![
            TestData {input: String::from("return 10;"), expected: 10},
            TestData {input: String::from("return 10; 9;"), expected: 10},
            TestData {input: String::from("return 2 * 5; 9;"), expected: 10},
            TestData {input: String::from("9; return 2 * 5; 9;"), expected: 10},
            TestData {input: String::from(r#"if (10 > 1) {
                                               if (10 > 1) {
                                                     return 10;
                                                  }
                                               return 1;
                                               }"#), expected: 10},
        ];

        for tt in tests {
            let evaluated = test_eval(tt.input.clone());
            assert!(test_integer_object(&evaluated, &tt.expected));
        }
    }

    #[test]
    fn test_error_handling() {
        struct TestData {
            input: String,
            expected_message: String
        }

        let tests = vec![
            TestData { input: String::from("5 + true;"), expected_message: String::from("type mismatch: INTEGER + BOOLEAN")},
            TestData { input: String::from("5 + true; 5;"), expected_message: String::from("type mismatch: INTEGER + BOOLEAN")},
            TestData { input: String::from("-true;"), expected_message: String::from("unknown operator: -BOOLEAN")},
            TestData { input: String::from("true + false;"), expected_message: String::from("unknown operator: BOOLEAN + BOOLEAN")},
            TestData { input: String::from("5; true + false; 5"), expected_message: String::from("unknown operator: BOOLEAN + BOOLEAN")},
            TestData { input: String::from("if (10 > 1) { true + false; }"), expected_message: String::from("unknown operator: BOOLEAN + BOOLEAN")},
            TestData { input: String::from(r#"if (10 > 1) {
                                                if (10 > 1) {
                                                    return true + false;
                                                }
                                            return 1;
                                            }"#), expected_message: String::from("unknown operator: BOOLEAN + BOOLEAN")},
        ];

        for tt in tests {
            let evaluated = test_eval(tt.input.clone());

            match evaluated {
                ObjectType::Error(ref err) => {
                    assert!(*err == tt.expected_message, "wrong error message. expected={}, got={}", *err, tt.expected_message);
                },
                _ => {
                    println!("no error object returned. got={}", evaluated);
                    continue;
                }
            }
        }
    }
}
