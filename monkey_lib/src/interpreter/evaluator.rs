use interpreter::parser::Parser;
use interpreter::lexer::Lexer;
use interpreter::program::Program;
use interpreter::object::{ObjectType};
use interpreter::environment::Environment;
use interpreter::ast::{Statement, StatementKind, Identifier, BlockStatement, Expression, ExpressionKind};
use interpreter::builtins::BuiltInKind;
use std::sync::Arc;
use std::fmt;
use std::cell::RefCell;

const TRUE: ObjectType = ObjectType::Booleans(true);
const FALSE: ObjectType = ObjectType::Booleans(false);
const NULL: ObjectType = ObjectType::Null;

pub struct Evaluator {
    
}

#[macro_export]
macro_rules! new_error {
        ($($arg:tt)*) => (ObjectType::Error(format!($($arg)*)))
    }

impl Evaluator {
    pub fn eval_program(program: &Program, env: &mut Arc<RefCell<Environment>>) -> ObjectType {
        return Evaluator::eval_statements(&program.statements, env);
    }

    pub fn eval_statements(statements: &Vec<Statement>, env: &mut Arc<RefCell<Environment>>) -> ObjectType {
        let mut result = ObjectType::Null;
        for stmt in statements {
            result = Evaluator::eval_statement(&stmt, env);
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

    pub fn eval_statement(statement: &Statement, env: &mut Arc<RefCell<Environment>>) -> ObjectType {
        match statement.stmtKind {
            StatementKind::ExpressionStatement(ref t, ref e) => {
                if let Some(ex) = e.clone() {
                    return Evaluator::eval_expression(&ex, env);
                }
                else {
                    return NULL;
                }
            },
            StatementKind::ReturnStatement(ref t, ref e) => {
                if let Some(ex) = e.clone() {
                    let result = Evaluator::eval_expression(&ex, env);
                    if Evaluator::is_error(&result) {
                        return result;
                    }
                    return ObjectType::Return(Box::new(result.clone()));
                }
                else {
                    return NULL;
                }
            },
            StatementKind::LetStatement(ref t, ref i, ref e) => {
                if let Some(ex) = e.clone() {
                    let val = Evaluator::eval_expression(&ex, env);
                    if Evaluator::is_error(&val) {
                        return val;
                    }

                    env.borrow_mut().set(&i.value.clone(), &val);
                }
                else {
                    return NULL;
                }
            }
            _ => {
                return NULL;
            }
        }

        return NULL;
    }

    fn eval_block_statement(block: &BlockStatement, env: &mut Arc<RefCell<Environment>>) -> ObjectType {
        let mut result = ObjectType::Null;
        let stmts = block.statements.clone();
        for stmt in stmts {
            result = Evaluator::eval_statement(&stmt, env);
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

    pub fn eval_expression(expression: &Expression, env: &mut Arc<RefCell<Environment>>) -> ObjectType {
        match expression.exprKind {
            ExpressionKind::Ident(ref t, ref i) => {
                return Evaluator::eval_identifier(i, env);
            }
            ExpressionKind::IntegerLiteral(ref t, ref i) => {
                return ObjectType::Integer(*i);
            },
            ExpressionKind::Boolean(ref t, ref b) => {
                return Evaluator::native_bool_to_boolean_object(*b)
            },
            ExpressionKind::PrefixExpression(ref t, ref o, ref e) => {

                // let ee = e.clone();
                let ref ex = *e.clone();
                let right = Evaluator::eval_expression(ex, env);
                if Evaluator::is_error(&right) {
                    return right;
                }
                return Evaluator::eval_prefix_expression(o, right, env);
            },
            ExpressionKind::InfixExpression(ref t, ref l, ref o, ref r) => {
                let ref le = *l.clone();
                let left = Evaluator::eval_expression(le, env);
                if Evaluator::is_error(&left) {
                    return left;
                }

                let ref re = *r.clone();
                let right = Evaluator::eval_expression(re, env);
                if Evaluator::is_error(&right) {
                    return right;
                }

                return Evaluator::eval_infix_expression(o, left, right, env);
            },
            ExpressionKind::If(..) => {
                return Evaluator::eval_if_expression(&expression.clone(), env);
            },
            ExpressionKind::FunctionLiteral(ref t, ref b, ref ids) => {
                return ObjectType::Function(b.clone(), ids.clone(), env.clone());
            },
            ExpressionKind::Call(ref t, ref f, ref a) => {
                let function = Evaluator::eval_expression(f, env);
                if Evaluator::is_error(&function) {
                    return function;
                }

                let args = Evaluator::eval_expressions(a.clone(), env);
                if args.len() == 1 && Evaluator::is_error(&args[0]) {
                    return args[0].clone();
                }

                println!("About to apply function");
                return Evaluator::apply_function(&function, &args);
            },
            ExpressionKind::StringLiteral(ref t, ref v) => {
                return ObjectType::String(v.clone())
            },
            ExpressionKind::ArrayLiteral(ref t, ref e) => {
                let elements = Evaluator::eval_expressions(e.clone(), env);
                if elements.len() == 1 && Evaluator::is_error(&elements[0]) {
                    return elements[0].clone();
                }

                return ObjectType::Array(elements);
            },
            ExpressionKind::IndexExpression(ref t, ref l, ref idx) => {
                let left = Evaluator::eval_expression(l, env);
                if Evaluator::is_error(&left) {
                    return left;
                }

                let index = Evaluator::eval_expression(idx, env);
                if Evaluator::is_error(&index) {
                    return index;
                }

                return Evaluator::eval_index_expression(&left, &index);
            },
            _ => {
                return NULL
            }
        }
    }

    fn apply_function(func: &ObjectType, args: &Vec<ObjectType>) -> ObjectType {
        match *func {
            ObjectType::Function(ref a, ref b, ref e) => {
                let mut extended_env = Evaluator::extend_function_env(func, args);
                let evaluated = Evaluator::eval_block_statement(b, &mut extended_env);
                return Evaluator::unwrap_return_value(&evaluated);
            },
            ObjectType::BuiltInIdentifier(ref bid) => {
                println!("apply_function BuildinIdentifier branch");
                return BuiltInKind::execute(bid, args);
            },
            _ => {
                return new_error!("not a function: {}", func);
            }
        }
    }

    fn extend_function_env(func: &ObjectType, args: &Vec<ObjectType>) -> Arc<RefCell<Environment>> {
        match *func {
            ObjectType::Function(ref a, ref b, ref e) => {
                let mut env = Arc::new(RefCell::new(Environment::new_enclosed(Some(e.clone()))));

                for (index, param) in a.iter().enumerate() {
                    env.borrow_mut().set(&param.value.clone(), &args[index]);
                }

                return env;
            },
            _ => {
                return Arc::new(RefCell::new(Environment::new()));
            }
        }
    }

    fn unwrap_return_value(obj: &ObjectType) -> ObjectType {
        match *obj {
            ObjectType::Return(ref s) => {
                return *s.clone();
            },
            _ => {
                return obj.clone();
            }
        }
    }

    fn eval_expressions(exps: Vec<Expression>, env: &mut Arc<RefCell<Environment>>) -> Vec<ObjectType> {
        let mut result = Vec::new();

        for e in exps {
            let evaluated = Evaluator::eval_expression(&e, env);
            if Evaluator::is_error(&evaluated) {
                result.push(evaluated);
                return result;
            }
            result.push(evaluated.clone());
        }

        result
    }

    fn eval_index_expression(left: &ObjectType, index: &ObjectType) -> ObjectType {
        match *left {
            ObjectType::Array(ref e) => {
                match *index {
                    ObjectType::Integer(ref i) => {
                        return Evaluator::eval_array_index_expression(left, index);
                    },
                    _ => { return new_error!("index operator not supported: {}", left); }
                }
            },
            _ => {
                return new_error!("index operator not supported: {}", index);
            }
        }
    }

    fn eval_array_index_expression(array: &ObjectType, index: &ObjectType) -> ObjectType {
        match *array {
            ObjectType::Array(ref e) => {
                match *index {
                    ObjectType::Integer(ref i) => {
                        let max = (e.len() - 1) as i64;
                        let idx = *i;

                        if *i < 0 || *i > max {
                            return NULL;
                        }

                        return e[idx as usize].clone();
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

    fn eval_identifier(identifier: &String, env: &mut Arc<RefCell<Environment>>) -> ObjectType {
        let val = env.borrow().get(identifier.clone());
        match val {
            Some(v) => v,
            None => {
                println!("Looking up builtin: {}", identifier);
                let builtin = BuiltInKind::get_identifier(identifier);
                if builtin != NULL {
                    return builtin;
                }

                new_error!("identifier not found: {}", identifier)
            }
        }
    }

    fn eval_if_expression(exp: &Expression, env: &mut Arc<RefCell<Environment>>) -> ObjectType {
        match exp.exprKind{
            ExpressionKind::If(ref t, ref c, ref cs, ref alt) => {
                let condition = Evaluator::eval_expression(c, env);
                if Evaluator::is_error(&condition) {
                    return condition;
                }

                if Evaluator::is_truthy(&condition) {
                    return Evaluator::eval_block_statement(&cs, env);
                }

                if let Some(a) = alt.clone() {
                    return Evaluator::eval_block_statement(&a, env);
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

    fn is_truthy(obj: &ObjectType) -> bool {
        match *obj {
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

    fn is_error(obj: &ObjectType) -> bool {
        match *obj {
            ObjectType::Error(..) => true,
            _ => false
        }
    }

    fn eval_prefix_expression(operator: &String, right: ObjectType, env: &Arc<RefCell<Environment>>) -> ObjectType {
        match operator.as_str() {
            "!" => {
                return Evaluator::eval_bang_operator_expression(right, env)
            },
            "-" => {
                return Evaluator::eval_minus_prefix_operator_expression(right, env)  
            },
            _ => {
                return new_error!("unknown operator: {}{}", operator, right.get_type())
            }
        }
    }

    fn eval_infix_expression(operator: &String, left: ObjectType, right: ObjectType, env: &Arc<RefCell<Environment>>) -> ObjectType {
        match left {
            ObjectType::Integer(..) => {
                match right {
                    ObjectType::Integer(..) => {
                        return Evaluator::eval_integer_infix_expression(operator.clone(), left, right, env);
                    },
                    _ => {
                        return new_error!("type mismatch: {} {} {}", left.get_type(), operator, right.get_type())
                    }
                }
            },
            ObjectType::String(..) => {
                match right {
                    ObjectType::String(..) => {
                        return Evaluator::eval_string_infix_expression(operator, &left, &right);
                    },
                    _ => {
                        return new_error!("unknown operator: {} {} {}", left.get_type(), operator, right.get_type());
                    }
                }
            }
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

    fn eval_string_infix_expression(operator: &String, left: &ObjectType, right: &ObjectType) -> ObjectType {
        if operator != "+" {
            return new_error!("unknown operator: {} {} {}", left.get_type(), operator, right.get_type());
        }

        let leftVal = Evaluator::get_string_value(left);
        let rightVal = Evaluator::get_string_value(right);
        return ObjectType::String(leftVal + rightVal.as_str());
    }

    fn get_string_value(obj: &ObjectType) -> String {
        match *obj {
            ObjectType::String(ref s) => {
                s.clone()
            },
            _ => {
                String::new()
            }
        }
    }

    fn eval_integer_infix_expression(operator: String, left: ObjectType, right: ObjectType, env: &Arc<RefCell<Environment>>) -> ObjectType {
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

    fn eval_minus_prefix_operator_expression(right: ObjectType, env: &Arc<RefCell<Environment>>) -> ObjectType {
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

    fn eval_bang_operator_expression(right: ObjectType, env: &Arc<RefCell<Environment>>) -> ObjectType {
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
        let mut env = Arc::new(RefCell::new(Environment::new()));

        return Evaluator::eval_program(&program, &mut env);
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
                println!("object is not integer. got={}", *object);
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
            TestData { input: String::from("foobar"), expected_message: String::from("identifier not found: foobar")},
            TestData { input: String::from(r#""Hello" - "World""#), expected_message: String::from("unknown operator: STRING - STRING")},
        ];

        for tt in tests {
            let evaluated = test_eval(tt.input.clone());

            match evaluated {
                ObjectType::Error(ref err) => {
                    assert!(*err == tt.expected_message, "wrong error message. expected={}, got={}", tt.expected_message, *err);
                },
                _ => {
                    println!("no error object returned. got={}", evaluated);
                    continue;
                }
            }
        }
    }

    #[test]
    fn test_let_statements() {
        struct TestData {
            input: String,
            expected: i64
        }

        let tests = vec![
            TestData { input: String::from("let a = 5; a;"), expected: 5},
            TestData { input: String::from("let a = 5 * 5; a;"), expected: 25},
            TestData { input: String::from("let a = 5; let b = a; b;"), expected: 5},
            TestData { input: String::from("let a = 5; let b = a; let c = a + b + 5; c;"), expected: 15}
        ];

        for tt in tests {
            let evaluated = test_eval(tt.input.clone());
            assert!(test_integer_object(&evaluated, &tt.expected));
        }
    }

    #[test]
    fn test_function_object() {
        let input = String::from("fn(x) { x + 2; };");

        let evaluated = test_eval(input.clone());
        match evaluated {
            ObjectType::Function(ref ids, ref b, ref e) => {
                if ids.len() != 1 {
                    assert!(false, "function has wrong parameters. Parameters={:?}", ids);
                }

                let first_param = ids[0].clone();
                let first_ident = format!("{}", first_param);
                assert!(first_ident == "x", "parameter is not 'x'. got={}", first_ident);

                let expected_body = "(x + 2)";

                let body = format!("{}", b);
                assert!(body == expected_body, "body is not {}. got={}", expected_body, body);
            },
            _ => {
                assert!(false, "object is not function. got={}", evaluated);
            }
        }
    }

    #[test]
    fn test_function_application() {
        struct TestData {
            input: String,
            expected: i64
        }

        let tests = vec![
            TestData{ input: String::from("let identity = fn(x) { x; }; identity(5);"), expected: 5},
            TestData{ input: String::from("let identity = fn(x) { return x; }; identity(5);"), expected: 5},
            TestData{ input: String::from("let double = fn(x) { x * 2; }; double(5);"), expected: 10},
            TestData{ input: String::from("let add = fn(x, y) { x + y; }; add(5, 5);"), expected: 10},
            TestData{ input: String::from("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));"), expected: 20},
            TestData{ input: String::from("fn(x) { x; }(5);"), expected: 5}
        ];

        for tt in tests {
            test_integer_object(&test_eval(tt.input.clone()), &tt.expected);
        }        
    }

    #[test]
    fn test_closures() {
        let input = String::from(r#"let newAdder = fn(x) {
                                         fn(y) { x + y };
                                    };

                                    let addTwo = newAdder(2);
                                    addTwo(2);"#);

        assert!(test_integer_object(&test_eval(input.clone()), &4));
    }

    #[test]
    fn test_string_literal() {
        let input = String::from(r#""Hello World!""#);

        let evaluated = test_eval(input.clone());
        match evaluated {
            ObjectType::String(ref s) => {
                assert!(s == "Hello World!", "String has wrong value. got={}", s);
            },
            _ => {
                assert!(false, "object is not String. got={}", evaluated);
            }
        }
    }

    #[test]
    fn test_string_concatenation() {
        let input = String::from(r#""Hello" + " " + "World!""#);

        let evaluated = test_eval(input.clone());
        match evaluated {
            ObjectType::String(ref s) => {
                assert!(s == "Hello World!", "String has wrong value. got={}", s);
            },
            _ => {
                assert!(false, "object is not String. got={}", evaluated);
            }
        }
    }

    #[test]
    fn test_builtin_functions() {
        struct TestData {
            input: String,
            expected_int: Option<i64>,
            expected_str: Option<String>
        }

        let tests = vec![
            TestData { input: String::from(r#"len("")"#), expected_int: Some(0), expected_str: None},
            TestData { input: String::from(r#"len("four")"#), expected_int: Some(4), expected_str: None},
            TestData { input: String::from(r#"len("hello world!")"#), expected_int: Some(12), expected_str: None},
            TestData { input: String::from(r#"len(1)"#), expected_int: None, expected_str: Some(String::from("argument to 'len' not supported, got INTEGER"))},
            TestData { input: String::from(r#"len("one", "two")"#), expected_int: None, expected_str: Some(String::from("wrong number of arguments. got=2, want=1"))},
        ];

        for tt in tests {
            println!("TestCase: {}", tt.input);
            let evaluated = test_eval(tt.input.clone());

            println!("Evaluated:  {}", evaluated);
            if let Some(i) = tt.expected_int {
                assert!(test_integer_object(&evaluated, &i));
            }

            if let Some(s) = tt.expected_str {
                match evaluated {
                    ObjectType::Error(ref e) => {
                        assert!(*e == s, "wrong error message. expected={}, got={}", s, e);
                    },
                    _ => {
                        println!("object is not Error. got={}", evaluated);
                        continue;
                    }
                }
            }
        }
    }

    #[test]
    fn test_array_literals() {
        let input = String::from("[1, 2 * 2, 3 + 3]");

        let evaluated = test_eval(input.clone());
        match evaluated {
            ObjectType::Array(ref a) => {
                if a.len() != 3 {
                    assert!(false, "array has wrong num of elements. got={}", a.len());
                }

                assert!(test_integer_object(&a[0], &1));
                assert!(test_integer_object(&a[1], &4));
                assert!(test_integer_object(&a[2], &6));
            },
            _ => {
                assert!(false, "object is not Array/ got={}", evaluated);
            }
        }
    }

    #[test]
    fn test_array_index_expressions() {
        struct TestData {
            input: String,
            expected: Option<i64>
        }

        let tests = vec![
            TestData{ input: "[1, 2, 3][0]".into(), expected: Some(1)},
            TestData{ input: "[1, 2, 3][1]".into(), expected: Some(2)},
            TestData{ input: "[1, 2, 3][2]".into(), expected: Some(3)},
            TestData{ input: "let i = 0; [1][i]".into(), expected: Some(1)},
            TestData{ input: "[1, 2, 3][1 + 1]".into(), expected: Some(3)},
            TestData{ input: "let myArray = [1, 2, 3]; myArray[2]".into(), expected: Some(3)},
            TestData{ input: "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];".into(), expected: Some(6)},
            TestData{ input: "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]".into(), expected: Some(2)},
            TestData{ input: "[1, 2, 3][3]".into(), expected: None},
            TestData{ input: "[1, 2, 3][-1]".into(), expected: None},
        ];

        for tt in tests {
            let evaluated = test_eval(tt.input);
            if let Some(i) = tt.expected {
                test_integer_object(&evaluated, &i);
            }
            else {
                test_null_object(&evaluated);
            }
        }
    }
}
