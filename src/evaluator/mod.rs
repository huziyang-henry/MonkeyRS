use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;
use crate::object::{Object, ObjectError};
use crate::object::environment::Environment;

pub trait Evaluator: Display {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Object, ObjectError>;
}

#[cfg(test)]
mod test {
    use std::cell::RefCell;
    use std::rc::Rc;
    use crate::evaluator::Evaluator;
    use crate::lexer::Lexer;
    use crate::object::{Object, ObjectError};
    use crate::object::environment::Environment;
    use crate::parser::Parser;

    #[test]
    fn test_eval_integer_expression() {
        let test_input = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];


        for input in test_input {
            let mut parser = Parser::new(Lexer::new(input.0));
            let program = parser.parse_program();
            let env = Rc::new(RefCell::new(Environment::new()));
            assert_eq!(program.eval(Rc::clone(&env)), Ok(Object::Integer(input.1)));
        }
    }

    #[test]
    fn test_eval_bool_expression() {
        let test_input = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for input in test_input {
            let mut parser = Parser::new(Lexer::new(input.0));
            let program = parser.parse_program();

            let env = Rc::new(RefCell::new(Environment::new()));
            assert_eq!(program.eval(Rc::clone(&env)), Ok(Object::Boolean(input.1)));
        }
    }

    #[test]
    fn test_if_else_expression() {
        let test_input = vec![
            ("if (true) { 10 }", Some(10)),
            ("if (false) { 10 }", None),
            ("if (1) { 10 }", Some(10)),
            ("if (1 < 2) { 10 }", Some(10)),
            ("if (1 > 2) { 10 }", None),
            ("if (1 > 2) { 10 } else { 20 }", Some(20)),
            ("if (1 < 2) { 10 } else { 20 }", Some(10)),
        ];

        for input in test_input {
            let mut parser = Parser::new(Lexer::new(input.0));
            let program = parser.parse_program();

            let env = Rc::new(RefCell::new(Environment::new()));
            match program.eval(Rc::clone(&env)) {
                Ok(Object::Integer(v)) => { assert_eq!(Some(v), input.1) }
                _ => { assert_eq!(None, input.1) }
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let test_input = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                "if (10 > 1) {
  if (10 > 1) {
    return 10;
  }

  return 1;
}", 10),
        ];

        for input in test_input {
            let mut parser = Parser::new(Lexer::new(input.0));
            let program = parser.parse_program();

            let env = Rc::new(RefCell::new(Environment::new()));
            assert_eq!(program.eval(Rc::clone(&env)), Ok(Object::Integer(input.1)))
        }
    }

    #[test]
    fn test_error_handling() {
        let test_input = vec![
            (
                "5 + true;",
                "type mismatch: Integer(5) + Boolean(true)",
            ),
            (
                "5 + true; 5;",
                "type mismatch: Integer(5) + Boolean(true)",
            ),
            (
                "-true",
                "unknown operator: -Boolean(true)",
            ),
            (
                "true + false;",
                "unknown operator: Boolean(true) + Boolean(false)",
            ),
            (
                "5; true + false; 5",
                "unknown operator: Boolean(true) + Boolean(false)",
            ),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: Boolean(true) + Boolean(false)",
            ),
            (
                "
if (10 > 1) {
  if (10 > 1) {
    return true + false;
  }

  return 1;
}
",
                "unknown operator: Boolean(true) + Boolean(false)",
            ),
            ("foobar", "identifier not found: foobar"),
        ];

        for input in test_input {
            let mut parser = Parser::new(Lexer::new(input.0));
            let program = parser.parse_program();

            let env = Rc::new(RefCell::new(Environment::new()));
            assert_eq!(program.eval(Rc::clone(&env)), Err(ObjectError::new(input.1.to_string())));
        }
    }

    #[test]
    fn test_let_statements() {
        let test_input = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for input in test_input {
            let mut parser = Parser::new(Lexer::new(input.0));
            let program = parser.parse_program();

            let env = Rc::new(RefCell::new(Environment::new()));
            assert_eq!(program.eval(Rc::clone(&env)), Ok(Object::Integer(input.1)));
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; }";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();
        let env = Rc::new(RefCell::new(Environment::new()));
        let result = program.eval(env);
        if let Ok(Object::Function(f)) = result {
            assert_eq!(f.parameters.len(), 1);
            assert_eq!(f.parameters[0].to_string(), "x");
            assert_eq!(f.body.to_string(), "(x + 2)");
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_function_application() {
        let test_input = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
            ("let add = fn(a, b) { a + b}; let sub = fn(a, b){a - b}; let appFn = fn(a, b, func) {func(a, b);}; appFn(2,2, add);", 4),
            ("let add = fn(a, b) { a + b}; let sub = fn(a, b){a - b}; let appFn = fn(a, b, func) {func(a, b);}; appFn(2,2, sub);", 0),
        ];

        for input in test_input {
            let mut parser = Parser::new(Lexer::new(input.0));
            let program = parser.parse_program();

            let env = Rc::new(RefCell::new(Environment::new()));
            assert_eq!(program.eval(Rc::clone(&env)), Ok(Object::Integer(input.1)));
        }
    }
}


