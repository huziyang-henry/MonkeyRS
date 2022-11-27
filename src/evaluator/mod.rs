use std::fmt::Display;
use crate::object::{Object, ObjectError};
use crate::parser::expression::Expression;
use crate::parser::node::Node;
use crate::parser::statement::Statement;

pub trait Evaluator: Display {
    fn eval(&self) -> Result<Object, ObjectError>;
}

#[cfg(test)]
mod test {
    use std::fmt::Pointer;
    use crate::evaluator::Evaluator;
    use crate::lexer::Lexer;
    use crate::token::Token;
    use crate::object::{Object, ObjectError};
    use crate::parser::expression::{BooleanLiteral, Expression, IntegerLiteral};
    use crate::parser::node::Node;
    use crate::parser::node::Node::ProgramNode;
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

            assert_eq!(program.eval(), Ok(Object::Integer(input.1)));
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

            assert_eq!(program.eval(), Ok(Object::Boolean(input.1)));
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

            match program.eval() {
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

            assert_eq!(program.eval(), Ok(Object::Integer(input.1)))
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
        ];

        for input in test_input {
            let mut parser = Parser::new(Lexer::new(input.0));
            let program = parser.parse_program();

            assert_eq!(program.eval(), Err(ObjectError::new(input.1.to_string())));
        }
    }
}


