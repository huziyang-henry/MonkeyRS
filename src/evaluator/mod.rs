use crate::object::{Object};
use crate::parser::expression::Expression;
use crate::parser::node::Node;
use crate::parser::statement::Statement;

// fn eval(node: &Node) -> Object {
//     match node {
//         Node::ProgramNode(_) => {}
//         Node::StatementNode(s) => {
//             match s {
//                 Statement::LetStatement(_) => {}
//                 Statement::ReturnStatement(_) => {}
//                 Statement::ExpressionStatement(stmt) => {
//                     // let exp = stmt.expression.as_ref().unwrap().as_ref();
//                     // eval(Node::ExpressionNode(exp.clone()));
//                 }
//                 Statement::BlockStatement(_) => {}
//             }
//         }
//         Node::ExpressionNode(e) => {
//             match e {
//                 Expression::Identifier(_) => {}
//                 Expression::IntegerLiteral(exp) => { Object::Integer(Integer { value: exp.value }) }
//                 Expression::BooleanLiteral(_) => {}
//                 Expression::FunctionLiteral(_) => {}
//                 Expression::PrefixExpression(_) => {}
//                 Expression::InfixExpression(_) => {}
//                 Expression::IfExpression(_) => {}
//                 Expression::CallExpression(_) => {}
//             }
//         }
//     }
//     todo!()
// }

//
// fn eval_statements(stmts: &Vec<Statement>) -> Object {
//     todo!()
// }
//
#[cfg(test)]
mod test {
    use std::fmt::Pointer;
    // use crate::evaluator::eval;
    use crate::lexer::lexer::Lexer;
    use crate::lexer::token::{Token, TokenType};
    use crate::object::{Object};
    use crate::parser::expression::{BooleanLiteral, Expression, IntegerLiteral};
    use crate::parser::node::{Node, NodeOp};
    use crate::parser::node::Node::ProgramNode;
    use crate::parser::parser::Parser;

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

        println!("{:?}", test_input);


        for input in test_input {
            let mut parser = Parser::new(Lexer::new(input.0));
            let program = parser.parse_program();

            assert_eq!(program.statements.len(), 1);
            assert_eq!(program.statements[0].eval(), Object::Integer(input.1));
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

            assert_eq!(program.statements.len(), 1);
            assert_eq!(program.statements[0].eval(), Object::Boolean(input.1));
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

            assert_eq!(program.statements.len(), 1);

            match program.statements[0].eval() {
                Object::Integer(v) => { assert_eq!(Some(v), input.1) }
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

            assert_eq!(program.eval(), Object::Integer(input.1))
        }
    }
}