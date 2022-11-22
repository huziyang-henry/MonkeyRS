use std::mem::swap;

use crate::lexer::lexer::Lexer;
use crate::lexer::token::{Token, TokenType};
use crate::parser::expression::Expression;
use crate::parser::program::Program;
use crate::parser::statement::{ExpressionStatement, Identifier, InfixExpression, IntegerLiteral, LetStatement, PrefixExpression, ReturnStatement, Statement};

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
enum PrecedenceType {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
}

pub struct Parser {
    lexer: Lexer,
    errors: Vec<String>,
    cur_token: Token,
    peek_token: Token,
}


impl Parser {
    fn new(mut lexer: Lexer) -> Self {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Parser {
            lexer,
            errors: Vec::new(),
            cur_token,
            peek_token,
        }
    }

    fn peek_error(&mut self, token_type: TokenType) {
        let msg = format!("expected next token to be {:?}, got {:?} instead", token_type, self.peek_token);
        self.errors.push(msg);
    }

    fn next_token(&mut self) {
        swap(&mut self.cur_token, &mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: Vec::new() };

        while self.cur_token.token_type != TokenType::EOF {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.cur_token.token_type {
            TokenType::LET => { self.parse_let_statement() }
            TokenType::RETURN => { self.parse_return_statement() }
            _ => { self.parse_expression_statement() }
        }
    }

    fn parse_let_statement(&mut self) -> Option<Box<dyn Statement>> {
        let token = self.cur_token.clone();
        if !self.expect_peek(TokenType::IDENT) {
            return None;
        }

        let name = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };

        if !self.expect_peek(TokenType::ASSIGN) {
            return None;
        }

        while self.cur_token.token_type != TokenType::SEMICOLON {
            self.next_token();
        }

        Some(Box::new(LetStatement {
            token,
            name,
            value: None,
        }))
    }

    fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let token = self.cur_token.clone();

        while self.cur_token.token_type != TokenType::SEMICOLON {
            self.next_token();
        }

        Some(Box::new(ReturnStatement { token, value: None }))
    }

    fn parse_expression_statement(&mut self) -> Option<Box<dyn Statement>> {
        let e = ExpressionStatement {
            token: self.cur_token.clone(),
            expression: self.parse_expression(PrecedenceType::LOWEST),
        };

        if self.peek_token.token_type == TokenType::SEMICOLON {
            self.next_token();
        }

        Some(Box::new(e))
    }

    fn parse_expression(&mut self, precedence: PrecedenceType) -> Option<Box<dyn Expression>> {
        let mut left = match self.cur_token.token_type {
            TokenType::IDENT => { self.parse_identifier() }
            TokenType::INT => { self.parse_integer_literal() }
            TokenType::BANG | TokenType::MINUS => { self.parse_prefix_expression() }
            _ => {
                self.errors.push(format!("no prefix parse function for {:?} found", self.cur_token.token_type));
                None
            }
        }?;


        while self.peek_token.token_type != TokenType::SEMICOLON
            && precedence < Self::get_precedence(self.peek_token.token_type)
            && matches!(self.peek_token.token_type, TokenType::PLUS
                | TokenType::MINUS
                | TokenType::SLASH
                | TokenType::ASTERISK
                | TokenType::EQ
                | TokenType::NEQ
                | TokenType::LT
                | TokenType::GT) {
            self.next_token();
            left = self.parse_infix_expression(left)?;
        }

        Some(left)
    }

    fn parse_identifier(&mut self) -> Option<Box<dyn Expression>> {
        Some(Box::new(Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        }))
    }

    fn parse_integer_literal(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();
        let value_result = token.literal.parse::<i64>();
        match value_result {
            Ok(value) => {
                Some(Box::new(IntegerLiteral {
                    token,
                    value,
                }))
            }
            Err(_) => {
                self.errors.push(format!("count not parse {} as integer", token.literal));
                None
            }
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();
        let operator = token.literal.clone();
        self.next_token();
        let right = self.parse_expression(PrecedenceType::PREFIX)?;
        Some(Box::new(PrefixExpression {
            token,
            operator,
            right,
        }))
    }

    fn parse_infix_expression(&mut self, left: Box<dyn Expression>) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();
        let operator = token.literal.clone();
        let left = left;

        let precedence = Self::get_precedence(self.cur_token.token_type);
        self.next_token();
        let right = self.parse_expression(precedence)?;
        Some(Box::new(InfixExpression {
            token,
            left,
            operator,
            right,
        }))
    }

    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.peek_token.token_type == token_type {
            self.next_token();
            true
        } else {
            self.peek_error(token_type);
            false
        }
    }

    fn get_precedence(token_type: TokenType) -> PrecedenceType {
        match token_type {
            TokenType::EQ | TokenType::NEQ => { PrecedenceType::EQUALS }
            TokenType::LT | TokenType::GT => { PrecedenceType::LESSGREATER }
            TokenType::PLUS | TokenType::MINUS => { PrecedenceType::SUM }
            TokenType::SLASH | TokenType::ASTERISK => { PrecedenceType::PRODUCT }
            _ => PrecedenceType::LOWEST
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::node::Node;
    use crate::parser::statement::{ExpressionStatement, InfixExpression, PrefixExpression};

    use super::*;

    #[test]
    fn test_parser() {
        let input = "
let x = 5;
let y = 10;
return 5;
";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        if parser.errors.len() > 0 {
            println!("parser.errors.len() = {:?}, {:?}", parser.errors.len(), parser.errors);
            assert_eq!(parser.errors.len(), 0);
            return;
        }

        let statements = program.statements;

        let s = (*statements[0]).as_any().downcast_ref::<LetStatement>().unwrap();
        assert_eq!(s.token_literal(), "let");
        assert_eq!(s.name.value, "x");
        assert_eq!(s.name.token_literal(), "x");

        let s = (*statements[1]).as_any().downcast_ref::<LetStatement>().unwrap();
        assert_eq!(s.token_literal(), "let");
        assert_eq!(s.name.value, "y");
        assert_eq!(s.name.token_literal(), "y");

        let s = (*statements[2]).as_any().downcast_ref::<ReturnStatement>().unwrap();
        assert_eq!(s.token_literal(), "return");
    }

    #[test]
    fn test_parser2() {
        let input = "let myVar = anotherVar;";
        let expected_output = Program {
            statements: vec![
                Box::new(LetStatement {
                    token: Token::new(TokenType::LET, "let"),
                    name: Identifier {
                        token: Token::new(TokenType::IDENT, "myVar"),
                        value: "myVar".to_string(),
                    },
                    value: Some(Box::new(Identifier {
                        token: Token::new(TokenType::IDENT, "anotherVar"),
                        value: "anotherVar".to_string(),
                    })),
                })
            ]
        };

        assert_eq!(input, expected_output.to_string());
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        if parser.errors.len() > 0 {
            println!("error: {:?}", parser.errors);
            return;
        }

        for s in &program.statements {
            println!("{}", s);
        }

        assert_eq!(program.statements.len(), 1);

        let id =
            program.statements[0]
                .as_ref()
                .as_any().downcast_ref::<ExpressionStatement>()
                .unwrap()
                .expression
                .as_ref()
                .unwrap()
                .as_ref()
                .as_any().downcast_ref::<Identifier>()
                .unwrap();

        assert_eq!(id.value, "foobar");
        assert_eq!(id.token_literal(), "foobar");
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        if parser.errors.len() > 0 {
            println!("{:?}", parser.errors);
            return;
        }

        for s in &program.statements {
            println!("{}", s);
        }

        assert_eq!(program.statements.len(), 1);
        let int_literal =
            program.statements[0]
                .as_ref()
                .as_any().downcast_ref::<ExpressionStatement>()
                .unwrap()
                .expression
                .as_ref()
                .unwrap()
                .as_ref()
                .as_any().downcast_ref::<IntegerLiteral>()
                .unwrap();

        assert_eq!(int_literal.value, 5);
        assert_eq!(int_literal.token_literal(), "5");
    }

    #[test]
    fn test_prefix_expression() {
        let prefix_tests = vec![
            ("!5", "!", 5),
            ("-15", "-", 15),
        ];

        for test in prefix_tests {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            if parser.errors.len() > 0 {
                println!("{:?}", parser.errors);
                assert!(false);
                continue;
            }

            for s in &program.statements {
                println!("{}", s);
            }
            assert_eq!(program.statements.len(), 1);
            let r = program.statements[0]
                .as_ref()
                .as_any().downcast_ref::<ExpressionStatement>()
                .unwrap()
                .expression
                .as_ref()
                .unwrap()
                .as_ref()
                .as_any().downcast_ref::<PrefixExpression>()
                .unwrap();

            assert_eq!(r.operator, test.1);
            test_integer_literal(r.right.as_ref(), test.2);
        }
    }

    fn test_integer_literal(exp: &dyn Expression, value: i64) {
        let r = exp
            .as_any().downcast_ref::<IntegerLiteral>()
            .unwrap();

        assert_eq!(r.value, value);
        assert_eq!(r.token_literal(), value.to_string());
    }

    #[test]
    fn test_infix_expression() {
        let infix_tests = vec![
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];

        for test in infix_tests {
            let mut parser = Parser::new(Lexer::new(test.0));
            let program = parser.parse_program();
            if parser.errors.len() > 0 {
                println!("{:?}", parser.errors);
                assert!(false);
                continue;
            }

            for s in &program.statements {
                println!("{}", s);
            }
            assert_eq!(program.statements.len(), 1);

            let r =
                program.statements[0]
                    .as_ref()
                    .as_any().downcast_ref::<ExpressionStatement>()
                    .unwrap()
                    .expression
                    .as_ref()
                    .unwrap()
                    .as_ref()
                    .as_any().downcast_ref::<InfixExpression>()
                    .unwrap();

            test_integer_literal(r.left.as_ref(), test.1);
            assert_eq!(r.operator, test.2);
            test_integer_literal(r.right.as_ref(), test.3);
        }
    }
}