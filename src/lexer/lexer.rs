use lazy_static::lazy_static;
use std::collections::HashMap;
use crate::lexer::token::{Token, TokenType};

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut result = HashMap::new();
        result.insert("fn", TokenType::FUNCTION);
        result.insert("let", TokenType::LET);
        result.insert("true", TokenType::TRUE);
        result.insert("false", TokenType::FALSE);
        result.insert("if", TokenType::IF);
        result.insert("else", TokenType::ELSE);
        result.insert("return", TokenType::RETURN);
        result
    };
}

#[derive(Debug, Clone)]
pub struct Lexer {
    pub input: String,
    pub position: usize,
    pub read_position: usize,
    pub ch: char,
}

impl Lexer {
    pub fn new(input: impl Into<String>) -> Self {
        let mut l = Lexer {
            input: input.into(),
            position: 0,
            read_position: 0,
            ch: 0 as char,
        };
        l.read_char();
        l
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            0 as char
        } else {
            self.input.as_bytes()[self.read_position] as char
        }
    }

    fn read_char(&mut self) {
        self.ch = self.peek_char();
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let ch = self.ch;
        let mut need_read_char = true;
        let token = match ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::EQ, "==")
                } else {
                    Token::new(TokenType::ASSIGN, ch)
                }
            }
            ';' => { Token::new(TokenType::SEMICOLON, ch) }
            '(' => { Token::new(TokenType::LPAREN, ch) }
            ')' => { Token::new(TokenType::RPAREN, ch) }
            ',' => { Token::new(TokenType::COMMA, ch) }
            '+' => { Token::new(TokenType::PLUS, ch) }
            '-' => { Token::new(TokenType::MINUS, ch) }
            '*' => { Token::new(TokenType::ASTERISK, ch) }
            '/' => { Token::new(TokenType::SLASH, ch) }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::NEQ, "!=")
                } else {
                    Token::new(TokenType::BANG, ch)
                }
            }
            '>' => { Token::new(TokenType::GT, ch) }
            '<' => { Token::new(TokenType::LT, ch) }
            '{' => { Token::new(TokenType::LBRACE, ch) }
            '}' => { Token::new(TokenType::RBRACE, ch) }
            c if c == char::from(0) => { Token::new(TokenType::EOF, ch) }
            c if Self::is_letter(c) => {
                need_read_char = false;
                let identifier = self.read_identifier();
                let token_type = Self::look_identifier(identifier);
                Token::new(token_type, identifier)
            }
            c if Self::is_digit(c) => {
                need_read_char = false;
                Token::new(TokenType::INT, self.read_number())
            }
            _ => { Token::new(TokenType::ILLEGAL, ch) }
        };

        if need_read_char {
            self.read_char();
        }

        token
    }

    fn skip_whitespace(&mut self) {
        while Self::is_whitespace(self.ch) {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> &str {
        self.read_literal(Self::is_letter)
    }

    fn read_number(&mut self) -> &str {
        self.read_literal(Self::is_digit)
    }

    fn read_literal(&mut self, predicate: fn(char) -> bool) -> &str {
        let p = self.position;
        while predicate(self.ch) {
            self.read_char();
        }

        &self.input[p..self.position]
    }

    fn look_identifier(identifier: &str) -> TokenType {
        if let Some(token_type) = KEYWORDS.get(identifier) {
            *token_type
        } else {
            TokenType::IDENT
        }
    }

    fn is_whitespace(ch: char) -> bool {
        ch.is_whitespace()
    }

    fn is_letter(ch: char) -> bool {
        ch.is_alphabetic() || ch == '_'
    }

    fn is_digit(ch: char) -> bool {
        ch.is_numeric()
    }
}

impl Iterator for Lexer {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        match token.token_type {
            TokenType::EOF => { None }
            _ => Some(token)
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::lexer::Lexer;
    use crate::lexer::token::{Token, TokenType};

    #[test]
    fn test_lexer() {
        let input = "
let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
}

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
";
        let expected_output = vec![
            Token::new(TokenType::LET, "let"),
            Token::new(TokenType::IDENT, "five"),
            Token::new(TokenType::ASSIGN, "="),
            Token::new(TokenType::INT, "5"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::LET, "let"),
            Token::new(TokenType::IDENT, "ten"),
            Token::new(TokenType::ASSIGN, "="),
            Token::new(TokenType::INT, "10"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::LET, "let"),
            Token::new(TokenType::IDENT, "add"),
            Token::new(TokenType::ASSIGN, "="),
            Token::new(TokenType::FUNCTION, "fn"),
            Token::new(TokenType::LPAREN, "("),
            Token::new(TokenType::IDENT, "x"),
            Token::new(TokenType::COMMA, ","),
            Token::new(TokenType::IDENT, "y"),
            Token::new(TokenType::RPAREN, ")"),
            Token::new(TokenType::LBRACE, "{"),
            Token::new(TokenType::IDENT, "x"),
            Token::new(TokenType::PLUS, "+"),
            Token::new(TokenType::IDENT, "y"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::RBRACE, "}"),
            Token::new(TokenType::LET, "let"),
            Token::new(TokenType::IDENT, "result"),
            Token::new(TokenType::ASSIGN, "="),
            Token::new(TokenType::IDENT, "add"),
            Token::new(TokenType::LPAREN, "("),
            Token::new(TokenType::IDENT, "five"),
            Token::new(TokenType::COMMA, ","),
            Token::new(TokenType::IDENT, "ten"),
            Token::new(TokenType::RPAREN, ")"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::BANG, "!"),
            Token::new(TokenType::MINUS, "-"),
            Token::new(TokenType::SLASH, "/"),
            Token::new(TokenType::ASTERISK, "*"),
            Token::new(TokenType::INT, "5"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::INT, "5"),
            Token::new(TokenType::LT, "<"),
            Token::new(TokenType::INT, "10"),
            Token::new(TokenType::GT, ">"),
            Token::new(TokenType::INT, "5"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::IF, "if"),
            Token::new(TokenType::LPAREN, "("),
            Token::new(TokenType::INT, "5"),
            Token::new(TokenType::LT, "<"),
            Token::new(TokenType::INT, "10"),
            Token::new(TokenType::RPAREN, ")"),
            Token::new(TokenType::LBRACE, "{"),
            Token::new(TokenType::RETURN, "return"),
            Token::new(TokenType::TRUE, "true"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::RBRACE, "}"),
            Token::new(TokenType::ELSE, "else"),
            Token::new(TokenType::LBRACE, "{"),
            Token::new(TokenType::RETURN, "return"),
            Token::new(TokenType::FALSE, "false"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::RBRACE, "}"),
            Token::new(TokenType::INT, "10"),
            Token::new(TokenType::EQ, "=="),
            Token::new(TokenType::INT, "10"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::INT, "10"),
            Token::new(TokenType::NEQ, "!="),
            Token::new(TokenType::INT, "9"),
            Token::new(TokenType::SEMICOLON, ";"),
        ];

        let lexer = Lexer::new(input);
        let result = lexer.into_iter().collect::<Vec<_>>();

        assert_eq!(result, expected_output);
    }
}