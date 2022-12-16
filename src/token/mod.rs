use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    ILLEGAL(String),
    EOF,

    IDENT(String),
    INT(String),
    STRING(String),

    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,

    LT,
    GT,

    EQ,
    NEQ,

    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,

    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.literal())
    }
}

impl Token {
    pub fn literal(&self) -> &str {
        match self {
            Token::ILLEGAL(s) => s.as_str(),
            Token::EOF => "\0",
            Token::IDENT(ident) => ident,
            Token::INT(i) => i,
            Token::STRING(s) => s,
            Token::ASSIGN => "=",
            Token::PLUS => "+",
            Token::MINUS => "-",
            Token::BANG => "!",
            Token::ASTERISK => "*",
            Token::SLASH => "/",
            Token::LT => "<",
            Token::GT => ">",
            Token::EQ => "==",
            Token::NEQ => "!=",
            Token::COMMA => ",",
            Token::SEMICOLON => ";",
            Token::LPAREN => "(",
            Token::RPAREN => ")",
            Token::LBRACE => "{",
            Token::RBRACE => "}",
            Token::LBRACKET => "[",
            Token::RBRACKET => "]",
            Token::FUNCTION => "fn",
            Token::LET => "let",
            Token::TRUE => "true",
            Token::FALSE => "false",
            Token::IF => "if",
            Token::ELSE => "else",
            Token::RETURN => "return",
        }
    }
}
