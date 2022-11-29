use crate::token::Token;

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
                    Token::EQ
                } else {
                    Token::ASSIGN
                }
            }
            ';' => { Token::SEMICOLON }
            '(' => { Token::LPAREN }
            ')' => { Token::RPAREN }
            ',' => { Token::COMMA }
            '+' => { Token::PLUS }
            '-' => { Token::MINUS }
            '*' => { Token::ASTERISK }
            '/' => { Token::SLASH }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::NEQ
                } else {
                    Token::BANG
                }
            }
            '>' => { Token::GT }
            '<' => { Token::LT }
            '{' => { Token::LBRACE }
            '}' => { Token::RBRACE }
            c if c == char::from(0) => { Token::EOF }
            c if Self::is_letter(c) => {
                need_read_char = false;
                let identifier = self.read_identifier();
                match identifier {
                    "fn" => { Token::FUNCTION }
                    "let" => { Token::LET }
                    "true" => { Token::TRUE }
                    "false" => { Token::FALSE }
                    "if" => { Token::IF }
                    "else" => { Token::ELSE }
                    "return" => { Token::RETURN }
                    _ => { Token::IDENT(identifier.to_string()) }
                }
            }
            c if Self::is_digit(c) => {
                need_read_char = false;
                Token::INT(self.read_number().to_string())
            }
            '"' => { Token::STRING(self.read_string().to_string()) }
            _ => { Token::ILLEGAL(ch.to_string()) }
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

    fn read_string(&mut self) -> &str {
        let p = self.position + 1;
        loop {
            self.read_char();
            if self.ch == '"' || self.ch == 0 as char {
                break;
            }
        }

        &self.input[p..self.position]
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
        match token {
            Token::EOF => { None }
            _ => Some(token)
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
    use crate::token::Token;

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
\"foobar\"
\"foo bar\"
";
        let expected_output = vec![
            Token::LET,
            Token::IDENT("five".to_string()),
            Token::ASSIGN,
            Token::INT("5".to_string()),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("ten".to_string()),
            Token::ASSIGN,
            Token::INT("10".to_string()),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("add".to_string()),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREN,
            Token::IDENT("x".to_string()),
            Token::COMMA,
            Token::IDENT("y".to_string()),
            Token::RPAREN,
            Token::LBRACE,
            Token::IDENT("x".to_string()),
            Token::PLUS,
            Token::IDENT("y".to_string()),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::LET,
            Token::IDENT("result".to_string()),
            Token::ASSIGN,
            Token::IDENT("add".to_string()),
            Token::LPAREN,
            Token::IDENT("five".to_string()),
            Token::COMMA,
            Token::IDENT("ten".to_string()),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::BANG,
            Token::MINUS,
            Token::SLASH,
            Token::ASTERISK,
            Token::INT("5".to_string()),
            Token::SEMICOLON,
            Token::INT("5".to_string()),
            Token::LT,
            Token::INT("10".to_string()),
            Token::GT,
            Token::INT("5".to_string()),
            Token::SEMICOLON,
            Token::IF,
            Token::LPAREN,
            Token::INT("5".to_string()),
            Token::LT,
            Token::INT("10".to_string()),
            Token::RPAREN,
            Token::LBRACE,
            Token::RETURN,
            Token::TRUE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::ELSE,
            Token::LBRACE,
            Token::RETURN,
            Token::FALSE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::INT("10".to_string()),
            Token::EQ,
            Token::INT("10".to_string()),
            Token::SEMICOLON,
            Token::INT("10".to_string()),
            Token::NEQ,
            Token::INT("9".to_string()),
            Token::SEMICOLON,
            Token::STRING("foobar".to_string()),
            Token::STRING("foo bar".to_string()),
        ];

        let lexer = Lexer::new(input);
        let result = lexer.into_iter().collect::<Vec<_>>();

        assert_eq!(result, expected_output);
    }
}
