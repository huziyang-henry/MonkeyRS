use std::mem::swap;
use crate::lexer::token::{Token, TokenType};

mod lexer;
mod parser;
mod object;
mod evaluator;

fn main() {
    let mut a = Token::new(TokenType::ASSIGN, "=");
    let mut b = Token::new(TokenType::PLUS, "+");
    println!("a: {:?}, b: {:?}", a, b);
    swap(&mut a, &mut b);
    println!("a: {:?}, b: {:?}", a, b);
    println!("Hello World");
}


