use std::fmt::Display;
use downcast_rs::Downcast;

pub trait Node: Display + Downcast {
    fn token_literal(&self) -> String;
}