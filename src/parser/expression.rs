use crate::parser::node::Node;

pub trait Expression: Node {
    fn expression_node(&self) -> Box<dyn Node>;
}