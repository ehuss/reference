#![allow(unused)]
use super::{Node, ParseError};
use crate::lexer::Token;
use crate::parser::Source;
use crate::parser::parse_expression;
use diagnostics::Diagnostics;
use std::ops::Range;

struct TokenSource<'src> {
    src: &'src str,
    tokens: Vec<Token>,
}

impl Source for TokenSource<'_> {
    fn get_substring(&self, offset: usize, len: usize) -> Option<&str> {
        self.tokens.get(offset).and_then(|t| {
            let s = &self.src[t.range.clone()];
            if !s.len() == len { None } else { Some(s) }
        })
    }

    fn get_next(&self, offset: usize) -> Option<&str> {
        self.tokens.get(offset).map(|t| &self.src[t.range.clone()])
    }

    fn len(&self) -> usize {
        self.tokens.len()
    }
}

pub fn parse_crate(src: &str) -> Result<Node, ParseError> {
    let mut diag = Diagnostics::new();
    let mut grammar = grammar::load_grammar(&mut diag);
    super::remove_breaks(&mut grammar);

    todo!();
    // let krate = grammar.get
}
