use super::{Node, ParseError};
use crate::lexer::tokenize;
use crate::parser::parse_production;
use crate::parser::{Source, SourceIndex};
use std::ops::Range;

struct TokenSource<'src> {
    src: &'src str,
    tokens: Vec<Node>,
}

impl Source for TokenSource<'_> {
    fn get_substring(&self, offset: SourceIndex, bytes: usize) -> Option<&str> {
        self.tokens.get(offset.0).and_then(|t| {
            let s = &self.src[t.range.clone()];
            if !s.len() == bytes { None } else { Some(s) }
        })
    }

    fn get_next(&self, offset: SourceIndex) -> Option<&str> {
        self.tokens
            .get(offset.0)
            .map(|t| &self.src[t.range.clone()])
    }

    fn len(&self) -> SourceIndex {
        SourceIndex(self.tokens.len())
    }

    fn advance(&self, index: SourceIndex, bytes: usize) -> SourceIndex {
        let token = &self.tokens[index.0];
        if token.byte_len() != bytes {
            panic!("advancing {bytes} at {index:?} is not equal to {token:?}");
        }
        SourceIndex(index.0 + 1)
    }

    fn index_to_range(&self, index: SourceIndex, bytes: usize) -> Range<usize> {
        let start = self.tokens[index.0].range.start;
        Range {
            start,
            end: start + bytes,
        }
    }

    fn get_node(&self, index: SourceIndex) -> Option<&Node> {
        self.tokens.get(index.0)
    }

    fn index_to_bytes(&self, index: SourceIndex) -> usize {
        if index.0 == self.tokens.len() {
            self.tokens[index.0 - 1].range.end
        } else {
            self.tokens[index.0].range.start
        }
    }
}

pub fn parse(src: &str, production: &str) -> Result<Node, ParseError> {
    let grammar = super::load_grammar();

    let krate = grammar.productions.get(production).unwrap();

    let tokens = tokenize(src)?;

    let tokens = tokens
        .tokens
        .into_iter()
        .filter(|token| !matches!(token.name.as_str(), "LINE_COMMENT" | "BLOCK_COMMENT"))
        .collect();

    let token_source = TokenSource { src, tokens };

    match parse_production(&grammar, &krate, &token_source, SourceIndex(0))? {
        Some((node, next_index)) => {
            if next_index < token_source.len() {
                return Err(ParseError {
                    message: format!("Crate production failed to parse all tokens"),
                    byte_offset: token_source.index_to_bytes(next_index),
                });
            }
            Ok(node)
        }
        None => panic!("input did not match {production}"),
    }
}
