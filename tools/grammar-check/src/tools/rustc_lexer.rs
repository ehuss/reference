extern crate rustc_lexer;

use parser::{Node, ParseError};
use rustc_lexer::{FrontmatterAllowed, TokenKind};
use std::ops::Range;

pub fn tokenize(src: &str) -> Result<Vec<Node>, ParseError> {
    let mut pos = 0;
    let ts: Vec<_> = rustc_lexer::tokenize(src, FrontmatterAllowed::Yes)
        .filter_map(|token| {
            let start = pos;
            let end = pos + token.len as usize;
            pos += token.len as usize;
            if matches!(token.kind, TokenKind::Whitespace) {
                return None;
            }
            let t = Node::new(format!("{:?}", token.kind), Range { start, end });
            Some(t)
        })
        .collect();
    Ok(ts)
}
