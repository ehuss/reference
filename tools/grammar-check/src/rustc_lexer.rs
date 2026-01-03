extern crate rustc_lexer;

use lexer::LexError;
use lexer::Token;
use std::ops::Range;
use rustc_lexer::TokenKind;
use rustc_lexer::FrontmatterAllowed;

pub fn tokenize(src: &str) -> Result<Vec<Token>, LexError> {
    let mut pos = 0;
    let ts: Vec<_> = rustc_lexer::tokenize(src, FrontmatterAllowed::Yes)
        .filter_map(|token| {
            let start = pos;
            let end = pos + token.len as usize;
            pos += token.len as usize;
            if matches!(token.kind, TokenKind::Whitespace) {
                return None;
            }
            let t = Token {
                name: format!("{:?}", token.kind),
                range: Range { start, end },
            };
            Some(t)
        })
        .collect();
    Ok(ts)
}
