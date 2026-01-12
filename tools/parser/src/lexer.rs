use super::{Node, ParseError};
use crate::parser::{SourceIndex, parse_production};
use diagnostics::Diagnostics;
use grammar::{ExpressionKind, Grammar};
// use std::ops::Range;
use tracing::debug;

#[derive(Default)]
pub struct Tokens {
    pub tokens: Vec<Node>,
    /// Byte range of the shebang.
    ///
    /// The reference lexer is the only tool that sets this.
    pub shebang: Option<Node>,
    /// Byte range of the frontmatter.
    ///
    /// The reference lexer is the only tool that sets this.
    pub frontmatter: Option<Node>,
}

pub fn tokenize(original_src: &str) -> Result<Tokens, ParseError> {
    let mut diag = Diagnostics::new();
    let mut grammar = grammar::load_grammar(&mut diag);
    super::remove_breaks(&mut grammar);

    let mut normalized_src = String::with_capacity(original_src.len());
    let mut removed_indices = Vec::new();
    let mut chars = original_src.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '\r' {
            if let Some(&'\n') = chars.peek() {
                removed_indices.push(normalized_src.len());
                continue;
            }
        }
        normalized_src.push(ch);
    }
    fn map_offset(removed_indices: &[usize], offset: usize) -> usize {
        offset + removed_indices.partition_point(|&x| x < offset)
    }
    fn adjust_node(removed_indices: &[usize], node: &mut Node) {
        node.range.start = map_offset(removed_indices, node.range.start);
        node.range.end = map_offset(removed_indices, node.range.end);
        for child in &mut node.children.0 {
            adjust_node(removed_indices, child);
        }
    }

    tokenize_normalized(&grammar, &normalized_src)
        .map(|mut tokens| {
            for token in &mut tokens.tokens {
                adjust_node(&removed_indices, token);
            }
            if let Some(shebang) = &mut tokens.shebang {
                adjust_node(&removed_indices, shebang);
            }
            if let Some(frontmatter) = &mut tokens.frontmatter {
                adjust_node(&removed_indices, frontmatter);
            }
            tokens
        })
        .map_err(|mut err| {
            err.byte_offset = map_offset(&removed_indices, err.byte_offset);
            err
        })
}

fn tokenize_normalized(grammar: &Grammar, src: &str) -> Result<Tokens, ParseError> {
    let mut tokens = Tokens::default();
    let mut top_prods = Vec::new();
    let comment = grammar.productions.get("COMMENT").unwrap();
    let ExpressionKind::Alt(es) = &comment.expression.kind else {
        panic!("expected alts");
    };
    for e in es {
        let nt = match &e.kind {
            ExpressionKind::Sequence(es) => {
                let seq: Vec<_> = es
                    .iter()
                    .filter_map(|e| match &e.kind {
                        ExpressionKind::Nt(nt) => Some(nt),
                        kind => panic!("unexpected kind {kind:?}"),
                    })
                    .collect();
                assert_eq!(seq.len(), 1);
                seq[0]
            }
            ExpressionKind::Nt(nt) => nt,
            kind => panic!("unexpected kind {kind:?}"),
        };
        top_prods.push(grammar.productions.get(nt).unwrap());
    }

    // let mut top_prods: Vec<_> = [
    //     "LINE_COMMENT",
    //     "BLOCK_COMMENT",
    //     "OUTER_BLOCK_DOC",
    //     "INNER_LINE_DOC",
    //     "INNER_BLOCK_DOC",
    //     "OUTER_LINE_DOC",
    // ]
    // .into_iter()
    // .map(|comment| grammar.productions.get(comment).unwrap())
    // .collect();

    // Collect the expressions from the Token alternation.
    let token = grammar.productions.get("Token").unwrap();
    let ExpressionKind::Alt(es) = &token.expression.kind else {
        panic!("expected alts");
    };
    for e in es {
        let nt = match &e.kind {
            ExpressionKind::Sequence(es) => {
                let seq: Vec<_> = es
                    .iter()
                    .filter_map(|e| match &e.kind {
                        ExpressionKind::Nt(nt) => Some(nt),
                        kind => panic!("unexpected kind {kind:?}"),
                    })
                    .collect();
                assert_eq!(seq.len(), 1);
                seq[0]
            }
            ExpressionKind::Nt(nt) => nt,
            kind => panic!("unexpected kind {kind:?}"),
        };
        top_prods.push(grammar.productions.get(nt).unwrap());
    }

    let whitespace = &grammar.productions.get("WHITESPACE").unwrap();

    let mut index = SourceIndex(0);
    // Remove BOM
    if src.starts_with('\u{FEFF}') {
        index.0 += 3;
    }

    let shebang = grammar.productions.get("SHEBANG").unwrap();
    if let Some((node, next_index)) = parse_production(&grammar, &shebang, &src, index)? {
        index = next_index;
        tokens.shebang = Some(node);
    }

    let frontmatter = grammar.productions.get("FRONTMATTER").unwrap();
    if let Some((node, next_index)) = parse_production(&grammar, &frontmatter, &src, index)
        .map_err(|e| ParseError {
            message: format!("invalid frontmatter: {}", e.message),
            byte_offset: e.byte_offset,
        })?
    {
        index = next_index;
        tokens.frontmatter = Some(node);
    } else {
        let invalid_frontmatter = grammar.productions.get("INVALID_FRONTMATTER").unwrap();
        if let Some(_) = parse_production(&grammar, &invalid_frontmatter, &src, index)? {
            return Err(ParseError {
                message: "invalid frontmatter".to_string(),
                byte_offset: index.0,
            });
        }
    }

    while index.0 < src.len() {
        if let Some((_node, next_index)) = parse_production(&grammar, whitespace, &src, index)? {
            index = next_index;
            continue;
        }

        let mut matched_token = None;
        for token_prod in &top_prods {
            debug!("try top-level token `{}`", token_prod.name);
            match parse_production(&grammar, &token_prod, &src, index)? {
                Some((node, next_index)) => {
                    if node.byte_len() > 0 {
                        index = next_index;
                        matched_token = Some(node);
                        break;
                    }
                }
                None => {}
            }
        }

        match matched_token {
            Some(node) => {
                tokens.tokens.push(node);
            }
            None => {
                return Err(ParseError {
                    byte_offset: index.0,
                    message: String::from("no tokens matched"),
                });
            }
        }
    }

    let mut stack = Vec::new();
    for token in &tokens.tokens {
        let text = &src[token.range.clone()];
        match text {
            "(" | "[" | "{" => stack.push((text, token.range.start)),
            ")" => {
                if stack.pop().map(|(s, _)| s) != Some("(") {
                    return Err(ParseError {
                        byte_offset: token.range.start,
                        message: "unbalanced `)`".to_string(),
                    });
                }
            }
            "]" => {
                if stack.pop().map(|(s, _)| s) != Some("[") {
                    return Err(ParseError {
                        byte_offset: token.range.start,
                        message: "unbalanced `]`".to_string(),
                    });
                }
            }
            "}" => {
                if stack.pop().map(|(s, _)| s) != Some("{") {
                    return Err(ParseError {
                        byte_offset: token.range.start,
                        message: "unbalanced `}`".to_string(),
                    });
                }
            }
            _ => {}
        }
    }
    if let Some((_, offset)) = stack.pop() {
        return Err(ParseError {
            byte_offset: offset,
            message: "unclosed delimiter".to_string(),
        });
    }

    debug!("lexing complete");

    Ok(tokens)
}
