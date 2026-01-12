use super::{Node, ParseError};
use crate::parser::{SourceIndex, parse_production};
use grammar::{ExpressionKind, Grammar, Production};
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
    let grammar = super::load_grammar();

    let (normalized_src, removed_indices) = normalize_crlf(original_src);

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

fn normalize_crlf(src: &str) -> (String, Vec<usize>) {
    let mut normalized_src = String::with_capacity(src.len());
    let mut removed_indices = Vec::new();
    let mut chars = src.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '\r' {
            if let Some(&'\n') = chars.peek() {
                removed_indices.push(normalized_src.len());
                continue;
            }
        }
        normalized_src.push(ch);
    }
    (normalized_src, removed_indices)
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

fn tokenize_normalized(grammar: &Grammar, src: &str) -> Result<Tokens, ParseError> {
    let mut tokens = Tokens::default();
    let top_prods = get_top_prods(grammar);

    let mut index = SourceIndex(0);
    // Remove BOM
    if src.starts_with('\u{FEFF}') {
        index.0 += 3;
    }

    index = parse_shebang(grammar, src, index, &mut tokens)?;
    index = parse_frontmatter(grammar, src, index, &mut tokens)?;
    parse_tokens(grammar, &top_prods, src, index, &mut tokens)?;
    validate_delimiters_balanced(&tokens, src)?;

    debug!("lexing complete");

    Ok(tokens)
}

fn get_top_prods(grammar: &Grammar) -> Vec<&Production> {
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
    top_prods
}

fn parse_shebang(
    grammar: &Grammar,
    src: &str,
    index: SourceIndex,
    tokens: &mut Tokens,
) -> Result<SourceIndex, ParseError> {
    let shebang = grammar.productions.get("SHEBANG").unwrap();
    if let Some((node, next_index)) = parse_production(&grammar, &shebang, &src, index)? {
        tokens.shebang = Some(node);
        Ok(next_index)
    } else {
        Ok(index)
    }
}

fn parse_frontmatter(
    grammar: &Grammar,
    src: &str,
    index: SourceIndex,
    tokens: &mut Tokens,
) -> Result<SourceIndex, ParseError> {
    let frontmatter = grammar.productions.get("FRONTMATTER").unwrap();
    if let Some((node, next_index)) = parse_production(&grammar, &frontmatter, &src, index)
        .map_err(|e| ParseError {
            message: format!("invalid frontmatter: {}", e.message),
            byte_offset: e.byte_offset,
        })?
    {
        tokens.frontmatter = Some(node);
        Ok(next_index)
    } else {
        let invalid_frontmatter = grammar.productions.get("INVALID_FRONTMATTER").unwrap();
        if let Some(_) = parse_production(&grammar, &invalid_frontmatter, &src, index)? {
            return Err(ParseError {
                message: "invalid frontmatter".to_string(),
                byte_offset: index.0,
            });
        }
        Ok(index)
    }
}

fn parse_tokens(
    grammar: &Grammar,
    top_prods: &[&Production],
    src: &str,
    mut index: SourceIndex,
    tokens: &mut Tokens,
) -> Result<(), ParseError> {
    let whitespace = &grammar.productions.get("WHITESPACE").unwrap();

    while index.0 < src.len() {
        if let Some((_node, next_index)) = parse_production(&grammar, whitespace, &src, index)? {
            index = next_index;
            continue;
        }

        let mut matched_token = None;
        for token_prod in top_prods {
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
            Some(mut node) => {
                normalize_line_doc(&mut node, src);
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
    Ok(())
}

fn validate_delimiters_balanced(tokens: &Tokens, src: &str) -> Result<(), ParseError> {
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
    Ok(())
}

/// Fix line doc comment range.
///
/// The Reference models line doc comments as *content* followed by a
/// linefeed. However, rustc and proc-macro2 model it as everything excluding
/// the linefeed. For convenience, this normalizes the range so that it
/// matches the other tools.
///
/// A real implementation using the Reference lexer would extract the content
/// from `LINE_DOC_COMMENT_CONTENT`, which does not include the linefeed.
fn normalize_line_doc(node: &mut Node, src: &str) {
    if matches!(node.name.as_str(), "INNER_LINE_DOC" | "OUTER_LINE_DOC")
        && src[node.range.clone()].ends_with('\n')
    {
        node.range.end -= 1;
    }
}
