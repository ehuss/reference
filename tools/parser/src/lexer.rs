use super::ParseError;
use crate::parser::parse_expression;
use diagnostics::Diagnostics;
use grammar::ExpressionKind;
use std::ops::Range;
use tracing::debug;

#[derive(Debug, Clone)]
pub struct Token {
    pub name: String,
    pub range: Range<usize>,
}

#[derive(Default)]
pub struct Tokens {
    pub tokens: Vec<Token>,
    /// Byte range of the shebang.
    ///
    /// The reference lexer is the only tool that sets this.
    pub shebang: Option<Range<usize>>,
    /// Byte range of the frontmatter.
    ///
    /// The reference lexer is the only tool that sets this.
    pub frontmatter: Option<Range<usize>>,
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
    let src = &*normalized_src;
    let map_offset =
        |offset: usize| -> usize { offset + removed_indices.partition_point(|&x| x < offset) };

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

    let whitespace = &grammar.productions.get("WHITESPACE").unwrap().expression;

    let mut index = 0;
    // Remove BOM
    if normalized_src.starts_with('\u{FEFF}') {
        index += 3;
    }

    let shebang = grammar.productions.get("SHEBANG").unwrap();
    if let Some(i) =
        parse_expression(&grammar, &shebang.expression, &src, index).map_err(|mut e| {
            e.byte_offset = map_offset(e.byte_offset);
            e
        })?
    {
        tokens.shebang = Some(Range {
            start: map_offset(index),
            end: map_offset(index + i),
        });
        index += i;
    }

    let frontmatter = grammar.productions.get("FRONTMATTER").unwrap();
    if let Some(i) =
        parse_expression(&grammar, &frontmatter.expression, &src, index).map_err(|mut e| {
            e.byte_offset = map_offset(e.byte_offset);
            ParseError {
                message: format!("invalid frontmatter: {}", e.message),
                byte_offset: e.byte_offset,
            }
        })?
    {
        tokens.frontmatter = Some(Range {
            start: map_offset(index),
            end: map_offset(index + i),
        });
        index += i;
    } else {
        let invalid_frontmatter = grammar.productions.get("INVALID_FRONTMATTER").unwrap();
        if let Some(_) = parse_expression(&grammar, &invalid_frontmatter.expression, &src, index)
            .map_err(|mut e| {
                e.byte_offset = map_offset(e.byte_offset);
                e
            })?
        {
            return Err(ParseError {
                message: "invalid frontmatter".to_string(),
                byte_offset: index,
            });
        }
    }

    while index < src.len() {
        if let Some(i) = parse_expression(&grammar, whitespace, &src, index).map_err(|mut e| {
            e.byte_offset = map_offset(e.byte_offset);
            e
        })? {
            index += i;
            continue;
        }

        let mut matched_token = None;
        for token_prod in &top_prods {
            debug!("try top-level token `{}`", token_prod.name);
            match parse_expression(&grammar, &token_prod.expression, &src, index).map_err(
                |mut e| {
                    e.byte_offset = map_offset(e.byte_offset);
                    e
                },
            )? {
                Some(l) => {
                    if l > 0 {
                        matched_token = Some((
                            l,
                            Token {
                                name: token_prod.name.clone(),
                                range: Range {
                                    start: map_offset(index),
                                    end: map_offset(index + l),
                                },
                            },
                        ));
                        break;
                    }
                }
                None => {}
            }
        }

        match matched_token {
            Some((l, t)) => {
                index += l;
                tokens.tokens.push(t);
            }
            None => {
                return Err(ParseError {
                    byte_offset: map_offset(index),
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

    Ok(tokens)
}
