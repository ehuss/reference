use lexer::LexError;
use lexer::Token;
use proc_macro2::Spacing;
use proc_macro2::TokenStream;
use proc_macro2::TokenTree;
use regex::Regex;
use std::ops::Range;
use std::str::FromStr;
use std::sync::LazyLock;

pub fn tokenize(src: &str) -> Result<Vec<Token>, LexError> {
    let mut tokens = Vec::new();
    let stream = TokenStream::from_str(src).map_err(|e| LexError {
        byte_offset: 0,
        message: e.to_string(),
    })?;
    tokens_from_ts(stream, &mut tokens)?;
    Ok(tokens)
}

// proc-macro2 does not reject literals starting with E.
// We'll need to do that to match behavior.
// https://github.com/dtolnay/proc-macro2/issues/506
static SUFFIX_NO_E: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(
        r"(?x)
        ^(
          ([0-9][0-9_]*[eE])  # DEC_LITERAL
        | (0b([01]|_)*?[01]([01]|_)*[eE]) # BIN_LITERL
        | (0o([0-7]|_)*?[0-7]([0-7]|_)*[eE]) # OCT_LITERAL
        | ([0-9]([0-9]|_)*\.[0-9]([0-9]|_)*[eE]) # FLOAT_LITERAL
        )
        ",
    )
    .unwrap()
});

static FLOAT_EXPONENT: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(
        r"(?x)
        ^
          [0-9]([0-9]|_)*
          (\. [0-9]([0-9]|_)*)?
          [eE] [+-]? ([0-9]|_)*? [0-9] ([0-9]|_)*
        ",
    )
    .unwrap()
});

fn tokens_from_ts(ts: TokenStream, output: &mut Vec<Token>) -> Result<(), LexError> {
    let trees: Vec<TokenTree> = ts.into_iter().collect();
    let mut i = 0;
    while i < trees.len() {
        let tt = &trees[i];
        let span = tt.span();
        let mut range = span.byte_range();

        match tt {
            TokenTree::Ident(_) => {
                output.push(Token {
                    name: format!("{tt:?}"),
                    range,
                });
                i += 1;
            }
            TokenTree::Punct(p) => {
                // In order to be consistent with rustc which uses joined tokens,
                // this looks to join multiple punctuation tokens.

                // s accumulates the punctuation string.
                let mut s = p.as_char().to_string();
                // j is the current index into the token trees.
                i += 1;
                let mut current_spacing = p.spacing();

                // Try to consume subsequent punctuation if it is joint and
                // forms a valid operator.
                while current_spacing == Spacing::Joint && i < trees.len() {
                    match &trees[i] {
                        TokenTree::Punct(next_p) => {
                            s.push(next_p.as_char());
                            if is_valid_punctuation(&s) {
                                range.end = next_p.span().byte_range().end;
                                current_spacing = next_p.spacing();
                                i += 1;
                            } else {
                                s.pop();
                                break;
                            }
                        }
                        TokenTree::Ident(ident) => {
                            // lifetime
                            s.push_str(&ident.to_string());
                            range.end = ident.span().byte_range().end;
                            i += 1;
                            break;
                        }
                        _ => break,
                    }
                }

                output.push(Token { name: s, range });
            }
            TokenTree::Literal(lit) => {
                let s = lit.to_string();
                if SUFFIX_NO_E.is_match(&s) {
                    if !FLOAT_EXPONENT.is_match(&s) {
                        return Err(LexError {
                            message: "bad E suffix".to_string(),
                            byte_offset: range.start,
                        });
                    }
                }
                output.push(Token {
                    name: format!("{tt:?}"),
                    range,
                });
                i += 1;
            }
            TokenTree::Group(group) => {
                let delim = group.delimiter();
                let delim_str = match &delim {
                    proc_macro2::Delimiter::Parenthesis => "(",
                    proc_macro2::Delimiter::Brace => "{",
                    proc_macro2::Delimiter::Bracket => "[",
                    proc_macro2::Delimiter::None => "",
                };
                if !delim_str.is_empty() {
                    output.push(Token {
                        name: delim_str.to_string(),
                        range: group.span_open().byte_range(),
                    });
                }
                tokens_from_ts(group.stream(), output)?;
                if !delim_str.is_empty() {
                    let close_delim = match delim_str {
                        "(" => ")",
                        "{" => "}",
                        "[" => "]",
                        _ => "",
                    };
                    output.push(Token {
                        name: close_delim.to_string(),
                        range: group.span_close().byte_range(),
                    });
                }
                i += 1;
            }
        }
    }
    Ok(())
}

fn is_valid_punctuation(s: &str) -> bool {
    matches!(
        s,
        "..."
            | "..="
            | "<<="
            | ">>="
            | "!="
            | "%="
            | "&&"
            | "&="
            | "*="
            | "+="
            | "-="
            | "->"
            | ".."
            | "/="
            | "::"
            | "<-"
            | "<<"
            | "<="
            | "=="
            | "=>"
            | ">="
            | ">>"
            | "^="
            | "|="
            | "||"
    )
}

pub fn normalize(tokens: &[Token], src: &str) -> Vec<Token> {
    tokens
        .iter()
        .filter(|token| !matches!(token.name.as_str(), "LINE_COMMENT" | "BLOCK_COMMENT"))
        .cloned()
        .fold(Vec::with_capacity(tokens.len()), |mut acc, token| {
            // proc-macro2 does not handle ## reserved tokens (treats them as individual punctuation)
            if token.name == "RESERVED_TOKEN" && src[token.range.clone()].chars().all(|c| c == '#')
            {
                let count = token.range.len();
                for i in 0..count {
                    acc.push(Token {
                        name: "#".to_string(),
                        range: Range {
                            start: token.range.start + i,
                            end: token.range.start + i + 1,
                        },
                    });
                }
                return acc;
            }
            // proc-macro2 converts doc comments into doc attributes.
            match &*token.name {
                "OUTER_LINE_DOC" | "INNER_LINE_DOC" | "OUTER_BLOCK_DOC" | "INNER_BLOCK_DOC" => {
                    acc.push(token.clone()); // #
                    if token.name.starts_with("INNER") {
                        acc.push(token.clone()); // !
                    }
                    acc.push(Token {
                        name: String::from("["),
                        range: Range {
                            start: token.range.start,
                            end: token.range.start + 1,
                        },
                    });
                    acc.push(token.clone()); // ident
                    acc.push(token.clone()); // =
                    acc.push(token.clone()); // literal
                    acc.push(Token {
                        name: String::from("]"),
                        range: Range {
                            start: token.range.end
                                - src[..token.range.end]
                                    .chars()
                                    .next_back()
                                    .unwrap()
                                    .len_utf8(),
                            end: token.range.end,
                        },
                    });
                }
                // acc.extend(std::iter::repeat(token).take(7)),
                _ => acc.push(token),
            }
            acc
        })
}
