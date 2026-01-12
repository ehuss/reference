use super::{Node, Nodes, ParseError};
use grammar::{Characters, Expression, ExpressionKind, Grammar, Production, RangeLimit};
use std::collections::HashMap;
use std::ops::Range;
use tracing::instrument;

/// This stores named repetitions.
///
/// The key is the name, and the value is the number of repetitions that
/// happened.
#[derive(Debug, Default)]
struct Environment {
    map: HashMap<String, u32>,
}

/// A wrapper around an index for referring to elements in a [`Source`].
#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
pub(crate) struct SourceIndex(pub(crate) usize);

/// Abstracts different kinds of sources for the parser.
///
/// This allows the parser to be used for both string sources and tokenized
/// sources. String sources work in elements of bytes of a string, whereas
/// token sources work in elements of tokens. The offsets are based on
/// elements in the sequence represented with [`SourceIndex`].
pub(crate) trait Source {
    /// Returns a substring from the given offset of the given length in bytes.
    ///
    /// If this does not match an entire token, it returns None.
    fn get_substring(&self, offset: SourceIndex, bytes: usize) -> Option<(&str, Range<usize>)>;

    /// Returns the next element at the given offset.
    fn get_next(&self, offset: SourceIndex) -> Option<(&str, Range<usize>)>;

    /// Returns the number of elements in the source.
    fn len(&self) -> SourceIndex;

    /// Returns what the next index should be when advanced from the current
    /// index with the given number of bytes.
    fn advance(&self, index: SourceIndex, bytes: usize) -> SourceIndex;

    /// If this is a token source, returns the node at the given index.
    ///
    /// Returns `None` if past the end of the input.
    ///
    /// This is essentially a hack to create a boundary between the lexer and
    /// the tree parser.
    fn get_node(&self, index: SourceIndex) -> Option<&Node>;

    /// Returns the byte offset of the start of the given element.
    ///
    /// When the index is at the end, returns the offset of the last element.
    fn index_to_bytes(&self, index: SourceIndex) -> usize;
}

impl<'a> Source for &'a str {
    fn get_substring(&self, offset: SourceIndex, bytes: usize) -> Option<(&str, Range<usize>)> {
        let end = offset.0.checked_add(bytes)?;
        if end > (*self).len() {
            return None;
        }
        if !self.is_char_boundary(offset.0) || !self.is_char_boundary(end) {
            return None;
        }
        let s = &self[offset.0..end];
        let range = Range {
            start: offset.0,
            end,
        };
        Some((s, range))
    }

    fn get_next(&self, offset: SourceIndex) -> Option<(&str, Range<usize>)> {
        let ch = self[offset.0..].chars().next()?;
        let len = ch.len_utf8();
        let s = &self[offset.0..offset.0 + len];
        let range = Range {
            start: offset.0,
            end: offset.0 + len,
        };
        Some((s, range))
    }

    fn len(&self) -> SourceIndex {
        SourceIndex((*self).len())
    }

    fn advance(&self, index: SourceIndex, bytes: usize) -> SourceIndex {
        SourceIndex(index.0 + bytes)
    }

    fn get_node(&self, _index: SourceIndex) -> Option<&Node> {
        None
    }

    fn index_to_bytes(&self, index: SourceIndex) -> usize {
        index.0
    }
}

/// Parse a production and return the Node with name from the production.
pub(crate) fn parse_production(
    grammar: &Grammar,
    prod: &Production,
    src: &dyn Source,
    index: SourceIndex,
) -> Result<Option<(Node, SourceIndex)>, ParseError> {
    let r = parse(
        grammar,
        &prod.expression,
        src,
        index,
        &mut Environment::default(),
    )?
    .map(|(children, next_index)| {
        let children = Node::with_children(prod.name.clone(), src.index_to_bytes(index), children);
        (children, next_index)
    });
    Ok(r)
}

/// Parse an expression.
///
/// Returns `Ok(None)` if the expression does not match. Otherwise, it
/// returns the [`Nodes`] that match, along with the new index pointing
/// just after the matched nodes.
///
/// Note that some expressions match zero elements (like `e*` when `e` doesn't
/// match), and those are treated as a successful match where `Nodes` is
/// empty.
///
/// Returns `Err` if there is some kind of syntax error.
#[instrument(level = "debug", skip(grammar, e, src), ret)]
fn parse(
    grammar: &Grammar,
    e: &Expression,
    src: &dyn Source,
    index: SourceIndex,
    env: &mut Environment,
) -> Result<Option<(Nodes, SourceIndex)>, ParseError> {
    tracing::debug!("e={e}");
    if index < src.len() {
        tracing::debug!("next={:?}", src.get_next(index));
    } else {
        tracing::debug!("eof");
    }
    match &e.kind {
        ExpressionKind::Grouped(group) => {
            assert_eq!(e.suffix, None);
            let r = parse(grammar, group, src, index, env)?.map(|(nodes, i)| {
                (
                    nodes.wrap(format!("Group({group})"), src.index_to_bytes(index)),
                    i,
                )
            });
            Ok(r)
        }
        ExpressionKind::Alt(es) => {
            assert_eq!(e.suffix, None);
            for e in es {
                if let Some(r) = parse(grammar, e, src, index, env)? {
                    return Ok(Some(r));
                }
            }
            Ok(None)
        }
        ExpressionKind::Sequence(es) => {
            assert_eq!(e.suffix, None);
            let mut current = index;
            let mut children = Vec::new();
            for e in es {
                match parse(grammar, e, src, current, env)? {
                    Some((nodes, next_index)) => {
                        current = next_index;
                        children.extend(nodes.0);
                    }
                    None => return Ok(None),
                }
            }
            Ok(Some((Nodes(children), current)))
        }
        ExpressionKind::Optional(opt) => {
            assert_eq!(e.suffix, None);
            match parse(grammar, opt, src, index, env)? {
                Some((children, next_index)) => Ok(Some((
                    children.wrap(format!("Optional({opt})"), src.index_to_bytes(index)),
                    next_index,
                ))),
                None => Ok(Some((Nodes::default(), index))),
            }
        }
        ExpressionKind::Not(n) => {
            assert_eq!(e.suffix, None);
            match parse(grammar, n, src, index, env)? {
                Some(_) => Ok(None),
                None => Ok(Some((Nodes::default(), index))),
            }
        }
        ExpressionKind::Repeat(r) => {
            assert_eq!(e.suffix, None);
            let mut current = index;
            let mut children = Nodes::default();
            while current < src.len() {
                match parse(grammar, r, src, current, env)? {
                    Some((nodes, next_index)) => {
                        current = next_index;
                        children.extend(nodes);
                    }
                    None => break,
                }
            }
            Ok(Some((
                children.wrap(format!("Repeat({r})"), src.index_to_bytes(index)),
                current,
            )))
        }
        ExpressionKind::RepeatPlus(r) => {
            assert_eq!(e.suffix, None);
            let mut current = index;
            let mut children = Nodes::default();
            while current < src.len() {
                match parse(grammar, r, src, current, env)? {
                    Some((nodes, next_index)) => {
                        current = next_index;
                        children.extend(nodes);
                    }
                    None => break,
                }
            }
            if current == index {
                Ok(None)
            } else {
                Ok(Some((
                    children.wrap(format!("RepeatPlus({r})"), src.index_to_bytes(index)),
                    current,
                )))
            }
        }
        ExpressionKind::RepeatRange {
            expr: r,
            name,
            min,
            max,
            limit,
        } => {
            let max = max.map(|max| match limit {
                RangeLimit::HalfOpen => max - 1,
                RangeLimit::Closed => max,
            });
            let mut current = index;
            let mut children = Nodes::default();
            let mut count = 0;
            while current < src.len() {
                match parse(grammar, r, src, current, env)? {
                    Some((nodes, next_index)) => {
                        current = next_index;
                        children.extend(nodes);
                        if let Some(max) = max {
                            if count + 1 == max {
                                break;
                            }
                        }
                        count += 1;
                    }
                    None => break,
                }
            }
            if let Some(min) = min {
                if count < *min {
                    return Ok(None);
                }
            }
            if let Some(name) = name {
                assert!(env.map.insert(name.clone(), count).is_none());
            }

            let start_byte_offset = src.index_to_bytes(index);
            match e.suffix.as_deref() {
                Some("valid hex char value") => {
                    let end = src.index_to_bytes(current);
                    let len = end - start_byte_offset;
                    let (hex, _) = src.get_substring(index, len).unwrap();
                    let hex_no_underscores = hex.replace('_', "");
                    let value =
                        u32::from_str_radix(&hex_no_underscores, 16).map_err(|_| ParseError {
                            byte_offset: start_byte_offset,
                            message: format!("invalid hex value: {hex}"),
                        })?;
                    if char::from_u32(value).is_none() {
                        return Err(ParseError {
                            byte_offset: start_byte_offset,
                            message: format!("invalid Unicode scalar value: {hex}"),
                        });
                    }
                }
                Some(s) => panic!("unknown suffix {s:?}"),
                None => {}
            }

            Ok(Some((
                children.wrap(format!("RepatRange({r})"), start_byte_offset),
                current,
            )))
        }
        ExpressionKind::RepeatRangeNamed(r, name) => {
            assert_eq!(e.suffix, None);
            let Some(count) = env.map.get(name) else {
                panic!("expected {name} in environment for {r}");
            };
            let mut current = index;
            let mut children = Nodes::default();
            for _ in 0..*count {
                match parse(grammar, r, src, current, env)? {
                    Some((nodes, next_index)) => {
                        current = next_index;
                        children.extend(nodes);
                    }
                    None => return Ok(None),
                }
            }
            Ok(Some((
                children.wrap(
                    format!("RepeatRangeNamed({r}, {name})"),
                    src.index_to_bytes(index),
                ),
                current,
            )))
        }
        ExpressionKind::Nt(s) => {
            let Some((nodes, next_index)) = parse_nt(grammar, s, src, index, env)? else {
                return Ok(None);
            };
            let len = nodes.byte_len();
            let (matched, _) = src.get_substring(index, len).unwrap();
            match e.suffix.as_deref() {
                Some("except `_`") => {
                    if matched == "_" {
                        return Ok(None);
                    }
                }
                Some("except `b` or `c` or `r` or `br` or `cr`") => {
                    if matches!(matched, "b" | "c" | "r" | "br" | "cr") {
                        return Ok(None);
                    }
                }
                Some("except `b`") => {
                    if matched == "b" {
                        return Ok(None);
                    }
                }
                Some("except `r` or `br` or `cr`") => {
                    if matches!(matched, "r" | "br" | "cr") {
                        return Ok(None);
                    }
                }
                Some("except `r`") => {
                    if matched == "r" {
                        return Ok(None);
                    }
                }
                Some("not beginning with `e` or `E`") => {
                    if matched.starts_with('e') || matched.starts_with('E') {
                        return Ok(None);
                    }
                }
                Some(
                    "except a [strict][lex.keywords.strict] or [reserved][lex.keywords.reserved] keyword",
                ) => {
                    let strict = grammar.productions.get("STRICT_KEYWORD").unwrap();
                    let reserved = grammar.productions.get("RESERVED_KEYWORD").unwrap();
                    for e in [&strict.expression, &reserved.expression] {
                        if let Ok(Some((nodes, _))) = parse(grammar, e, src, index, env)
                            && nodes.byte_len() > 0
                        {
                            return Ok(None);
                        }
                    }
                }
                Some("except [delimiters][lex.token.delim]") => {
                    if matches!(matched, "{" | "}" | "[" | "]" | "(" | ")") {
                        return Ok(None);
                    }
                }
                Some(suffix) => panic!("unknown suffix {suffix:?}"),
                None => {}
            }
            Ok(Some((nodes, next_index)))
        }
        ExpressionKind::Terminal(s) => {
            let Some((next_s, range)) = src.get_substring(index, s.len()) else {
                return Ok(None);
            };
            if next_s != s {
                return Ok(None);
            }
            let next_index = src.advance(index, s.len());
            match e.suffix.as_deref() {
                Some("immediately followed by LF") => {
                    if let Some((next_s, _)) = src.get_next(next_index)
                        && next_s != "\n"
                    {
                        return Ok(None);
                    }
                }
                Some(suffix) => panic!("unknown suffix {suffix:?}"),
                None => {}
            }
            let node = Node {
                name: format!("Terminal {s:?}"),
                range,
                children: Nodes::default(),
            };
            Ok(Some((Nodes(vec![node]), next_index)))
        }
        ExpressionKind::Prose(s) => {
            assert_eq!(e.suffix, None);
            match_prose(s, src, index)
        }
        ExpressionKind::Break(_) => unreachable!(),
        ExpressionKind::Comment(_) => Ok(Some((Nodes::default(), index))),
        ExpressionKind::Charset(chars) => {
            assert_eq!(e.suffix, None);
            if index >= src.len() {
                return Ok(None);
            }
            for ch in chars {
                match ch {
                    Characters::Named(name) => {
                        if let Some((nodes, next_index)) = parse_nt(grammar, name, src, index, env)?
                        {
                            return Ok(Some((nodes, next_index)));
                        }
                    }
                    Characters::Terminal(s) => {
                        if let Some((next_s, range)) = src.get_substring(index, s.len())
                            && next_s == s
                        {
                            let next_index = src.advance(index, s.len());
                            let node = Node {
                                name: format!("Terminal {s:?}"),
                                range,
                                children: Nodes::default(),
                            };
                            return Ok(Some((Nodes(vec![node]), next_index)));
                        }
                    }
                    Characters::Range(a, b) => {
                        let (next, range) = src.get_next(index).unwrap();
                        if next.chars().count() == 1 {
                            let ch = next.chars().next().unwrap();
                            if ch >= *a && ch <= *b {
                                let next_index = src.advance(index, ch.len_utf8());
                                let node = Node {
                                    name: format!("Range {a:?} to {b:?}"),
                                    range,
                                    children: Nodes::default(),
                                };
                                return Ok(Some((Nodes(vec![node]), next_index)));
                            }
                        }
                    }
                }
            }
            Ok(None)
        }
        ExpressionKind::NegExpression(neg) => {
            assert_eq!(e.suffix, None);
            match parse(grammar, neg, src, index, env)? {
                Some(_) => Ok(None),
                None => {
                    if let Some((s, range)) = src.get_next(index) {
                        let next_index = src.advance(index, s.len());
                        let node = Node {
                            name: format!("NegExpression {neg}"),
                            range,
                            children: Nodes::default(),
                        };
                        Ok(Some((Nodes(vec![node]), next_index)))
                    } else {
                        Ok(None)
                    }
                }
            }
        }
        ExpressionKind::Cut(inner) => {
            assert_eq!(e.suffix, None);
            match parse(grammar, inner, src, index, env)? {
                Some(r) => Ok(Some(r)),
                None => Err(ParseError {
                    byte_offset: src.index_to_bytes(index),
                    message: format!("expected {}", inner),
                }),
            }
        }
        ExpressionKind::Unicode(s) => {
            assert_eq!(e.suffix, None);
            let c = char::from_u32(u32::from_str_radix(s, 16).unwrap()).unwrap();
            let mut buf = [0u8; 4];
            let c_str = c.encode_utf8(&mut buf);
            if let Some((next_s, range)) = src.get_next(index)
                && next_s == c_str
            {
                let next_index = src.advance(index, c.len_utf8());
                Ok(Some((
                    Nodes::new(format!("Unicode {s}"), range),
                    next_index,
                )))
            } else {
                Ok(None)
            }
        }
    }
}

fn parse_nt(
    grammar: &Grammar,
    prod_name: &str,
    src: &dyn Source,
    index: SourceIndex,
    env: &mut Environment,
) -> Result<Option<(Nodes, SourceIndex)>, ParseError> {
    let prod = grammar.productions.get(prod_name).unwrap();
    // If this matches a lexer token, don't parse it and use the token
    // directly. The lexer rules are incompatible when reading tokens.
    let (nodes, next_index) = if let Some(node) = src.get_node(index)
        && node.name == prod.name
    {
        (Nodes(vec![node.clone()]), SourceIndex(index.0 + 1))
    } else {
        let nodes = parse(grammar, &prod.expression, src, index, env)?;
        let Some((nodes, next_index)) = nodes else {
            return Ok(None);
        };
        (
            nodes.wrap(prod.name.clone(), src.index_to_bytes(index)),
            next_index,
        )
    };
    Ok(Some((nodes, next_index)))
}

fn match_prose(
    prose: &str,
    src: &dyn Source,
    index: SourceIndex,
) -> Result<Option<(Nodes, SourceIndex)>, ParseError> {
    let next_as_ch = || {
        src.get_next(index).and_then(|(next, range)| {
            let mut chars = next.chars();
            let ch = chars.next().unwrap();
            if chars.next().is_some() {
                None
            } else {
                Some((ch, range))
            }
        })
    };

    let ascii_but = |except: &dyn Fn(char) -> bool| {
        if let Some((ch, range)) = next_as_ch() {
            Ok((ch >= '\0' && ch <= '\x7f' && !except(ch)).then(|| {
                let nodes = Nodes::new(format!("Prose {prose}"), range);
                let next = src.advance(index, 1);
                (nodes, next)
            }))
        } else {
            Ok(None)
        }
    };

    match prose {
        "`XID_Start` defined by Unicode" => {
            if let Some((ch, range)) = next_as_ch() {
                Ok(unicode_ident::is_xid_start(ch).then(|| {
                    let nodes = Nodes::new(format!("Prose: {prose}"), range);
                    (nodes, src.advance(index, ch.len_utf8()))
                }))
            } else {
                Ok(None)
            }
        }
        "`XID_Continue` defined by Unicode" => {
            if let Some((ch, range)) = next_as_ch() {
                Ok(unicode_ident::is_xid_continue(ch).then(|| {
                    let nodes = Nodes::new(format!("Prose: {prose}"), range);
                    (nodes, src.advance(index, ch.len_utf8()))
                }))
            } else {
                Ok(None)
            }
        }
        "any ASCII (i.e. 0x00 to 0x7F) except CR" => ascii_but(&|ch| ch == '\r'),
        "any ASCII (i.e. 0x00 to 0x7F) except `'`, `\\`, LF, CR, or TAB" => {
            ascii_but(&|ch| matches!(ch, '\'' | '\\' | '\n' | '\r' | '\t'))
        }
        "any ASCII (i.e 0x00 to 0x7F) except `\"`, `\\`, or CR" => {
            ascii_but(&|ch| matches!(ch, '\"' | '\\' | '\r'))
        }
        "a Unicode scalar value" => {
            if let Some((ch, range)) = next_as_ch() {
                let nodes = Nodes::new(format!("Prose: {prose}"), range);
                Ok(Some((nodes, src.advance(index, ch.len_utf8()))))
            } else {
                Ok(None)
            }
        }

        p => panic!("unknown prose {p}"),
    }
}
