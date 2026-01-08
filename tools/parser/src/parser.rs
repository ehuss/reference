use super::ParseError;
use grammar::{Characters, Expression, ExpressionKind, Grammar, RangeLimit};
use std::collections::HashMap;
use tracing::instrument;

#[derive(Debug, Default)]
struct Environment {
    map: HashMap<String, u32>,
}

pub(crate) fn parse_expression(
    grammar: &Grammar,
    e: &Expression,
    src: &str,
    index: usize,
) -> Result<Option<usize>, ParseError> {
    parse(grammar, e, src, index, &mut Environment::default())
}

#[instrument(level = "debug", skip(grammar, e, src), ret)]
fn parse(
    grammar: &Grammar,
    e: &Expression,
    src: &str,
    index: usize,
    env: &mut Environment,
) -> Result<Option<usize>, ParseError> {
    tracing::debug!("e={e}");
    if index < src.len() {
        tracing::debug!("next char={:?}", src[index..].chars().next());
    } else {
        tracing::debug!("eof");
    }
    match &e.kind {
        ExpressionKind::Grouped(group) => {
            assert_eq!(e.suffix, None);
            let l = parse(grammar, group, src, index, env)?;
            let l = match l {
                Some(l) => l,
                None => return Ok(None),
            };
            Ok(Some(l))
        }
        ExpressionKind::Alt(es) => {
            assert_eq!(e.suffix, None);
            for e in es {
                if let Some(l) = parse(grammar, e, src, index, env)? {
                    // if l != 0 {
                    return Ok(Some(l));
                    // }
                }
            }
            Ok(None)
        }
        ExpressionKind::Sequence(es) => {
            assert_eq!(e.suffix, None);
            let mut i = 0;
            let mut es_i = es.iter().peekable();
            while let Some(e) = es_i.next() {
                match parse(grammar, e, &src, index + i, env)? {
                    Some(l) => {
                        i += l;
                    }
                    None => return Ok(None),
                }
            }
            Ok(Some(i))
        }
        ExpressionKind::Optional(opt) => {
            assert_eq!(e.suffix, None);
            match parse(grammar, opt, src, index, env)? {
                Some(l) => Ok(Some(l)),
                None => Ok(Some(0)),
            }
        }
        ExpressionKind::Not(n) => {
            assert_eq!(e.suffix, None);
            match parse(grammar, n, src, index, env)? {
                Some(_) => Ok(None),
                None => Ok(Some(0)),
            }
        }
        ExpressionKind::Repeat(r) => {
            assert_eq!(e.suffix, None);
            let mut i = 0;
            while i < src.len() {
                match parse(grammar, r, &src, index + i, env)? {
                    Some(l) => i += l,
                    None => break,
                }
            }
            Ok(Some(i))
        }
        ExpressionKind::RepeatPlus(r) => {
            assert_eq!(e.suffix, None);
            let mut i = 0;
            while i < src.len() {
                match parse(grammar, r, &src, index + i, env)? {
                    Some(l) => i += l,
                    None => break,
                }
            }
            if i == 0 { Ok(None) } else { Ok(Some(i)) }
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
            let mut i = 0;
            let mut count = 0;
            while i < src.len() {
                match parse(grammar, r, &src, index + i, env)? {
                    Some(l) => {
                        i += l;
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

            match e.suffix.as_deref() {
                Some("valid hex char value") => {
                    let hex = &src[index..index + i];
                    let hex_no_underscores = hex.replace('_', "");
                    let value =
                        u32::from_str_radix(&hex_no_underscores, 16).map_err(|_| ParseError {
                            byte_offset: index,
                            message: format!("invalid hex value: {hex}"),
                        })?;
                    if char::from_u32(value).is_none() {
                        return Err(ParseError {
                            byte_offset: index,
                            message: format!("invalid Unicode scalar value: {hex}"),
                        });
                    }
                }
                Some(s) => panic!("unknown suffix {s:?}"),
                None => {}
            }

            Ok(Some(i))
        }
        ExpressionKind::RepeatRangeNamed(r, name) => {
            assert_eq!(e.suffix, None);
            let Some(count) = env.map.get(name) else {
                panic!("expected {name} in environment for {r}");
            };
            let mut i = 0;
            for _ in 0..*count {
                match parse(grammar, r, &src, index + i, env)? {
                    Some(l) => i += l,
                    None => return Ok(None),
                }
            }
            Ok(Some(i))
        }
        ExpressionKind::Nt(s) => {
            let prod = grammar.productions.get(s).unwrap();
            let l = parse(grammar, &prod.expression, src, index, env)?;
            let l = match l {
                Some(l) => l,
                None => return Ok(None),
            };
            match e.suffix.as_deref() {
                Some("except `\\0` or `\\x00`") => {
                    let s = &src[index..index + l];
                    if matches!(s, "\\0" | "\\x00") {
                        return Ok(None);
                    }
                }
                Some("except `\\u{0}`, `\\u{00}`, …, `\\u{000000}`") => {
                    let s = &src[index..index + l];
                    if matches!(
                        s,
                        "\\u{0}"
                            | "\\u{00}"
                            | "\\u{000}"
                            | "\\u{0000}"
                            | "\\u{00000}"
                            | "\\u{000000}"
                    ) {
                        return Ok(None);
                    }
                }
                Some("except `_`") => {
                    let s = &src[index..index + l];
                    if s == "_" {
                        return Ok(None);
                    }
                }
                Some("except `b` or `c` or `r` or `br` or `cr`") => {
                    let s = &src[index..index + l];
                    if matches!(s, "b" | "c" | "r" | "br" | "cr") {
                        return Ok(None);
                    }
                }
                Some("except `b`") => {
                    let s = &src[index..index + l];
                    if s == "b" {
                        return Ok(None);
                    }
                }
                Some("except `r` or `br` or `cr`") => {
                    let s = &src[index..index + l];
                    if matches!(s, "r" | "br" | "cr") {
                        return Ok(None);
                    }
                }
                Some("except `r`") => {
                    let s = &src[index..index + l];
                    if s == "r" {
                        return Ok(None);
                    }
                }
                Some("not beginning with `e` or `E`") => {
                    let s = &src[index..index + l];
                    if s.starts_with('e') || s.starts_with('E') {
                        return Ok(None);
                    }
                }
                Some(s) => panic!("unknown suffix {s:?}"),
                None => {}
            }
            Ok(Some(l))
        }
        ExpressionKind::Terminal(s) => {
            if index >= src.len() || !src[index..].starts_with(s) {
                return Ok(None);
            }
            let l = s.len();
            match e.suffix.as_deref() {
                Some("immediately followed by LF") => {
                    if src[index + l..].chars().next() != Some('\n') {
                        return Ok(None);
                    }
                }
                Some(s) => panic!("unknown suffix {s:?}"),
                None => {}
            }
            Ok(Some(l))
        }
        ExpressionKind::Prose(s) => {
            assert_eq!(e.suffix, None);
            match_prose(s, src, index)
        }
        ExpressionKind::Break(_) => unreachable!(),
        ExpressionKind::Comment(_) => Ok(Some(0)),
        ExpressionKind::Charset(chars) => {
            assert_eq!(e.suffix, None);
            if index >= src.len() {
                return Ok(None);
            }
            for ch in chars {
                match ch {
                    Characters::Named(name) => {
                        let prod = grammar.productions.get(name).unwrap();
                        if let Some(l) = parse(grammar, &prod.expression, src, index, env)? {
                            return Ok(Some(l));
                        }
                    }
                    Characters::Terminal(s) => {
                        if src[index..].starts_with(s) {
                            return Ok(Some(s.len()));
                        }
                    }
                    Characters::Range(a, b) => {
                        let next = src[index..].chars().next().unwrap();
                        if next >= *a && next <= *b {
                            return Ok(Some(next.len_utf8()));
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
                    if let Some(ch) = src[index..].chars().next() {
                        Ok(Some(ch.len_utf8()))
                    } else {
                        Ok(None)
                    }
                }
            }
        }
        ExpressionKind::Cut(e) => {
            assert_eq!(e.suffix, None);
            match parse(grammar, e, src, index, env)? {
                Some(l) => Ok(Some(l)),
                None => Err(ParseError {
                    byte_offset: index,
                    message: format!("expected {}", e),
                }),
            }
        }
        ExpressionKind::Unicode(s) => {
            assert_eq!(e.suffix, None);
            let c = char::from_u32(u32::from_str_radix(s, 16).unwrap()).unwrap();
            if src[index..].starts_with(c) {
                Ok(Some(c.len_utf8()))
            } else {
                Ok(None)
            }
        }
    }
}

fn match_prose(prose: &str, src: &str, index: usize) -> Result<Option<usize>, ParseError> {
    let ch = src[index..].chars().next();
    let ascii_but = |except: &dyn Fn(char) -> bool| {
        if let Some(ch) = ch {
            Ok((ch >= '\0' && ch <= '\x7f' && !except(ch)).then_some(1))
        } else {
            Ok(None)
        }
    };

    match prose {
        "`XID_Start` defined by Unicode" => {
            if let Some(ch) = ch {
                Ok(unicode_ident::is_xid_start(ch).then(|| ch.len_utf8()))
            } else {
                Ok(None)
            }
        }
        "`XID_Continue` defined by Unicode" => {
            if let Some(ch) = ch {
                Ok(unicode_ident::is_xid_continue(ch).then(|| ch.len_utf8()))
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
            if let Some(ch) = ch {
                Ok(Some(ch.len_utf8()))
            } else {
                Ok(None)
            }
        }

        p => panic!("unknown prose {p}"),
    }
}
