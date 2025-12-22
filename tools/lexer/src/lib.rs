#![allow(unused)]
use diagnostics::Diagnostics;
use grammar::Characters;
use grammar::Expression;
use grammar::ExpressionKind;
use grammar::Grammar;
use std::collections::HashMap;
use std::error::Error;
use std::ops::Range;
use tracing::debug;
use tracing::instrument;

// type Result<T> = std::result::Result<T, Box<dyn Error>>;

#[derive(Debug, Clone)]
pub struct Token {
    pub name: String,
    pub range: Range<usize>,
}

#[derive(Debug)]
pub struct LexError {
    pub byte_offset: usize,
    pub message: String,
}

impl LexError {
    pub fn display(&self, src: &str) -> String {
        let s = &src[self.byte_offset..];
        match s.char_indices().nth(100) {
            Some((i, _)) => format!("{} at `{}…`", self.message, &s[..i]),
            None => format!("{} at `{s}`", self.message),
        }
    }
}

#[derive(Debug, Default)]
struct Environment {
    map: HashMap<String, u32>,
}

pub fn tokenize(src: &str) -> Result<Vec<Token>, LexError> {
    let mut diag = Diagnostics::new();
    let mut grammar = grammar::load_grammar(&mut diag);
    remove_breaks(&mut grammar);

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
    let map_offset =
        |offset: usize| -> usize { offset + removed_indices.partition_point(|&x| x < offset) };

    let mut tokens = Vec::new();
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
    if let Some(i) = parse_expression(
        &grammar,
        &shebang.expression,
        None,
        &normalized_src,
        index,
        &mut Environment::default(),
    ).map_err(|mut e| {
        e.byte_offset = map_offset(e.byte_offset);
        e
    })? {
        index += i;
    }

    // TODO: Is frontmatter before/after crlf normalization?
    let frontmatter = grammar.productions.get("FRONTMATTER").unwrap();
    if let Some(i) = parse_expression(
        &grammar,
        &frontmatter.expression,
        None,
        &normalized_src,
        index,
        &mut Environment::default(),
    )
    .map_err(|mut e| {
        e.byte_offset = map_offset(e.byte_offset);
        e
    })? {
        index += i;
    } else {
        let invalid_frontmatter = grammar.productions.get("INVALID_FRONTMATTER").unwrap();
        if let Some(_) = parse_expression(
            &grammar,
            &invalid_frontmatter.expression,
            None,
            &normalized_src,
            index,
            &mut Environment::default(),
        ).map_err(|mut e| {
        e.byte_offset = map_offset(e.byte_offset);
        e
    })? {
            return Err(LexError {
                message: "invalid frontmatter".to_string(),
                byte_offset: index,
            });
        }
    }

    while index < normalized_src.len() {
        if let Some(i) = parse_expression(
            &grammar,
            whitespace,
            None,
            &normalized_src,
            index,
            &mut Environment::default(),
        )
        .map_err(|mut e| {
            e.byte_offset = map_offset(e.byte_offset);
            e
        })? {
            index += i;
            continue;
        }

        let mut matched_token = None;
        for token_prod in &top_prods {
            debug!("try top-level token `{}`", token_prod.name);
            match parse_expression(
                &grammar,
                &token_prod.expression,
                None,
                &normalized_src,
                index,
                &mut Environment::default(),
            )
            .map_err(|mut e| {
                e.byte_offset = map_offset(e.byte_offset);
                e
            })? {
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
                tokens.push(t);
            }
            None => {
                return Err(LexError {
                    byte_offset: map_offset(index),
                    message: String::from("no tokens matched"),
                });
            }
        }
    }

    let mut stack = Vec::new();
    for token in &tokens {
        let text = &src[token.range.clone()];
        match text {
            "(" | "[" | "{" => stack.push((text, token.range.start)),
            ")" => {
                if stack.pop().map(|(s, _)| s) != Some("(") {
                    return Err(LexError {
                        byte_offset: token.range.start,
                        message: "unbalanced `)`".to_string(),
                    });
                }
            }
            "]" => {
                if stack.pop().map(|(s, _)| s) != Some("[") {
                    return Err(LexError {
                        byte_offset: token.range.start,
                        message: "unbalanced `]`".to_string(),
                    });
                }
            }
            "}" => {
                if stack.pop().map(|(s, _)| s) != Some("{") {
                    return Err(LexError {
                        byte_offset: token.range.start,
                        message: "unbalanced `}`".to_string(),
                    });
                }
            }
            _ => {}
        }
    }
    if let Some((_, offset)) = stack.pop() {
        return Err(LexError {
            byte_offset: offset,
            message: "unclosed delimiter".to_string(),
        });
    }

    Ok(tokens)
}

#[instrument(level = "debug", skip(grammar, e, next, src), ret)]
fn parse_expression(
    grammar: &Grammar,
    e: &Expression,
    next: Option<&Expression>,
    src: &str,
    index: usize,
    env: &mut Environment,
) -> Result<Option<usize>, LexError> {
    // eprintln!("match {e:?}");
    tracing::debug!("e={e}");
    if index < src.len() {
        tracing::debug!("next char={:?}", src[index..].chars().next());
    } else {
        tracing::debug!("eof");
    }
    match &e.kind {
        ExpressionKind::Grouped(group) => {
            let l = parse_expression(grammar, group, None, src, index, env)?;
            let l = match l {
                Some(l) => l,
                None => return Ok(None),
            };
            match e.suffix.as_deref() {
                Some("not immediately followed by XID_Continue") => {
                    if let Some(next_ch) = src[index + l..].chars().next() {
                        if unicode_ident::is_xid_continue(next_ch) {
                            return Ok(None);
                        }
                    }
                }
                Some("not immediately followed by `'`") => {
                    if src[index + l..].chars().next() == Some('\'') {
                        return Ok(None);
                    }
                }
                Some(s) => panic!("unknown suffix {s:?}"),
                None => {}
            }
            Ok(Some(l))
        }
        ExpressionKind::Alt(es) => {
            assert_eq!(e.suffix, None);
            for e in es {
                if let Some(l) = parse_expression(grammar, e, None, src, index, env)? {
                    if l != 0 {
                        return Ok(Some(l));
                    }
                }
            }
            Ok(None)
        }
        ExpressionKind::Sequence(es) => {
            assert_eq!(e.suffix, None);
            let mut i = 0;
            let mut es_i = es.iter().peekable();
            while let Some(e) = es_i.next() {
                let next = es_i.peek().map(|e| *e);
                match parse_expression(grammar, e, next, &src, index + i, env)? {
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
            match parse_expression(grammar, opt, None, src, index, env)? {
                Some(l) => Ok(Some(l)),
                None => Ok(Some(0)),
            }
        }
        ExpressionKind::Repeat(r) => {
            assert_eq!(e.suffix, None);
            let mut i = 0;
            while i < src.len() {
                match parse_expression(grammar, r, None, &src, index + i, env)? {
                    Some(l) => i += l,
                    None => break,
                }
            }
            Ok(Some(i))
        }
        ExpressionKind::RepeatNonGreedy(r) => {
            assert_eq!(e.suffix, None);
            let Some(next) = next else {
                panic!("expected next after non-greedy");
            };
            let mut i = 0;
            while i < src.len() {
                if let Some(_) = parse_expression(grammar, next, None, src, index + i, env)? {
                    break;
                }
                match parse_expression(grammar, r, None, &src, index + i, env)? {
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
                match parse_expression(grammar, r, None, &src, index + i, env)? {
                    Some(l) => i += l,
                    None => break,
                }
            }
            if i == 0 { Ok(None) } else { Ok(Some(i)) }
        }
        ExpressionKind::RepeatPlusNonGreedy(r) => {
            assert_eq!(r.suffix, None);
            todo!()
        }
        ExpressionKind::RepeatRange(r, name, min, max) => {
            let mut i = 0;
            let mut count = 0;
            while i < src.len() {
                match parse_expression(grammar, r, None, &src, index + i, env)? {
                    Some(l) => {
                        i += l;
                        if let Some(max) = max {
                            if count + 1 == *max {
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
                        u32::from_str_radix(&hex_no_underscores, 16).map_err(|_| LexError {
                            byte_offset: index,
                            message: format!("invalid hex value: {hex}"),
                        })?;
                    if char::from_u32(value).is_none() {
                        return Err(LexError {
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
                match parse_expression(grammar, r, None, &src, index + i, env)? {
                    Some(l) => i += l,
                    None => return Ok(None),
                }
            }
            Ok(Some(i))
        }
        ExpressionKind::Nt(s) => {
            let prod = grammar.productions.get(s).unwrap();
            let l = parse_expression(
                grammar,
                &prod.expression,
                None,
                src,
                index,
                env,
            )?;
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
                Some("not immediately followed by `'`") => {
                    if src[index + l..].chars().next() == Some('\'') {
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
                Some("not immediately followed by `.`, `_` or an XID_Start character") => {
                    if let Some(next_ch) = src[index + l..].chars().next() {
                        if matches!(next_ch, '.' | '_') || unicode_ident::is_xid_start(next_ch) {
                            return Ok(None);
                        }
                    }
                }
                Some("not immediately followed by `#`") => {
                    if src[index + l..].chars().next() == Some('#') {
                        return Ok(None);
                    }
                }
                Some("immediately followed by LF") => {
                    if src[index + l..].chars().next() != Some('\n') {
                        return Ok(None);
                    }
                }
                Some("not immediately followed by `*` or `/`") => {
                    if matches!(src[index + l..].chars().next(), Some('*' | '/')) {
                        return Ok(None);
                    }
                }
                Some("not immediately followed by `/`") => {
                    if src[index + l..].chars().next() == Some('/') {
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
            match_prose(grammar, s, src, index, env)
        }
        ExpressionKind::Break(_) => unreachable!(),
        ExpressionKind::Comment(s) => Ok(Some(0)),
        ExpressionKind::Charset(chars) => {
            assert_eq!(e.suffix, None);
            if index >= src.len() {
                return Ok(None);
            }
            for ch in chars {
                match ch {
                    Characters::Named(name) => {
                        let prod = grammar.productions.get(name).unwrap();
                        if let Some(l) = parse_expression(
                            grammar,
                            &prod.expression,
                            None,
                            src,
                            index,
                            env,
                        )? {
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
            match parse_expression(grammar, neg, None, src, index, env)? {
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
        ExpressionKind::Cut(e1, e2) => {
            assert_eq!(e.suffix, None);
            let e1_len = match parse_expression(grammar, e1, None, src, index, env)? {
                Some(l) => l,
                None => return Ok(None),
            };
            match parse_expression(grammar, e2, None, src, index + e1_len, env)? {
                Some(e2_len) => Ok(Some(e1_len + e2_len)),
                None => Err(LexError {
                    byte_offset: index + e1_len,
                    message: format!("expected {}", e2),
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

fn match_prose(
    grammar: &Grammar,
    prose: &str,
    src: &str,
    index: usize,
    env: &mut Environment,
) -> Result<Option<usize>, LexError> {
    let ch = src[index..].chars().next();
    let mut eof_but_not_digit = |prod| match ch {
        Some(ch) => {
            let p = grammar.productions.get(prod).unwrap();
            match parse_expression(
                grammar,
                &p.expression,
                None,
                src,
                index,
                &mut Environment::default(),
            )? {
                Some(_) => Ok(None),
                None => Ok(Some(0)),
            }
        }
        None => Ok(Some(0)),
    };
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
        "end of input or not BIN_DIGIT" => eof_but_not_digit("BIN_DIGIT"),
        "end of input or not DEC_DIGIT" => eof_but_not_digit("DEC_DIGIT"),
        "end of input or not HEX_DIGIT" => eof_but_not_digit("HEX_DIGIT"),
        "end of input or not OCT_DIGIT" => eof_but_not_digit("OCT_DIGIT"),
        "a Unicode scalar value" => {
            if let Some(ch) = ch {
                Ok(Some(ch.len_utf8()))
            } else {
                Ok(None)
            }
        }
        "end of input" => {
            match ch {
                Some(_) => Ok(None),
                None => Ok(Some(0))
            }
        }

        p => panic!("unknown prose {p}"),
    }
}

fn remove_breaks(grammar: &mut Grammar) {
    for prod in grammar.productions.values_mut() {
        remove_break_expr(&mut prod.expression);
    }
}

fn remove_break_expr(e: &mut Expression) {
    match &mut e.kind {
        ExpressionKind::Grouped(e) => remove_break_expr(e),
        ExpressionKind::Alt(es) => {
            es.retain(|e| !matches!(e.kind, ExpressionKind::Break(_)));
            for e in es {
                remove_break_expr(e);
            }
        }
        ExpressionKind::Sequence(es) => {
            es.retain(|e| !matches!(e.kind, ExpressionKind::Break(_)));
            for e in es {
                remove_break_expr(e);
            }
        }
        ExpressionKind::Optional(e) => remove_break_expr(e),
        ExpressionKind::Repeat(e) => remove_break_expr(e),
        ExpressionKind::RepeatNonGreedy(e) => remove_break_expr(e),
        ExpressionKind::RepeatPlus(e) => remove_break_expr(e),
        ExpressionKind::RepeatPlusNonGreedy(e) => remove_break_expr(e),
        ExpressionKind::RepeatRange(e, _, _, _) => remove_break_expr(e),
        ExpressionKind::RepeatRangeNamed(e, _) => remove_break_expr(e),
        ExpressionKind::Nt(_) => {}
        ExpressionKind::Terminal(_) => {}
        ExpressionKind::Prose(_) => {}
        ExpressionKind::Break(_) => panic!("unvisitable"),
        ExpressionKind::Comment(_) => {}
        ExpressionKind::Charset(_) => {}
        ExpressionKind::NegExpression(e) => remove_break_expr(e),
        ExpressionKind::Cut(e1, e2) => {
            remove_break_expr(e1);
            remove_break_expr(e2);
        }
        ExpressionKind::Unicode(_) => {}
    }
}
