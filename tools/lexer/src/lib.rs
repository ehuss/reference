use diagnostics::Diagnostics;
use grammar::Characters;
use grammar::Expression;
use grammar::ExpressionKind;
use grammar::Grammar;
use grammar::RangeLimit;
use std::collections::HashMap;
use std::ops::Range;
use tracing::debug;
use tracing::instrument;

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

pub fn tokenize(src: &str) -> Result<Tokens, LexError> {
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
    if let Some(i) = parse_expression(
        &grammar,
        &shebang.expression,
        &normalized_src,
        index,
        &mut Environment::default(),
    )
    .map_err(|mut e| {
        e.byte_offset = map_offset(e.byte_offset);
        e
    })? {
        tokens.shebang = Some(Range {
            start: map_offset(index),
            end: map_offset(index + i),
        });
        index += i;
    }

    // TODO: Is frontmatter before/after crlf normalization?
    let frontmatter = grammar.productions.get("FRONTMATTER").unwrap();
    if let Some(i) = parse_expression(
        &grammar,
        &frontmatter.expression,
        &normalized_src,
        index,
        &mut Environment::default(),
    )
    .map_err(|mut e| {
        e.byte_offset = map_offset(e.byte_offset);
        LexError {
            message: format!("invalid frontmatter: {}", e.message),
            byte_offset: e.byte_offset,
        }
    })? {
        tokens.frontmatter = Some(Range {
            start: map_offset(index),
            end: map_offset(index + i),
        });
        index += i;
    } else {
        let invalid_frontmatter = grammar.productions.get("INVALID_FRONTMATTER").unwrap();
        if let Some(_) = parse_expression(
            &grammar,
            &invalid_frontmatter.expression,
            &normalized_src,
            index,
            &mut Environment::default(),
        )
        .map_err(|mut e| {
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
                tokens.tokens.push(t);
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
    for token in &tokens.tokens {
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

#[instrument(level = "debug", skip(grammar, e, src), ret)]
fn parse_expression(
    grammar: &Grammar,
    e: &Expression,
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
            assert_eq!(e.suffix, None);
            let l = parse_expression(grammar, group, src, index, env)?;
            let l = match l {
                Some(l) => l,
                None => return Ok(None),
            };
            Ok(Some(l))
        }
        ExpressionKind::Alt(es) => {
            assert_eq!(e.suffix, None);
            for e in es {
                if let Some(l) = parse_expression(grammar, e, src, index, env)? {
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
            let mut cut = false;
            while let Some(e) = es_i.next() {
                if matches!(e.kind, ExpressionKind::Cut) {
                    cut = true;
                    continue;
                }
                match parse_expression(grammar, e, &src, index + i, env)? {
                    Some(l) => {
                        i += l;
                    }
                    None if cut => {
                        return Err(LexError {
                            byte_offset: index + i,
                            message: format!("expected {e}"),
                        });
                    }
                    None => return Ok(None),
                }
            }
            Ok(Some(i))
        }
        ExpressionKind::Optional(opt) => {
            assert_eq!(e.suffix, None);
            match parse_expression(grammar, opt, src, index, env)? {
                Some(l) => Ok(Some(l)),
                None => Ok(Some(0)),
            }
        }
        ExpressionKind::Not(n) => {
            assert_eq!(e.suffix, None);
            match parse_expression(grammar, n, src, index, env)? {
                Some(_) => Ok(None),
                None => Ok(Some(0)),
            }
        }
        ExpressionKind::Repeat(r) => {
            assert_eq!(e.suffix, None);
            let mut i = 0;
            while i < src.len() {
                match parse_expression(grammar, r, &src, index + i, env)? {
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
                match parse_expression(grammar, r, &src, index + i, env)? {
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
                match parse_expression(grammar, r, &src, index + i, env)? {
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
                match parse_expression(grammar, r, &src, index + i, env)? {
                    Some(l) => i += l,
                    None => return Ok(None),
                }
            }
            Ok(Some(i))
        }
        ExpressionKind::Nt(s) => {
            let prod = grammar.productions.get(s).unwrap();
            let l = parse_expression(grammar, &prod.expression, src, index, env)?;
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
                        if let Some(l) =
                            parse_expression(grammar, &prod.expression, src, index, env)?
                        {
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
            match parse_expression(grammar, neg, src, index, env)? {
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
        ExpressionKind::Cut => {
            panic!("unexpected cut operator");
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

fn match_prose(prose: &str, src: &str, index: usize) -> Result<Option<usize>, LexError> {
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
        ExpressionKind::Not(e) => remove_break_expr(e),
        ExpressionKind::Repeat(e) => remove_break_expr(e),
        ExpressionKind::RepeatPlus(e) => remove_break_expr(e),
        ExpressionKind::RepeatRange { expr: e, .. } => remove_break_expr(e),
        ExpressionKind::RepeatRangeNamed(e, _) => remove_break_expr(e),
        ExpressionKind::Nt(_) => {}
        ExpressionKind::Terminal(_) => {}
        ExpressionKind::Prose(_) => {}
        ExpressionKind::Break(_) => panic!("unvisitable"),
        ExpressionKind::Comment(_) => {}
        ExpressionKind::Charset(_) => {}
        ExpressionKind::NegExpression(e) => remove_break_expr(e),
        ExpressionKind::Cut => {}
        ExpressionKind::Unicode(_) => {}
    }
}
