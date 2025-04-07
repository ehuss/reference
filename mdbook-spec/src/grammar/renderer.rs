use std::borrow::Cow;
use regex::Regex;
use std::sync::LazyLock;
use super::{Characters, Expression, ExpressionKind, Production};
use crate::grammar::Grammar;
use crate::{Diagnostics, warn_or_err};
use anyhow::bail;
use std::fmt::Write;

impl Grammar {
    pub fn to_markdown(&self, names: &[&str], output: &mut String) -> anyhow::Result<()> {
        output.push_str(
            "> <div class=\"grammar\">\n\
             >\n",
        );
        let mut iter = names.into_iter().peekable();
        while let Some(name) = iter.next() {
            let prod = match self.productions.get(*name) {
                Some(p) => p,
                None => bail!("could not find grammar production named `{name}`"),
            };
            prod.to_markdown(output);
            if iter.peek().is_some() {
                output.push_str(">\n");
            }
        }
        output.push_str(
            ">\n\
             > </div>\n",
        );
        Ok(())
    }

    pub fn find_missing_nt(&self, diag: &mut Diagnostics) {
        for p in self.productions.values() {
            p.expression.find_missing_nt(self, diag);
        }
    }
}

impl Production {
    pub fn to_markdown(&self, output: &mut String) {
        write!(
            output,
            "> <span class=\"grammar-text grammar-production\" id=\"{}\">{}</span> → ",
            self.id(),
            self.name
        )
        .unwrap();
        self.expression.to_markdown(output);
        output.push('\n');
    }

    fn id(&self) -> String {
        format!("grammar-{}", self.name)
    }
}

impl Expression {
    fn last(&self) -> &ExpressionKind {
        match &self.kind {
            ExpressionKind::Alt(es) | ExpressionKind::Sequence(es) => es.last().unwrap().last(),
            ExpressionKind::Grouped(_)
            | ExpressionKind::Optional(_)
            | ExpressionKind::Repeat(_)
            | ExpressionKind::RepeatNonGreedy(_)
            | ExpressionKind::RepeatPlus(_)
            | ExpressionKind::RepeatPlusNonGreedy(_)
            | ExpressionKind::RepeatN(_, _, _)
            | ExpressionKind::Nt(_)
            | ExpressionKind::Terminal(_)
            | ExpressionKind::Prose(_)
            | ExpressionKind::Break(_)
            | ExpressionKind::Charset(_)
            | ExpressionKind::NegExpression(_)
            | ExpressionKind::Unicode(_) => &self.kind,
        }
    }

    fn to_markdown(&self, output: &mut String) {
        match &self.kind {
            ExpressionKind::Grouped(e) => {
                output.push_str("( ");
                e.to_markdown(output);
                if !matches!(e.last(), ExpressionKind::Break(_)) {
                    output.push(' ');
                }
                output.push(')');
            }
            ExpressionKind::Alt(es) => {
                let mut iter = es.iter().peekable();
                while let Some(e) = iter.next() {
                    e.to_markdown(output);
                    if iter.peek().is_some() {
                        if !matches!(e.last(), ExpressionKind::Break(_)) {
                            output.push(' ');
                        }
                        output.push_str("| ");
                    }
                }
            }
            ExpressionKind::Sequence(es) => {
                let mut iter = es.iter().peekable();
                while let Some(e) = iter.next() {
                    e.to_markdown(output);
                    if iter.peek().is_some() && !matches!(e.last(), ExpressionKind::Break(_)) {
                        output.push(' ');
                    }
                }
            }
            ExpressionKind::Optional(e) => {
                e.to_markdown(output);
                output.push_str("<sup>?</sup>");
            }
            ExpressionKind::Repeat(e) => {
                e.to_markdown(output);
                output.push_str("<sup>\\*</sup>");
            }
            ExpressionKind::RepeatNonGreedy(e) => {
                e.to_markdown(output);
                output.push_str("<sup>\\* (non-greedy)</sup>");
            }
            ExpressionKind::RepeatPlus(e) => {
                e.to_markdown(output);
                output.push_str("<sup>+</sup>");
            }
            ExpressionKind::RepeatPlusNonGreedy(e) => {
                e.to_markdown(output);
                output.push_str("<sup>+ (non-greedy)</sup>");
            }
            ExpressionKind::RepeatN(e, a, b) => {
                e.to_markdown(output);
                write!(
                    output,
                    "<sup>{}..{}</sup>",
                    a.map(|v| v.to_string()).unwrap_or_default(),
                    b.map(|v| v.to_string()).unwrap_or_default(),
                )
                .unwrap();
            }
            ExpressionKind::Nt(nt) => {
                write!(output, "<span class=\"grammar-text\">[{nt}]</span>").unwrap();
            }
            ExpressionKind::Terminal(t) => {
                write!(output, "<span class=\"grammar-literal\">{}</span>", markdown_escape(t)).unwrap();
            }
            ExpressionKind::Prose(s) => {
                write!(output, "<span class=\"grammar-text\">\\<{s}\\></span>").unwrap();
            }
            ExpressionKind::Break(indent) => {
                output.push_str("\\\n> ");
                output.push_str(&"&nbsp;".repeat(*indent));
            }
            ExpressionKind::Charset(set) => charset_to_markdown(set, output),
            ExpressionKind::NegExpression(e) => {
                output.push('~');
                e.to_markdown(output);
            }
            ExpressionKind::Unicode(s) => {
                output.push_str("U+");
                output.push_str(s);
            }
        }
        if let Some(suffix) = &self.suffix {
            write!(output, "<sub class=\"grammar-text\">{suffix}</sub>").unwrap();
        }
        if let Some(footnote) = &self.footnote {
            // The ZeroWidthSpace is to avoid conflicts with markdown link references.
            write!(output, "&ZeroWidthSpace;[^{footnote}]").unwrap();
        }
    }

    fn find_missing_nt(&self, grammar: &Grammar, diag: &mut Diagnostics) {
        match &self.kind {
            ExpressionKind::Grouped(e)
            | ExpressionKind::Optional(e)
            | ExpressionKind::Repeat(e)
            | ExpressionKind::RepeatNonGreedy(e)
            | ExpressionKind::RepeatPlus(e)
            | ExpressionKind::RepeatPlusNonGreedy(e)
            | ExpressionKind::RepeatN(e, _, _)
            | ExpressionKind::NegExpression(e) => {
                e.find_missing_nt(grammar, diag);
            }
            ExpressionKind::Alt(es) | ExpressionKind::Sequence(es) => {
                for e in es {
                    e.find_missing_nt(grammar, diag);
                }
            }
            ExpressionKind::Nt(nt) => {
                if !grammar.productions.contains_key(nt) {
                    warn_or_err!(diag, "non-terminal `{nt}` is used but not defined");
                }
            }
            ExpressionKind::Terminal(_)
            | ExpressionKind::Prose(_)
            | ExpressionKind::Break(_)
            | ExpressionKind::Unicode(_) => {}
            ExpressionKind::Charset(set) => {
                for ch in set {
                    match ch {
                        Characters::Named(s) => {
                            if !grammar.productions.contains_key(s) {
                                warn_or_err!(diag, "non-terminal `{s}` is used but not defined");
                            }
                        }
                        Characters::Terminal(_) | Characters::Range(_, _) => {}
                    }
                }
            }
        }
    }
}

fn charset_to_markdown(set: &[Characters], output: &mut String) {
    output.push_str("\\[");
    let mut iter = set.iter().peekable();
    while let Some(chars) = iter.next() {
        chars.to_markdown(output);
        if iter.peek().is_some() {
            output.push(' ');
        }
    }
    output.push(']');
}

impl Characters {
    fn to_markdown(&self, output: &mut String) {
        match self {
            Characters::Named(s) => write!(output, "[{s}]").unwrap(),
            Characters::Terminal(s) => write!(output, "<span class=\"grammar-literal\">{}</span>", markdown_escape(s)).unwrap(),
            Characters::Range(a, b) => write!(output, "<span class=\"grammar-literal\">{a}</span>-<span class=\"grammar-literal\">{b}</span>").unwrap(),
        }
    }
}

fn markdown_escape(s: &str) -> Cow<'_, str> {
    static ESC_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r#"[\\`_*\[\](){}'"]"#).unwrap());
    ESC_RE.replace_all(s, r"\$0")
}
