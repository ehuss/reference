use super::{Characters, Expression, ExpressionKind, Production};
use crate::grammar::Grammar;
use crate::{Diagnostics, warn_or_err};
use anyhow::bail;
use std::fmt::Write;

impl Grammar {
    pub fn to_html(&self, names: &[&str], output: &mut String) -> anyhow::Result<()> {
        if self
            .productions
            .get(names[0])
            .map(|p| p.is_lexer)
            .unwrap_or(false)
        {
            output.push_str("> **<sup>Lexer</sup>**\n");
        } else {
            output.push_str("> **<sup>Syntax</sup>**\n");
        }
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
            prod.to_html(output);
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
    pub fn to_html(&self, output: &mut String) {
        write!(
            output,
            "> <span id=\"{}\">{}</span> → ",
            self.id(),
            self.name
        )
        .unwrap();
        self.expression.to_html(output);
        output.push('\n');
    }

    fn id(&self) -> String {
        format!("grammar-{}", self.name)
    }
}

impl Expression {
    fn to_html(&self, output: &mut String) {
        match &self.kind {
            ExpressionKind::Grouped(e) => {
                output.push_str("( ");
                e.to_html(output);
                output.push_str(" )");
            }
            ExpressionKind::Alt(es) => {
                let mut iter = es.iter().peekable();
                while let Some(e) = iter.next() {
                    e.to_html(output);
                    if iter.peek().is_some() {
                        output.push_str(" | ");
                    }
                }
            }
            ExpressionKind::Sequence(es) => {
                let mut iter = es.iter().peekable();
                while let Some(e) = iter.next() {
                    e.to_html(output);
                    if iter.peek().is_some() {
                        output.push(' ');
                    }
                }
            }
            ExpressionKind::Optional(e) => {
                e.to_html(output);
                output.push_str("<sup>?</sup>");
            }
            ExpressionKind::Repeat(e) => {
                e.to_html(output);
                output.push_str("<sup>\\*</sup>");
            }
            ExpressionKind::RepeatNonGreedy(e) => {
                e.to_html(output);
                output.push_str("<sup>\\* (non-greedy)</sup>");
            }
            ExpressionKind::RepeatPlus(e) => {
                e.to_html(output);
                output.push_str("<sup>+</sup>");
            }
            ExpressionKind::RepeatPlusNonGreedy(e) => {
                e.to_html(output);
                output.push_str("<sup>+ (non-greedy)</sup>");
            }
            ExpressionKind::RepeatN(e, a, b) => {
                e.to_html(output);
                write!(
                    output,
                    "<sup>{}..{}</sup>",
                    a.map(|v| v.to_string()).unwrap_or_default(),
                    b.map(|v| v.to_string()).unwrap_or_default(),
                )
                .unwrap();
            }
            ExpressionKind::Nt(nt) => {
                write!(output, "[{nt}]").unwrap();
            }
            ExpressionKind::Terminal(t) => {
                write!(output, "`{t}`").unwrap();
            }
            ExpressionKind::Prose(s) => {
                write!(output, "<span class=\"grammar-prose\">\\<{s}\\></span>").unwrap();
            }
            ExpressionKind::Break(indent) => {
                output.push_str("\\\n> ");
                output.push_str(&"&nbsp;".repeat(*indent));
            }
            ExpressionKind::Charset(set) => charset_to_html(set, output),
            ExpressionKind::NegExpression(e) => {
                output.push('~');
                e.to_html(output);
            }
            ExpressionKind::Unicode(s) => {
                output.push_str("U+");
                output.push_str(s);
            }
        }
        if let Some(suffix) = &self.suffix {
            write!(output, " <sub class=\"grammar-prose\">{suffix}</sub>").unwrap();
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

fn charset_to_html(set: &[Characters], output: &mut String) {
    output.push_str("\\[");
    let mut iter = set.iter().peekable();
    while let Some(chars) = iter.next() {
        chars.to_html(output);
        if iter.peek().is_some() {
            output.push(' ');
        }
    }
    output.push(']');
}

impl Characters {
    fn to_html(&self, output: &mut String) {
        match self {
            Characters::Named(s) => write!(output, "[{s}]").unwrap(),
            Characters::Terminal(s) => write!(output, "`{s}`").unwrap(),
            Characters::Range(a, b) => write!(output, "`{a}`-`{b}`").unwrap(),
        }
    }
}
