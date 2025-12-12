#![allow(unused)]
use super::{Expression, ExpressionKind};
use std::fmt::Display;
use std::fmt::Formatter;

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match &self.kind {
            ExpressionKind::Grouped(e) => write!(f, "({e})")?,
            ExpressionKind::Alt(es) => {
                for (i, e) in es.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{e}")?;
                }
            }
            ExpressionKind::Sequence(es) => {
                for (i, e) in es.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{e}")?;
                }
            }
            ExpressionKind::Optional(e) => write!(f, "{e}?")?,
            ExpressionKind::Repeat(e) => write!(f, "{e}*")?,
            ExpressionKind::RepeatNonGreedy(e) => write!(f, "{e}*?")?,
            ExpressionKind::RepeatPlus(e) => write!(f, "{e}+")?,
            ExpressionKind::RepeatPlusNonGreedy(e) => write!(f, "{e}+?")?,
            ExpressionKind::RepeatRange(e, min, max) => write!(
                f,
                "{e}{{{}..{}}}",
                min.map(|v| v.to_string()).unwrap_or_default(),
                max.map(|v| v.to_string()).unwrap_or_default(),
            )?,
            ExpressionKind::Nt(s) => write!(f, "{s}")?,
            ExpressionKind::Terminal(s) => write!(f, "`{s}`")?,
            ExpressionKind::Prose(s) => write!(f, "<{s}>")?,
            ExpressionKind::Break(n) => write!(f, " ")?,
            ExpressionKind::Comment(s) => {}
            ExpressionKind::Charset(chars) => {
                write!(f, "[")?;
                for (i, c) in chars.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    match c {
                        super::Characters::Named(s) => write!(f, "{s}")?,
                        super::Characters::Terminal(s) => write!(f, "`{s}`")?,
                        super::Characters::Range(start, end) => write!(f, "`{start}`-`{end}`")?,
                    }
                }
                write!(f, "]")?;
            }
            ExpressionKind::NegExpression(e) => write!(f, "~{e}")?,
            ExpressionKind::Unicode(s) => write!(f, "U+{s}")?,
        }
        Ok(())
    }
}
