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
            ExpressionKind::Not(e) => write!(f, "-{e}")?,
            ExpressionKind::Repeat(e) => write!(f, "{e}*")?,
            ExpressionKind::RepeatPlus(e) => write!(f, "{e}+")?,
            ExpressionKind::RepeatRange{expr, name, min, max, limit} => write!(
                f,
                "{expr}{{{}{}{limit}{}}}",
                name.as_ref().map(|n| format!("{n}:")).unwrap_or_default(),
                min.map(|v| v.to_string()).unwrap_or_default(),
                max.map(|v| v.to_string()).unwrap_or_default(),
            )?,
            ExpressionKind::RepeatRangeNamed(e, name) => write!(f, "{e}{{{name}}}")?,
            ExpressionKind::Nt(s) => write!(f, "{s}")?,
            ExpressionKind::Terminal(s) => write!(f, "`{s}`")?,
            ExpressionKind::Prose(s) => write!(f, "<{s}>")?,
            ExpressionKind::Break(_) => write!(f, " ")?,
            ExpressionKind::Comment(_) => {}
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
            ExpressionKind::Cut => write!(f, "^")?,
            ExpressionKind::Unicode(s) => write!(f, "U+{s}")?,
        }
        if let Some(suffix) = &self.suffix {
            write!(f, " _{suffix}_")?;
        }
        Ok(())
    }
}
