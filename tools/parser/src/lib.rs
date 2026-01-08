use grammar::{Expression, ExpressionKind, Grammar};

pub mod ast;
pub mod lexer;
mod parser;

#[derive(Debug)]
pub struct ParseError {
    pub byte_offset: usize,
    pub message: String,
}

impl ParseError {
    pub fn display(&self, src: &str) -> String {
        let s = &src[self.byte_offset..];
        match s.char_indices().nth(100) {
            Some((i, _)) => format!("{} at `{}…`", self.message, &s[..i]),
            None => format!("{} at `{s}`", self.message),
        }
    }
}

fn remove_breaks(grammar: &mut Grammar) {
    for prod in grammar.productions.values_mut() {
        remove_break_expr(&mut prod.expression);
    }
}

fn remove_break_expr(e: &mut Expression) {
    match &mut e.kind {
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
        ExpressionKind::Grouped(e)
        | ExpressionKind::Optional(e)
        | ExpressionKind::Not(e)
        | ExpressionKind::Repeat(e)
        | ExpressionKind::RepeatPlus(e)
        | ExpressionKind::RepeatRange { expr: e, .. }
        | ExpressionKind::RepeatRangeNamed(e, _)
        | ExpressionKind::NegExpression(e)
        | ExpressionKind::Cut(e) => remove_break_expr(e),
        ExpressionKind::Nt(_) => {}
        ExpressionKind::Terminal(_) => {}
        ExpressionKind::Prose(_) => {}
        ExpressionKind::Break(_) => panic!("unvisitable"),
        ExpressionKind::Comment(_) => {}
        ExpressionKind::Charset(_) => {}
        ExpressionKind::Unicode(_) => {}
    }
}
