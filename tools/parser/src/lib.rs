use grammar::{Expression, ExpressionKind, Grammar};
use std::ops::Range;

pub mod lexer;
mod parser;
pub mod tree;

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

/// A parsed section of source corresponding to some grammar expression.
#[derive(Clone, Debug, Default)]
pub struct Node {
    pub name: String,
    /// Range in bytes of the original source that this node covers.
    pub range: Range<usize>,
    pub children: Nodes,
}

impl Node {
    pub fn new(name: String, range: Range<usize>) -> Node {
        Node {
            name,
            range,
            children: Nodes::default(),
        }
    }

    fn with_children(name: String, start: usize, children: Nodes) -> Node {
        let range = if children.0.is_empty() {
            Range { start, end: start }
        } else {
            Range {
                start: children.0.first().unwrap().range.start,
                end: children.0.last().unwrap().range.end,
            }
        };
        Node {
            name,
            range,
            children,
        }
    }

    fn byte_len(&self) -> usize {
        self.range.end - self.range.start
    }
}

#[derive(Clone, Debug, Default)]
pub struct Nodes(pub Vec<Node>);

impl Nodes {
    fn new(name: String, start: usize, length: usize) -> Nodes {
        let node = Node {
            name,
            range: Range {
                start,
                end: start + length,
            },
            children: Nodes::default(),
        };
        Nodes(vec![node])
    }

    fn wrap(self, name: String, start: usize) -> Nodes {
        Nodes(vec![Node::with_children(name.to_string(), start, self)])
    }

    fn extend(&mut self, other: Nodes) {
        self.0.extend(other.0)
    }

    fn byte_len(&self) -> usize {
        if self.0.is_empty() {
            0
        } else {
            self.0.last().unwrap().range.end - self.0.first().unwrap().range.start
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
