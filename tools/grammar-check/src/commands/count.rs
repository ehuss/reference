use clap::ArgMatches;
use diagnostics::Diagnostics;
use grammar::{Expression, ExpressionKind, Grammar};
use tracing::instrument;

pub fn count(matches: &ArgMatches) {
    let prod_name = matches.get_one::<String>("production").unwrap();
    let mut diag = Diagnostics::new();
    let grammar = grammar::load_grammar(&mut diag);
    let count = count_expr(
        &grammar,
        &grammar
            .productions
            .get(prod_name.as_str())
            .unwrap()
            .expression,
    );
    eprintln!("total for {prod_name}: {count}");
}

#[instrument(level = "debug", skip(grammar), ret)]
fn count_expr(grammar: &Grammar, expr: &Expression) -> u64 {
    match &expr.kind {
        ExpressionKind::Grouped(e) => count_expr(grammar, e),
        ExpressionKind::Alt(es) => es.iter().map(|e| count_expr(grammar, e)).sum(),
        ExpressionKind::Sequence(es) => es.iter().map(|e| count_expr(grammar, e).max(1)).product(),
        ExpressionKind::Optional(e) => count_expr(grammar, e) + 1,
        ExpressionKind::NegativeLookahead(_) => 1,
        ExpressionKind::Repeat(e) => count_expr(grammar, e) * 2 + 1,
        ExpressionKind::RepeatPlus(e) => count_expr(grammar, e) * 2,
        ExpressionKind::RepeatRange { expr, .. } => count_expr(grammar, expr) * 2,
        ExpressionKind::RepeatRangeNamed(_, _) => 1,
        ExpressionKind::Nt(nt) => {
            count_expr(grammar, &grammar.productions.get(nt).unwrap().expression)
        }

        ExpressionKind::Terminal(_) => 1,
        ExpressionKind::Prose(_) => 3,
        ExpressionKind::Break(_) => 0,
        ExpressionKind::Comment(_) => 0,
        ExpressionKind::Charset(chars) => chars.iter().map(|ch| count_expr(grammar, ch)).sum(),
        ExpressionKind::CharacterRange(_, _) => 2,
        ExpressionKind::NegExpression(_) => 1,
        ExpressionKind::Cut(e) => count_expr(grammar, e),
        ExpressionKind::Unicode(_) => 1,
    }
}
