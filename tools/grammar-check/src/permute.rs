//! Generates test case permutations from grammar productions.
//!
//! This module provides functionality to automatically generate test cases
//! by traversing grammar productions and creating many possible variations.
//!
//! Use the `--permute <NAME>` flag with commands like `lex-compare` to generate
//! test cases from a grammar production. Add `--negative` to also include
//! negative test cases (expected to fail parsing).

use grammar::{Characters, Expression, ExpressionKind, Grammar, RangeLimit};
use std::collections::HashMap;
use std::sync::Arc;

/// Represents a generated permutation with its name and content.
#[derive(Debug, Clone)]
pub struct Permutation {
    /// The name describing this permutation (for test identification).
    pub name: String,
    /// The generated source string.
    pub content: String,
}

/// Placeholder characters for generating negative test cases.
mod placeholders {
    /// A character that is unlikely to match most terminals.
    pub const MISMATCH_CHAR: &str = "\u{FFFD}"; // Unicode replacement character
    /// For XID_Start negative cases (not a valid identifier start).
    pub const NOT_XID_START: &str = "0";
    /// For XID_Continue negative cases.
    pub const NOT_XID_CONTINUE: &str = ".";
}

/// Configuration for permutation generation.
#[derive(Clone)]
pub struct PermuteConfig {
    pub grammar: Arc<Grammar>,
    pub production_name: String,
    pub include_negative: bool,
}

/// A streaming iterator that yields permutations.
pub struct PermutationIterator {
    config: PermuteConfig,
    /// Stack of iterators for generating combinations.
    state: IterState,
    /// Counter for naming permutations.
    positive_count: usize,
    negative_count: usize,
    /// Whether we're in the negative phase.
    in_negative_phase: bool,
}

enum IterState {
    Generating(Box<dyn Iterator<Item = String> + Send>),
    Done,
}

impl PermutationIterator {
    pub fn new(config: PermuteConfig) -> Self {
        let grammar = config.grammar.clone();

        let Some(production) = grammar.productions.get(config.production_name.as_str()) else {
            panic!("unknown production `{}`", &config.production_name);
        };
        let expr = production.expression.clone();
        let iter = Box::new(PermuteExprIter::new(grammar, expr, false));

        Self {
            config,
            state: IterState::Generating(iter),
            positive_count: 0,
            negative_count: 0,
            in_negative_phase: false,
        }
    }
}

impl Iterator for PermutationIterator {
    type Item = Permutation;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match &mut self.state {
                IterState::Generating(iter) => {
                    if let Some(content) = iter.next() {
                        if self.in_negative_phase {
                            let name = format!(
                                "{}::negative::{}",
                                self.config.production_name, self.negative_count
                            );
                            self.negative_count += 1;
                            return Some(Permutation { name, content });
                        } else {
                            let name = format!(
                                "{}::positive::{}",
                                self.config.production_name, self.positive_count
                            );
                            self.positive_count += 1;
                            return Some(Permutation { name, content });
                        }
                    } else if !self.in_negative_phase && self.config.include_negative {
                        // Switch to negative phase.
                        self.in_negative_phase = true;
                        let grammar = self.config.grammar.clone();
                        if let Some(production) =
                            grammar.productions.get(&self.config.production_name)
                        {
                            let expr = production.expression.clone();
                            self.state = IterState::Generating(Box::new(PermuteExprIter::new(
                                grammar, expr, true,
                            )));
                            continue;
                        } else {
                            self.state = IterState::Done;
                            return None;
                        }
                    } else {
                        self.state = IterState::Done;
                        return None;
                    }
                }
                IterState::Done => return None,
            }
        }
    }
}

/// Iterator that generates permutations for an expression lazily.
struct PermuteExprIter {
    #[allow(dead_code)]
    grammar: Arc<Grammar>,
    inner: Box<dyn Iterator<Item = String> + Send>,
}

impl PermuteExprIter {
    fn new(grammar: Arc<Grammar>, expr: Expression, negative: bool) -> Self {
        let inner = build_expr_iter(grammar.clone(), expr, negative, HashMap::new());
        Self { grammar, inner }
    }
}

impl Iterator for PermuteExprIter {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

/// Build an iterator for the given expression.
fn build_expr_iter(
    grammar: Arc<Grammar>,
    expr: Expression,
    negative: bool,
    env: HashMap<String, u32>,
) -> Box<dyn Iterator<Item = String> + Send> {
    if negative {
        build_negative_iter(grammar, expr, env)
    } else {
        build_positive_iter(grammar, expr, env)
    }
}

/// Build a positive permutation iterator for an expression.
fn build_positive_iter(
    grammar: Arc<Grammar>,
    expr: Expression,
    env: HashMap<String, u32>,
) -> Box<dyn Iterator<Item = String> + Send> {
    match expr.kind {
        ExpressionKind::Grouped(inner) => build_positive_iter(grammar, *inner, env),

        ExpressionKind::Alt(alts) => {
            // Chain all alternatives together.
            let iters: Vec<_> = alts
                .into_iter()
                .map(|alt| build_positive_iter(grammar.clone(), alt, env.clone()))
                .collect();
            Box::new(iters.into_iter().flatten())
        }

        ExpressionKind::Sequence(seq) => {
            // Filter out Break and Comment expressions.
            let filtered: Vec<_> = seq
                .into_iter()
                .filter(|e| {
                    !matches!(
                        e.kind,
                        ExpressionKind::Break(_) | ExpressionKind::Comment(_)
                    )
                })
                .collect();

            if filtered.is_empty() {
                return Box::new(std::iter::once(String::new()));
            }

            // Build a cartesian product iterator.
            Box::new(CartesianProductIter::new(grammar, filtered, env))
        }

        ExpressionKind::Optional(inner) => {
            // 0 repetitions (empty), then 1 repetition.
            let inner_iter = build_positive_iter(grammar, *inner, env);
            Box::new(std::iter::once(String::new()).chain(inner_iter))
        }

        ExpressionKind::Not(_inner) => {
            // Not expressions don't produce output in positive cases.
            Box::new(std::iter::once(String::new()))
        }

        ExpressionKind::Repeat(inner) => {
            // 0, 1, or 2 repetitions.
            let inner_expr = *inner;
            Box::new(RepeatIter::new(grammar, inner_expr, env, 0, 2))
        }

        ExpressionKind::RepeatPlus(inner) => {
            // 1 or 2 repetitions.
            let inner_expr = *inner;
            Box::new(RepeatIter::new(grammar, inner_expr, env, 1, 2))
        }

        ExpressionKind::RepeatRange {
            expr: inner,
            name,
            min,
            max,
            limit,
        } => {
            let effective_max = max.map(|m| match limit {
                RangeLimit::HalfOpen => m.saturating_sub(1),
                RangeLimit::Closed => m,
            });

            let min_count = min.unwrap_or(0);
            let max_count = effective_max.unwrap_or(min_count + 2);

            // Generate for min and max counts.
            let mut counts = vec![min_count];
            if max_count != min_count {
                counts.push(max_count);
            }
            // If min is not specified, also generate for count 1.
            if min.is_none() && min_count == 0 && max_count > 1 && !counts.contains(&1) {
                counts.push(1);
            }

            let mut env = env;
            if let Some(name) = name {
                // For simplicity, use min_count in env. A more sophisticated approach
                // would track per-permutation.
                env.insert(name, min_count);
            }

            Box::new(RepeatCountsIter::new(grammar, *inner, env, counts))
        }

        ExpressionKind::RepeatRangeNamed(inner, name) => {
            let count = env.get(&name).copied().unwrap_or(0);
            Box::new(RepeatIter::new(grammar, *inner, env, count, count))
        }

        ExpressionKind::Nt(nt_name) => {
            if let Some(prod) = grammar.productions.get(&nt_name) {
                let prod_expr = prod.expression.clone();
                build_positive_iter(grammar, prod_expr, env)
            } else {
                Box::new(std::iter::once(format!("<{nt_name}>")))
            }
        }

        ExpressionKind::Terminal(s) => Box::new(std::iter::once(s)),

        ExpressionKind::Prose(prose) => {
            let result = match prose.as_str() {
                "`XID_Start` defined by Unicode" => "a".to_string(),
                "`XID_Continue` defined by Unicode" => "b".to_string(),
                s => panic!("prose {s} not supported"),
            };
            Box::new(std::iter::once(result))
        }

        ExpressionKind::Break(_) | ExpressionKind::Comment(_) => {
            Box::new(std::iter::once(String::new()))
        }

        ExpressionKind::Charset(chars) => {
            let iters: Vec<_> = chars
                .into_iter()
                .map(|ch| build_char_iter(grammar.clone(), ch, env.clone()))
                .collect();
            Box::new(iters.into_iter().flatten())
        }

        ExpressionKind::NegExpression(_neg) => {
            // For positive cases, use a placeholder.
            Box::new(std::iter::once("a".to_string()))
        }

        ExpressionKind::Cut(inner) => build_positive_iter(grammar, *inner, env),

        ExpressionKind::Unicode((ch, _)) => Box::new(std::iter::once(ch.to_string())),
    }
}

/// Build an iterator for a Characters element.
fn build_char_iter(
    grammar: Arc<Grammar>,
    ch: Characters,
    env: HashMap<String, u32>,
) -> Box<dyn Iterator<Item = String> + Send> {
    match ch {
        Characters::Named(name) => {
            let prod = grammar.productions.get(&name).unwrap();
            let prod_expr = prod.expression.clone();
            build_positive_iter(grammar, prod_expr, env)
        }
        Characters::Terminal(s) => Box::new(std::iter::once(s)),
        Characters::Range(start, end) => {
            Box::new(vec![start.get_ch().to_string(), end.get_ch().to_string()].into_iter())
        }
    }
}

/// Build a negative permutation iterator for an expression.
fn build_negative_iter(
    grammar: Arc<Grammar>,
    expr: Expression,
    env: HashMap<String, u32>,
) -> Box<dyn Iterator<Item = String> + Send> {
    match expr.kind {
        ExpressionKind::Grouped(inner) => build_negative_iter(grammar, *inner, env),

        ExpressionKind::Alt(alts) => {
            let iters: Vec<_> = alts
                .into_iter()
                .map(|alt| build_negative_iter(grammar.clone(), alt, env.clone()))
                .collect();
            Box::new(iters.into_iter().flatten())
        }

        ExpressionKind::Sequence(seq) => {
            let filtered: Vec<_> = seq
                .into_iter()
                .filter(|e| {
                    !matches!(
                        e.kind,
                        ExpressionKind::Break(_) | ExpressionKind::Comment(_)
                    )
                })
                .collect();

            if filtered.is_empty() {
                return Box::new(std::iter::empty());
            }

            Box::new(SequenceNegativeIter::new(grammar, filtered, env))
        }

        ExpressionKind::Optional(inner) => {
            // Negative cases come from the inner expression's negative cases.
            build_negative_iter(grammar, *inner, env)
        }

        ExpressionKind::Not(inner) => {
            // The negative of Not is when the expression matches.
            build_positive_iter(grammar, *inner, env)
        }

        ExpressionKind::Repeat(inner) => {
            // Malformed repetitions: bad element at position 0 or 1.
            let inner_expr = (*inner).clone();
            let inner_neg = build_negative_iter(grammar.clone(), *inner, env.clone());
            let inner_pos = build_positive_iter(grammar.clone(), inner_expr.clone(), env.clone());

            // After 0 valid: just the negative.
            // After 1 valid: positive + negative.
            let one_then_bad = CartesianProductPairIter::new(
                collect_limited(inner_pos, 100),
                collect_limited(build_negative_iter(grammar, inner_expr, env), 100),
            );

            Box::new(inner_neg.chain(one_then_bad))
        }

        ExpressionKind::RepeatPlus(inner) => {
            let inner_expr = (*inner).clone();
            let inner_neg = build_negative_iter(grammar.clone(), *inner, env.clone());
            let inner_pos = build_positive_iter(grammar.clone(), inner_expr.clone(), env.clone());

            let one_then_bad = CartesianProductPairIter::new(
                collect_limited(inner_pos, 100),
                collect_limited(build_negative_iter(grammar, inner_expr, env), 100),
            );

            Box::new(inner_neg.chain(one_then_bad))
        }

        ExpressionKind::RepeatRange {
            expr: inner, min, ..
        } => {
            let min_count = min.unwrap_or(0);
            let inner_expr = (*inner).clone();
            let inner_neg = build_negative_iter(grammar.clone(), *inner, env.clone());

            let mut results: Vec<String> = Vec::new();

            // If min > 0, fewer than min is a failure.
            if min_count > 0 {
                results.push(String::new());
                if min_count > 1 {
                    let prefix: Vec<_> =
                        build_positive_iter(grammar.clone(), inner_expr.clone(), env.clone())
                            .take(10)
                            .collect();
                    for p in repeat_n_cartesian(&prefix, (min_count - 1) as usize).take(100) {
                        results.push(p);
                    }
                }
            }

            Box::new(results.into_iter().chain(inner_neg))
        }

        ExpressionKind::RepeatRangeNamed(inner, name) => {
            let count = env.get(&name).copied().unwrap_or(0);
            let inner_neg = build_negative_iter(grammar.clone(), *inner, env.clone());

            if count > 0 {
                Box::new(inner_neg)
            } else {
                Box::new(std::iter::empty())
            }
        }

        ExpressionKind::Nt(nt_name) => {
            let prod = grammar.productions.get(&nt_name).unwrap();
            let prod_expr = prod.expression.clone();
            build_negative_iter(grammar, prod_expr, env)
        }

        ExpressionKind::Terminal(s) => {
            let mut results = vec![String::new()];
            if !s.is_empty() {
                results.push(placeholders::MISMATCH_CHAR.to_string());
                if s.len() == 1 {
                    let ch = s.chars().next().unwrap();
                    let diff = if ch == 'x' { 'y' } else { 'x' };
                    results.push(diff.to_string());
                } else {
                    results.push(s[..s.len() - 1].to_string());
                }
            }
            Box::new(results.into_iter())
        }

        ExpressionKind::Prose(prose) => {
            let results = match prose.as_str() {
                "`XID_Start` defined by Unicode" => vec![
                    placeholders::NOT_XID_START.to_string(),
                    " ".to_string(),
                    ".".to_string(),
                ],
                "`XID_Continue` defined by Unicode" => vec![
                    placeholders::NOT_XID_CONTINUE.to_string(),
                    ".".to_string(),
                    "!".to_string(),
                ],
                _ => vec![String::new()],
            };
            Box::new(results.into_iter())
        }

        ExpressionKind::Break(_) | ExpressionKind::Comment(_) => Box::new(std::iter::empty()),

        ExpressionKind::Charset(_chars) => {
            Box::new(vec![placeholders::MISMATCH_CHAR.to_string(), String::new()].into_iter())
        }

        ExpressionKind::NegExpression(neg) => build_positive_iter(grammar, *neg, env),

        ExpressionKind::Cut(inner) => build_negative_iter(grammar, *inner, env),

        ExpressionKind::Unicode((ch, _)) => {
            let diff = if ch == 'a' { "b" } else { "a" };
            Box::new(
                vec![
                    String::new(),
                    diff.to_string(),
                    placeholders::MISMATCH_CHAR.to_string(),
                ]
                .into_iter(),
            )
        }
    }
}

/// Collect up to `limit` items from an iterator.
fn collect_limited<I: Iterator<Item = String>>(iter: I, limit: usize) -> Vec<String> {
    iter.take(limit).collect()
}

/// Cartesian product iterator for a sequence of expressions.
struct CartesianProductIter {
    grammar: Arc<Grammar>,
    expressions: Vec<Expression>,
    env: HashMap<String, u32>,
    /// Current values for each position.
    current_values: Vec<Vec<String>>,
    /// Current indices for each position.
    indices: Vec<usize>,
    /// Whether we've started iterating.
    started: bool,
    /// Whether we're done.
    done: bool,
}

impl CartesianProductIter {
    fn new(grammar: Arc<Grammar>, expressions: Vec<Expression>, env: HashMap<String, u32>) -> Self {
        let n = expressions.len();
        Self {
            grammar,
            expressions,
            env,
            current_values: Vec::new(),
            indices: vec![0; n],
            started: false,
            done: false,
        }
    }

    fn initialize(&mut self) {
        // Collect all values for each expression (limited to avoid memory explosion).
        for expr in &self.expressions {
            let iter = build_positive_iter(self.grammar.clone(), expr.clone(), self.env.clone());
            let values: Vec<_> = iter.take(1000).collect();
            if values.is_empty() {
                self.done = true;
                return;
            }
            self.current_values.push(values);
        }
        self.started = true;
    }

    fn current_string(&self) -> String {
        let mut result = String::new();
        for (i, idx) in self.indices.iter().enumerate() {
            result.push_str(&self.current_values[i][*idx]);
        }
        result
    }

    fn advance(&mut self) -> bool {
        // Advance indices like a multi-digit counter.
        for i in (0..self.indices.len()).rev() {
            self.indices[i] += 1;
            if self.indices[i] < self.current_values[i].len() {
                return true;
            }
            self.indices[i] = 0;
        }
        false
    }
}

impl Iterator for CartesianProductIter {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        if !self.started {
            self.initialize();
            if self.done {
                return None;
            }
            return Some(self.current_string());
        }

        if self.advance() {
            Some(self.current_string())
        } else {
            self.done = true;
            None
        }
    }
}

/// Cartesian product of two pre-computed vectors.
struct CartesianProductPairIter {
    a: Vec<String>,
    b: Vec<String>,
    i: usize,
    j: usize,
}

impl CartesianProductPairIter {
    fn new(a: Vec<String>, b: Vec<String>) -> Self {
        Self { a, b, i: 0, j: 0 }
    }
}

impl Iterator for CartesianProductPairIter {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        if self.a.is_empty() || self.b.is_empty() {
            return None;
        }
        if self.i >= self.a.len() {
            return None;
        }

        let result = format!("{}{}", self.a[self.i], self.b[self.j]);

        self.j += 1;
        if self.j >= self.b.len() {
            self.j = 0;
            self.i += 1;
        }

        Some(result)
    }
}

/// Iterator for repeat expressions with a range of counts.
struct RepeatIter {
    grammar: Arc<Grammar>,
    expr: Expression,
    env: HashMap<String, u32>,
    max_count: u32,
    current_count: u32,
    current_iter: Option<Box<dyn Iterator<Item = String> + Send>>,
}

impl RepeatIter {
    fn new(
        grammar: Arc<Grammar>,
        expr: Expression,
        env: HashMap<String, u32>,
        min_count: u32,
        max_count: u32,
    ) -> Self {
        Self {
            grammar,
            expr,
            env,
            max_count,
            current_count: min_count,
            current_iter: None,
        }
    }

    fn build_iter_for_count(&self, count: u32) -> Box<dyn Iterator<Item = String> + Send> {
        if count == 0 {
            return Box::new(std::iter::once(String::new()));
        }

        let base_values: Vec<_> =
            build_positive_iter(self.grammar.clone(), self.expr.clone(), self.env.clone())
                .take(100)
                .collect();

        if base_values.is_empty() {
            return Box::new(std::iter::empty());
        }

        Box::new(repeat_n_cartesian(&base_values, count as usize))
    }
}

impl Iterator for RepeatIter {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(ref mut iter) = self.current_iter {
                if let Some(val) = iter.next() {
                    return Some(val);
                }
            }

            if self.current_count > self.max_count {
                return None;
            }

            self.current_iter = Some(self.build_iter_for_count(self.current_count));
            self.current_count += 1;
        }
    }
}

/// Iterator for repeat with specific counts.
struct RepeatCountsIter {
    grammar: Arc<Grammar>,
    expr: Expression,
    env: HashMap<String, u32>,
    counts: Vec<u32>,
    count_idx: usize,
    current_iter: Option<Box<dyn Iterator<Item = String> + Send>>,
}

impl RepeatCountsIter {
    fn new(
        grammar: Arc<Grammar>,
        expr: Expression,
        env: HashMap<String, u32>,
        counts: Vec<u32>,
    ) -> Self {
        Self {
            grammar,
            expr,
            env,
            counts,
            count_idx: 0,
            current_iter: None,
        }
    }

    fn build_iter_for_count(&self, count: u32) -> Box<dyn Iterator<Item = String> + Send> {
        if count == 0 {
            return Box::new(std::iter::once(String::new()));
        }

        let base_values: Vec<_> =
            build_positive_iter(self.grammar.clone(), self.expr.clone(), self.env.clone())
                .take(100)
                .collect();

        if base_values.is_empty() {
            return Box::new(std::iter::empty());
        }

        Box::new(repeat_n_cartesian(&base_values, count as usize))
    }
}

impl Iterator for RepeatCountsIter {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(ref mut iter) = self.current_iter {
                if let Some(val) = iter.next() {
                    return Some(val);
                }
            }

            if self.count_idx >= self.counts.len() {
                return None;
            }

            let count = self.counts[self.count_idx];
            self.current_iter = Some(self.build_iter_for_count(count));
            self.count_idx += 1;
        }
    }
}

/// Iterator for n-way cartesian product of the same set.
fn repeat_n_cartesian(values: &[String], n: usize) -> Box<dyn Iterator<Item = String> + Send> {
    let values = values.to_vec();
    let len = values.len();

    if n == 0 {
        return Box::new(std::iter::once(String::new()));
    }

    let total = len.saturating_pow(n as u32);
    let values_clone = values.clone();

    Box::new((0..total).map(move |mut idx| {
        let mut result = String::new();
        for _ in 0..n {
            let val_idx = idx % len;
            // Build in reverse order, then reverse the whole string.
            result = format!("{}{}", values_clone[val_idx], result);
            idx /= len;
        }
        result
    }))
}

/// Iterator for generating negative permutations of a sequence.
struct SequenceNegativeIter {
    grammar: Arc<Grammar>,
    expressions: Vec<Expression>,
    env: HashMap<String, u32>,
    /// Current fail position (which element is failing).
    fail_pos: usize,
    /// Cached positive values for each expression.
    positive_values: Vec<Vec<String>>,
    /// Current iterator for the current fail position.
    current_iter: Option<Box<dyn Iterator<Item = String> + Send>>,
    /// Whether we've initialized.
    initialized: bool,
}

impl SequenceNegativeIter {
    fn new(grammar: Arc<Grammar>, expressions: Vec<Expression>, env: HashMap<String, u32>) -> Self {
        Self {
            grammar,
            expressions,
            env,
            fail_pos: 0,
            positive_values: Vec::new(),
            current_iter: None,
            initialized: false,
        }
    }

    fn initialize(&mut self) {
        // Cache positive values for each expression.
        for expr in &self.expressions {
            let iter = build_positive_iter(self.grammar.clone(), expr.clone(), self.env.clone());
            let values: Vec<_> = iter.take(100).collect();
            self.positive_values.push(values);
        }
        self.initialized = true;
    }

    fn build_iter_for_fail_pos(&self) -> Box<dyn Iterator<Item = String> + Send> {
        if self.fail_pos >= self.expressions.len() {
            return Box::new(std::iter::empty());
        }

        // Build prefix: cartesian product of positions 0..fail_pos.
        let prefix_values = if self.fail_pos == 0 {
            vec![String::new()]
        } else {
            let mut result = self.positive_values[0].clone();
            for i in 1..self.fail_pos {
                let next = &self.positive_values[i];
                result = cartesian_product_vec(&result, next);
                // Limit to avoid explosion.
                if result.len() > 1000 {
                    result.truncate(1000);
                }
            }
            result
        };

        // Get negative permutations for the failing element.
        let fail_expr = &self.expressions[self.fail_pos];

        // Handle Not expressions specially.
        let fail_iter: Box<dyn Iterator<Item = String> + Send> =
            if let ExpressionKind::Not(inner) = &fail_expr.kind {
                build_positive_iter(self.grammar.clone(), (**inner).clone(), self.env.clone())
            } else {
                build_negative_iter(self.grammar.clone(), fail_expr.clone(), self.env.clone())
            };

        let fail_values: Vec<_> = fail_iter.take(100).collect();

        Box::new(CartesianProductPairIter::new(prefix_values, fail_values))
    }
}

impl Iterator for SequenceNegativeIter {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.initialized {
            self.initialize();
        }

        loop {
            if let Some(ref mut iter) = self.current_iter {
                if let Some(val) = iter.next() {
                    return Some(val);
                }
            }

            if self.fail_pos >= self.expressions.len() {
                return None;
            }

            self.current_iter = Some(self.build_iter_for_fail_pos());
            self.fail_pos += 1;
        }
    }
}

/// Compute cartesian product of two vectors.
fn cartesian_product_vec(a: &[String], b: &[String]) -> Vec<String> {
    let mut result = Vec::with_capacity(a.len() * b.len());
    for s1 in a {
        for s2 in b {
            result.push(format!("{s1}{s2}"));
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cartesian_product_pair() {
        let a = vec!["a".to_string(), "b".to_string()];
        let b = vec!["1".to_string(), "2".to_string()];
        let result: Vec<_> = CartesianProductPairIter::new(a, b).collect();
        assert_eq!(result, vec!["a1", "a2", "b1", "b2"]);
    }

    #[test]
    fn test_repeat_n_cartesian() {
        let values = vec!["a".to_string(), "b".to_string()];
        let result: Vec<_> = repeat_n_cartesian(&values, 2).collect();
        assert_eq!(result, vec!["aa", "ab", "ba", "bb"]);
    }
}
