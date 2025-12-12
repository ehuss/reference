//! Generates test case permutations from grammar productions.
//!
//! This module provides functionality to automatically generate test cases
//! by traversing grammar productions and creating many possible variations.
//!
//! Use the `--permute <NAME>` flag with commands like `lex-compare` to generate
//! test cases from a grammar production. Both positive permutations (valid inputs)
//! and negative permutations (invalid inputs) are generated.

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
                    // } else if !self.in_negative_phase {
                    //     // Switch to negative phase.
                    //     self.in_negative_phase = true;
                    //     let grammar = self.config.grammar.clone();
                    //     if let Some(production) =
                    //         grammar.productions.get(&self.config.production_name)
                    //     {
                    //         let expr = production.expression.clone();
                    //         self.state = IterState::Generating(Box::new(PermuteExprIter::new(
                    //             grammar, expr, true,
                    //         )));
                    //         continue;
                    //     } else {
                    //         self.state = IterState::Done;
                    //         return None;
                    //     }
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

        ExpressionKind::NegativeLookahead(_inner) => {
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
            name: _,
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

            //TODO: This is supposed to be storing the count in the environment

            // Generate for min and max counts.
            let mut counts = vec![min_count];
            if max_count != min_count {
                counts.push(max_count);
            }
            // If min is not specified, also generate for count 1.
            if min.is_none() && min_count == 0 && max_count > 1 && !counts.contains(&1) {
                counts.push(1);
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

        ExpressionKind::NegativeLookahead(inner) => {
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

/// Build a fair iterator that samples from all alternatives in an Alt expression.
/// This ensures all alternatives get representation, not just the first one.
fn build_fair_iter(
    grammar: &Arc<Grammar>,
    expr: &Expression,
    env: &HashMap<String, u32>,
) -> Box<dyn Iterator<Item = String> + Send> {
    // Unwrap Grouped expressions to find the inner Alt.
    let inner_expr = unwrap_to_alt(expr);

    // If this is an Alt, interleave values from each alternative.
    if let ExpressionKind::Alt(ref alts) = inner_expr.kind {
        Box::new(InterleavingAltIter::new(grammar.clone(), alts.clone(), env.clone()))
    } else {
        build_positive_iter(grammar.clone(), expr.clone(), env.clone())
    }
}

/// Iterator that interleaves values from multiple alternatives fairly.
struct InterleavingAltIter {
    iterators: Vec<Box<dyn Iterator<Item = String> + Send>>,
    current_idx: usize,
    exhausted: Vec<bool>,
    all_exhausted: bool,
}

impl InterleavingAltIter {
    fn new(grammar: Arc<Grammar>, alts: Vec<Expression>, env: HashMap<String, u32>) -> Self {
        let iterators: Vec<_> = alts
            .into_iter()
            .map(|alt| build_positive_iter(grammar.clone(), alt, env.clone()))
            .collect();
        let exhausted = vec![false; iterators.len()];
        Self {
            iterators,
            current_idx: 0,
            exhausted,
            all_exhausted: false,
        }
    }
}

impl Iterator for InterleavingAltIter {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        if self.all_exhausted || self.iterators.is_empty() {
            return None;
        }

        let start_idx = self.current_idx;
        loop {
            if !self.exhausted[self.current_idx] {
                if let Some(val) = self.iterators[self.current_idx].next() {
                    self.current_idx = (self.current_idx + 1) % self.iterators.len();
                    return Some(val);
                } else {
                    self.exhausted[self.current_idx] = true;
                }
            }

            self.current_idx = (self.current_idx + 1) % self.iterators.len();

            // If we've gone full circle, check if all are exhausted.
            if self.current_idx == start_idx {
                if self.exhausted.iter().all(|&e| e) {
                    self.all_exhausted = true;
                    return None;
                }
            }
        }
    }
}

/// Unwrap Grouped expressions to find an inner Alt, if present.
fn unwrap_to_alt(expr: &Expression) -> &Expression {
    match &expr.kind {
        ExpressionKind::Grouped(inner) => unwrap_to_alt(inner),
        _ => expr,
    }
}

/// Cartesian product iterator for a sequence of expressions.
/// This streams results without collecting all values into memory.
struct CartesianProductIter {
    grammar: Arc<Grammar>,
    expressions: Vec<Expression>,
    env: HashMap<String, u32>,
    /// Current value at each position (accumulated from iterators).
    current_values: Vec<String>,
    /// Iterator for each position.
    iterators: Vec<Box<dyn Iterator<Item = String> + Send>>,
    /// Whether we've started iterating.
    started: bool,
    /// Whether we're done.
    done: bool,
    /// Named ranges and their possible counts.
    named_range_counts: HashMap<String, Vec<u32>>,
    /// Current index into each named range's counts.
    named_range_count_idx: HashMap<String, usize>,
    /// Current env with named range values set.
    current_named_env: HashMap<String, u32>,
}

impl CartesianProductIter {
    fn new(grammar: Arc<Grammar>, expressions: Vec<Expression>, env: HashMap<String, u32>) -> Self {
        // Pre-scan for RepeatRange with names and collect their possible counts.
        // We need to handle these specially so RepeatRangeNamed can coordinate.
        let mut named_range_counts: HashMap<String, Vec<u32>> = HashMap::new();
        for expr in &expressions {
            if let ExpressionKind::RepeatRange {
                name: Some(ref name),
                min,
                max,
                limit,
                ..
            } = expr.kind
            {
                let effective_max = max.map(|m| match limit {
                    RangeLimit::HalfOpen => m.saturating_sub(1),
                    RangeLimit::Closed => m,
                });
                let min_count = min.unwrap_or(0);
                let max_count = effective_max.unwrap_or(min_count + 2);

                let mut counts = vec![min_count];
                if max_count != min_count {
                    counts.push(max_count);
                }
                if min.is_none() && min_count == 0 && max_count > 1 && !counts.contains(&1) {
                    counts.push(1);
                }
                named_range_counts.insert(name.clone(), counts);
            }
        }

        let n = expressions.len();
        Self {
            grammar,
            expressions,
            env,
            current_values: vec![String::new(); n],
            iterators: Vec::new(),
            started: false,
            done: false,
            named_range_counts,
            named_range_count_idx: HashMap::new(),
            current_named_env: HashMap::new(),
        }
    }

    fn initialize(&mut self) {
        // Initialize named range count indices.
        for name in self.named_range_counts.keys() {
            self.named_range_count_idx.insert(name.clone(), 0);
        }
        self.setup_for_current_named_counts();
    }

    fn setup_for_current_named_counts(&mut self) {
        // Build the env with current named range values.
        self.current_named_env = self.env.clone();
        for (name, counts) in &self.named_range_counts {
            let idx = self.named_range_count_idx.get(name).copied().unwrap_or(0);
            if idx < counts.len() {
                self.current_named_env.insert(name.clone(), counts[idx]);
            }
        }

        // Create fresh iterators for all positions and get first value from each.
        self.iterators.clear();
        self.current_values = vec![String::new(); self.expressions.len()];

        for (i, expr) in self.expressions.iter().enumerate() {
            let mut iter = build_fair_iter(&self.grammar, expr, &self.current_named_env);
            if let Some(val) = iter.next() {
                self.current_values[i] = val;
                self.iterators.push(iter);
            } else {
                // This expression produces no values, try next named range combo.
                if !self.advance_named_counts() {
                    self.done = true;
                    return;
                }
                return self.setup_for_current_named_counts();
            }
        }
        self.started = true;
    }

    fn advance_named_counts(&mut self) -> bool {
        // Advance the named range count indices like a multi-digit counter.
        let names: Vec<_> = self.named_range_counts.keys().cloned().collect();
        for name in names.iter().rev() {
            let counts = &self.named_range_counts[name];
            let idx = self.named_range_count_idx.get_mut(name).unwrap();
            *idx += 1;
            if *idx < counts.len() {
                return true;
            }
            *idx = 0;
        }
        false
    }

    fn current_string(&self) -> String {
        self.current_values.concat()
    }

    /// Advance to the next combination. Returns true if successful.
    fn advance(&mut self) -> bool {
        // Try to advance from the rightmost position.
        for i in (0..self.expressions.len()).rev() {
            if let Some(val) = self.iterators[i].next() {
                self.current_values[i] = val;
                return true;
            }
            // This iterator exhausted, reset it and try the next position.
            let mut new_iter = build_fair_iter(
                &self.grammar,
                &self.expressions[i],
                &self.current_named_env,
            );
            if let Some(val) = new_iter.next() {
                self.current_values[i] = val;
                self.iterators[i] = new_iter;
            } else {
                // Should not happen if setup succeeded, but handle gracefully.
                return false;
            }
        }
        // All positions wrapped around, we're done with this combination.
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
            return Some(self.current_string());
        }

        // Current cartesian product exhausted, try next named count combination.
        if self.advance_named_counts() {
            self.started = false;
            self.setup_for_current_named_counts();
            if self.done {
                return None;
            }
            return Some(self.current_string());
        }

        self.done = true;
        None
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

        // Build a streaming cartesian product of `count` copies of the expression.
        Box::new(RepeatNIter::new(
            self.grammar.clone(),
            self.expr.clone(),
            self.env.clone(),
            count as usize,
        ))
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

/// Streaming iterator for N repetitions of an expression (cartesian product).
struct RepeatNIter {
    grammar: Arc<Grammar>,
    expr: Expression,
    env: HashMap<String, u32>,
    n: usize,
    /// Current value at each of the N positions.
    current_values: Vec<String>,
    /// Iterator for each of the N positions.
    iterators: Vec<Box<dyn Iterator<Item = String> + Send>>,
    started: bool,
    done: bool,
}

impl RepeatNIter {
    fn new(grammar: Arc<Grammar>, expr: Expression, env: HashMap<String, u32>, n: usize) -> Self {
        Self {
            grammar,
            expr,
            env,
            n,
            current_values: vec![String::new(); n],
            iterators: Vec::new(),
            started: false,
            done: false,
        }
    }

    fn initialize(&mut self) {
        for i in 0..self.n {
            let mut iter = build_fair_iter(&self.grammar, &self.expr, &self.env);
            if let Some(val) = iter.next() {
                self.current_values[i] = val;
                self.iterators.push(iter);
            } else {
                self.done = true;
                return;
            }
        }
        self.started = true;
    }

    fn advance(&mut self) -> bool {
        for i in (0..self.n).rev() {
            if let Some(val) = self.iterators[i].next() {
                self.current_values[i] = val;
                return true;
            }
            // Reset this iterator and try advancing the next position.
            let mut new_iter = build_fair_iter(&self.grammar, &self.expr, &self.env);
            if let Some(val) = new_iter.next() {
                self.current_values[i] = val;
                self.iterators[i] = new_iter;
            } else {
                return false;
            }
        }
        false
    }
}

impl Iterator for RepeatNIter {
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
            return Some(self.current_values.concat());
        }

        if self.advance() {
            return Some(self.current_values.concat());
        }

        self.done = true;
        None
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

        // Build a streaming cartesian product of `count` copies of the expression.
        Box::new(RepeatNIter::new(
            self.grammar.clone(),
            self.expr.clone(),
            self.env.clone(),
            count as usize,
        ))
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
    /// Current iterator for the current fail position.
    current_iter: Option<Box<dyn Iterator<Item = String> + Send>>,
}

impl SequenceNegativeIter {
    fn new(grammar: Arc<Grammar>, expressions: Vec<Expression>, env: HashMap<String, u32>) -> Self {
        Self {
            grammar,
            expressions,
            env,
            fail_pos: 0,
            current_iter: None,
        }
    }

    fn build_iter_for_fail_pos(&self) -> Box<dyn Iterator<Item = String> + Send> {
        if self.fail_pos >= self.expressions.len() {
            return Box::new(std::iter::empty());
        }

        // Get negative permutations for the failing element.
        let fail_expr = &self.expressions[self.fail_pos];

        // Handle Not expressions specially.
        let fail_iter: Box<dyn Iterator<Item = String> + Send> =
            if let ExpressionKind::NegativeLookahead(inner) = &fail_expr.kind {
                build_positive_iter(self.grammar.clone(), (**inner).clone(), self.env.clone())
            } else {
                build_negative_iter(self.grammar.clone(), fail_expr.clone(), self.env.clone())
            };

        if self.fail_pos == 0 {
            // No prefix needed.
            return fail_iter;
        }

        // Build a streaming prefix iterator using cartesian product of positions 0..fail_pos.
        let prefix_exprs: Vec<_> = self.expressions[0..self.fail_pos].to_vec();
        let prefix_iter = CartesianProductIter::new(
            self.grammar.clone(),
            prefix_exprs,
            self.env.clone(),
        );

        // Stream the cartesian product of prefix × fail.
        Box::new(StreamingPairIter::new(prefix_iter, fail_iter, self.grammar.clone(), fail_expr.clone(), self.env.clone()))
    }
}

/// Streaming cartesian product of two iterators where the second can be recreated.
struct StreamingPairIter {
    a_iter: Box<dyn Iterator<Item = String> + Send>,
    current_a: Option<String>,
    b_iter: Box<dyn Iterator<Item = String> + Send>,
    grammar: Arc<Grammar>,
    b_expr: Expression,
    env: HashMap<String, u32>,
    done: bool,
}

impl StreamingPairIter {
    fn new(
        a_iter: impl Iterator<Item = String> + Send + 'static,
        b_iter: Box<dyn Iterator<Item = String> + Send>,
        grammar: Arc<Grammar>,
        b_expr: Expression,
        env: HashMap<String, u32>,
    ) -> Self {
        let mut a_iter = Box::new(a_iter) as Box<dyn Iterator<Item = String> + Send>;
        let current_a = a_iter.next();
        let done = current_a.is_none();
        Self {
            a_iter,
            current_a,
            b_iter,
            grammar,
            b_expr,
            env,
            done,
        }
    }

    fn create_b_iter(&self) -> Box<dyn Iterator<Item = String> + Send> {
        if let ExpressionKind::NegativeLookahead(inner) = &self.b_expr.kind {
            build_positive_iter(self.grammar.clone(), (**inner).clone(), self.env.clone())
        } else {
            build_negative_iter(self.grammar.clone(), self.b_expr.clone(), self.env.clone())
        }
    }
}

impl Iterator for StreamingPairIter {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        loop {
            if let Some(ref a) = self.current_a {
                if let Some(b) = self.b_iter.next() {
                    return Some(format!("{}{}", a, b));
                }
                // b exhausted, advance a and reset b.
                self.current_a = self.a_iter.next();
                if self.current_a.is_some() {
                    self.b_iter = self.create_b_iter();
                    continue;
                }
            }
            self.done = true;
            return None;
        }
    }
}

impl Iterator for SequenceNegativeIter {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
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
