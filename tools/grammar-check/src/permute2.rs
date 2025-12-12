#![allow(unused)]
use grammar::{Characters, Expression, ExpressionKind, Grammar, RangeLimit};
use std::collections::HashMap;

/// Represents a generated permutation with its name and content.
#[derive(Debug, Clone)]
pub struct Permutation {
    /// The name describing this permutation (for test identification).
    pub name: String,
    /// The generated source string.
    pub content: String,
}

pub struct PermutationIterator<'g> {
    pub grammar: &'g Grammar,
    name_context: HashMap<String, usize>,
    state: IteratorState<'g>,
}

enum IteratorState<'g> {
    Terminal {
        value: String,
        done: bool,
    },
    Seq {
        exprs: Vec<&'g Expression>,
        iterators: Vec<PermutationIterator<'g>>,
        current_values: Vec<String>,
        indices: Vec<usize>,
        exhausted: bool,
    },
    SeqWithNamedRanges {
        exprs: Vec<&'g Expression>,
        named_range_indices: Vec<(usize, String, usize, usize)>, // (index, name, min, max)
        current_named_values: HashMap<String, usize>,
        iterators: Vec<PermutationIterator<'g>>,
        current_values: Vec<String>,
        exhausted: bool,
    },
    Alt {
        iterators: Vec<PermutationIterator<'g>>,
        current_index: usize,
    },
    Optional {
        iterator: Box<PermutationIterator<'g>>,
        emitted_empty: bool,
    },
    Repeat {
        expr: &'g Expression,
        include_empty: bool,
        current_stage: usize, // 0 = empty (if include_empty), 1 = single, 2 = double
        iterator: Option<Box<PermutationIterator<'g>>>,
        pending_double: Option<String>, // Current value being repeated for stage 2
    },
    RepeatRange {
        expr: &'g Expression,
        min: usize,
        max: usize,
        current_count: usize,
        iterator: Option<Box<PermutationIterator<'g>>>,
        pending_repeat: Option<(String, usize)>, // (value, times_left_to_emit)
    },
}

impl<'g> PermutationIterator<'g> {
    pub fn new(grammar: &'g Grammar, expression: &'g Expression) -> PermutationIterator<'g> {
        Self::new_with_context(grammar, expression, HashMap::new())
    }

    fn new_with_context(
        grammar: &'g Grammar,
        expression: &'g Expression,
        name_context: HashMap<String, usize>,
    ) -> PermutationIterator<'g> {
        let state = match &expression.kind {
            ExpressionKind::Alt(exprs) => {
                let iterators: Vec<_> = exprs
                    .iter()
                    .map(|e| Self::new_with_context(grammar, e, name_context.clone()))
                    .collect();
                IteratorState::Alt {
                    iterators,
                    current_index: 0,
                }
            }
            ExpressionKind::Grouped(expr) => {
                return Self::new_with_context(grammar, expr, name_context);
            }
            ExpressionKind::Sequence(exprs) => {
                if exprs.is_empty() {
                    IteratorState::Terminal {
                        value: String::new(),
                        done: false,
                    }
                } else {
                    let filtered_exprs: Vec<&Expression> = exprs
                        .iter()
                        .filter(|e| {
                            !matches!(
                                e.kind,
                                ExpressionKind::Break(_) | ExpressionKind::Comment(_)
                            )
                        })
                        .collect();

                    // Check if any expressions are named repeat ranges
                    let mut named_range_indices = Vec::new();
                    for (idx, expr) in filtered_exprs.iter().enumerate() {
                        if let ExpressionKind::RepeatRange {
                            name: Some(name),
                            min,
                            max,
                            limit,
                            ..
                        } = &expr.kind
                        {
                            let min_count = min.unwrap_or(0) as usize;
                            let max_count = match max {
                                Some(m) => match limit {
                                    RangeLimit::HalfOpen => *m as usize,
                                    RangeLimit::Closed => (*m + 1) as usize,
                                },
                                None => min_count + 3,
                            };
                            named_range_indices.push((idx, name.clone(), min_count, max_count));
                        }
                    }

                    if named_range_indices.is_empty() {
                        // No named ranges, use regular Seq
                        let iterators: Vec<_> = filtered_exprs
                            .iter()
                            .map(|e| Self::new_with_context(grammar, e, name_context.clone()))
                            .collect();
                        let current_values: Vec<_> =
                            iterators.iter().map(|_| String::new()).collect();
                        let indices = vec![0; iterators.len()];

                        IteratorState::Seq {
                            exprs: filtered_exprs,
                            iterators,
                            current_values,
                            indices,
                            exhausted: false,
                        }
                    } else {
                        // Has named ranges, use special handling
                        let current_named_values: HashMap<String, usize> = named_range_indices
                            .iter()
                            .map(|(_, name, min, _)| (name.clone(), *min))
                            .collect();

                        IteratorState::SeqWithNamedRanges {
                            exprs: filtered_exprs,
                            named_range_indices,
                            current_named_values,
                            iterators: Vec::new(),
                            current_values: Vec::new(),
                            exhausted: false,
                        }
                    }
                }
            }
            ExpressionKind::Optional(expr) => {
                let iterator =
                    Box::new(Self::new_with_context(grammar, expr, name_context.clone()));
                IteratorState::Optional {
                    iterator,
                    emitted_empty: false,
                }
            }
            ExpressionKind::NegativeLookahead(expr) => IteratorState::Terminal {
                value: String::new(),
                done: false,
            },
            ExpressionKind::Repeat(expr) => IteratorState::Repeat {
                expr,
                include_empty: true,
                current_stage: 0,
                iterator: None,
                pending_double: None,
            },
            ExpressionKind::RepeatPlus(expr) => {
                IteratorState::Repeat {
                    expr,
                    include_empty: false,
                    current_stage: 1, // Skip stage 0 (empty)
                    iterator: None,
                    pending_double: None,
                }
            }
            ExpressionKind::RepeatRange {
                expr,
                name,
                min,
                max,
                limit,
            } => {
                // If this has a name and it's in the context, use that specific count
                if let Some(name) = name {
                    if let Some(&count) = name_context.get(name) {
                        // Use the specified count from context
                        IteratorState::RepeatRange {
                            expr,
                            min: count,
                            max: count + 1,
                            current_count: count,
                            iterator: None,
                            pending_repeat: None,
                        }
                    } else {
                        // Name not in context yet, this shouldn't happen in SeqWithNamedRanges
                        // but handle it anyway
                        let min_count = min.unwrap_or(0) as usize;
                        let max_count = match max {
                            Some(m) => match limit {
                                RangeLimit::HalfOpen => *m as usize,
                                RangeLimit::Closed => (*m + 1) as usize,
                            },
                            None => min_count + 3,
                        };
                        let start_count = if min_count == 0 { 0 } else { min_count };
                        IteratorState::RepeatRange {
                            expr,
                            min: min_count,
                            max: max_count,
                            current_count: start_count,
                            iterator: None,
                            pending_repeat: None,
                        }
                    }
                } else {
                    // No name, normal behavior
                    let min_count = min.unwrap_or(0) as usize;
                    let max_count = match max {
                        Some(m) => match limit {
                            RangeLimit::HalfOpen => *m as usize,
                            RangeLimit::Closed => (*m + 1) as usize,
                        },
                        None => min_count + 3,
                    };
                    let start_count = if min_count == 0 { 0 } else { min_count };
                    IteratorState::RepeatRange {
                        expr,
                        min: min_count,
                        max: max_count,
                        current_count: start_count,
                        iterator: None,
                        pending_repeat: None,
                    }
                }
            }
            ExpressionKind::RepeatRangeNamed(expr, name) => {
                // Look up the count from the context
                let count = name_context.get(name).copied().unwrap_or(1);
                IteratorState::RepeatRange {
                    expr,
                    min: count,
                    max: count + 1,
                    current_count: count,
                    iterator: None,
                    pending_repeat: None,
                }
            }
            ExpressionKind::Nt(name) => {
                let prod = grammar.productions.get(name).unwrap();
                return Self::new_with_context(grammar, &prod.expression, name_context);
            }
            ExpressionKind::Terminal(s) => IteratorState::Terminal {
                value: s.clone(),
                done: false,
            },
            ExpressionKind::Prose(prose) => match prose.as_str() {
                "`XID_Start` defined by Unicode" => IteratorState::Terminal {
                    value: "a".to_string(),
                    done: false,
                },
                "`XID_Continue` defined by Unicode" => IteratorState::Terminal {
                    value: "b".to_string(),
                    done: false,
                },
                _ => panic!("prose {prose} not supported"),
            },
            ExpressionKind::Break(_) => unreachable!(),
            ExpressionKind::Comment(_) => unreachable!(),
            ExpressionKind::Charset(items) => {
                // Charset behaves as Alt of the given Characters
                let mut iterators = Vec::new();
                for chars in items {
                    match chars {
                        Characters::Named(name) => {
                            // Behave like Nt - lookup the production
                            let prod = grammar.productions.get(name).unwrap();
                            iterators.push(Self::new_with_context(
                                grammar,
                                &prod.expression,
                                name_context.clone(),
                            ));
                        }
                        Characters::Terminal(s) => {
                            // Behave like Terminal - create a Terminal iterator directly
                            iterators.push(PermutationIterator {
                                grammar,
                                name_context: name_context.clone(),
                                state: IteratorState::Terminal {
                                    value: s.clone(),
                                    done: false,
                                },
                            });
                        }
                        Characters::Range(start, end) => {
                            // Behave like Alt of start and end characters
                            let start_ch = start.get_ch();
                            let end_ch = end.get_ch();
                            iterators.push(PermutationIterator {
                                grammar,
                                name_context: name_context.clone(),
                                state: IteratorState::Terminal {
                                    value: start_ch.to_string(),
                                    done: false,
                                },
                            });
                            iterators.push(PermutationIterator {
                                grammar,
                                name_context: name_context.clone(),
                                state: IteratorState::Terminal {
                                    value: end_ch.to_string(),
                                    done: false,
                                },
                            });
                        }
                    }
                }

                IteratorState::Alt {
                    iterators,
                    current_index: 0,
                }
            }
            ExpressionKind::NegExpression(expr) => IteratorState::Terminal {
                value: String::from("a"), // TODO: Comment here why this choice.
                done: false,
            },
            ExpressionKind::Cut(expr) => {
                return Self::new_with_context(grammar, expr, name_context);
            }
            ExpressionKind::Unicode((ch, _)) => IteratorState::Terminal {
                value: ch.to_string(),
                done: false,
            },
        };
        PermutationIterator {
            grammar,
            name_context,
            state,
        }
    }
}

impl<'g> Iterator for PermutationIterator<'g> {
    // type Item = Permutation;
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        // Capture grammar reference before mutably borrowing state
        let grammar = self.grammar;

        match &mut self.state {
            IteratorState::Terminal { value, done } => {
                if *done {
                    None
                } else {
                    *done = true;
                    Some(value.clone())
                }
            }
            IteratorState::Alt {
                iterators,
                current_index,
            } => {
                while *current_index < iterators.len() {
                    if let Some(val) = iterators[*current_index].next() {
                        return Some(val);
                    }
                    *current_index += 1;
                }
                None
            }
            IteratorState::Optional {
                iterator,
                emitted_empty,
            } => {
                if !*emitted_empty {
                    *emitted_empty = true;
                    return Some(String::new());
                }
                iterator.next()
            }
            IteratorState::Repeat {
                expr,
                include_empty,
                current_stage,
                iterator,
                pending_double,
            } => {
                // Stage 0: emit empty string (only for Repeat, not RepeatPlus)
                if *current_stage == 0 && *include_empty {
                    *current_stage = 1;
                    return Some(String::new());
                }

                // Stage 1: emit single permutations
                if *current_stage == 1 {
                    if iterator.is_none() {
                        *iterator = Some(Box::new(Self::new_with_context(
                            grammar,
                            expr,
                            self.name_context.clone(),
                        )));
                    }

                    if let Some(iter) = iterator {
                        if let Some(result) = iter.next() {
                            return Some(result);
                        }
                    }

                    // Stage 1 complete, move to stage 2
                    *current_stage = 2;
                    *iterator = Some(Box::new(Self::new_with_context(
                        grammar,
                        expr,
                        self.name_context.clone(),
                    )));
                }

                // Stage 2: emit double permutations (each element repeated twice)
                if *current_stage == 2 {
                    // Check if we have a pending double to emit
                    if let Some(val) = pending_double.take() {
                        return Some(val);
                    }

                    // Get next single value and prepare to emit it twice
                    if let Some(iter) = iterator {
                        if let Some(val) = iter.next() {
                            let doubled = format!("{}{}", val, val);
                            return Some(doubled);
                        }
                    }
                }

                None
            }
            IteratorState::RepeatRange {
                expr,
                min,
                max,
                current_count,
                iterator,
                pending_repeat,
            } => {
                // If we're at count 0 (min was 0), emit empty string
                if *current_count == 0 {
                    *current_count = 1;
                    if *current_count >= *max {
                        return None;
                    }
                    return Some(String::new());
                }

                loop {
                    // If we haven't reached max count yet
                    if *current_count >= *max {
                        return None;
                    }

                    // Check if we have a pending repeat to emit
                    if let Some((val, times_left)) = pending_repeat {
                        if *times_left > 1 {
                            *times_left -= 1;
                            return Some(val.clone());
                        } else {
                            // Emit last repetition and clear pending
                            let result = val.clone();
                            *pending_repeat = None;
                            return Some(result);
                        }
                    }

                    // Initialize iterator for current count if needed
                    if iterator.is_none() {
                        *iterator = Some(Box::new(Self::new_with_context(
                            grammar,
                            expr,
                            self.name_context.clone(),
                        )));
                    }

                    // Try to get next value from iterator
                    if let Some(iter) = iterator {
                        if let Some(val) = iter.next() {
                            let result = val.repeat(*current_count);
                            return Some(result);
                        }
                    }

                    // Current count exhausted, move to next
                    *current_count += 1;
                    *iterator = None;
                }
            }
            IteratorState::Seq {
                exprs,
                iterators,
                current_values,
                indices,
                exhausted,
            } => {
                if *exhausted {
                    return None;
                }

                // Initialize: get first value from each iterator.
                if indices.iter().all(|&i| i == 0) && current_values[0].is_empty() {
                    for (i, iter) in iterators.iter_mut().enumerate() {
                        if let Some(val) = iter.next() {
                            current_values[i] = val;
                        } else {
                            *exhausted = true;
                            return None;
                        }
                    }
                    // Return the first combination.
                    return Some(current_values.concat());
                }

                // Try to advance the rightmost iterator.
                let mut pos = iterators.len() - 1;
                loop {
                    if let Some(val) = iterators[pos].next() {
                        current_values[pos] = val;
                        indices[pos] += 1;
                        return Some(current_values.concat());
                    } else {
                        // This iterator is exhausted, reset it and move to the left.
                        if pos == 0 {
                            // All iterators exhausted.
                            *exhausted = true;
                            return None;
                        }

                        // Reset this iterator.
                        iterators[pos] =
                            Self::new_with_context(grammar, &exprs[pos], self.name_context.clone());
                        if let Some(val) = iterators[pos].next() {
                            current_values[pos] = val;
                            indices[pos] = 0;
                        }

                        pos -= 1;
                    }
                }
            }
            IteratorState::SeqWithNamedRanges {
                exprs,
                named_range_indices,
                current_named_values,
                iterators,
                current_values,
                exhausted,
            } => {
                if *exhausted {
                    return None;
                }

                loop {
                    // Initialize iterators if needed
                    if iterators.is_empty() {
                        for expr in exprs.iter() {
                            iterators.push(Self::new_with_context(
                                grammar,
                                expr,
                                current_named_values.clone(),
                            ));
                        }
                        *current_values = iterators.iter().map(|_| String::new()).collect();

                        // Get first value from each iterator
                        for (i, iter) in iterators.iter_mut().enumerate() {
                            if let Some(val) = iter.next() {
                                current_values[i] = val;
                            } else {
                                // Empty iterator, try next name values
                                break;
                            }
                        }

                        if current_values.iter().all(|v| !v.is_empty()) {
                            return Some(current_values.concat());
                        }
                    }

                    // Try to advance rightmost iterator
                    let mut pos = iterators.len().checked_sub(1)?;
                    loop {
                        if let Some(val) = iterators[pos].next() {
                            current_values[pos] = val;
                            return Some(current_values.concat());
                        } else {
                            // This iterator exhausted
                            if pos == 0 {
                                // All iterators for this name combo exhausted
                                // Try to increment named values (only min and max, not values in between)
                                let mut incremented = false;
                                for (idx, name, min, max) in named_range_indices.iter().rev() {
                                    let current_val =
                                        current_named_values.get(name).copied().unwrap_or(*min);
                                    // Only generate for min and max values
                                    if current_val == *min && *min + 1 < *max {
                                        // Jump from min to max-1 (which is the actual max value since max is exclusive)
                                        current_named_values.insert(name.clone(), *max - 1);
                                        incremented = true;
                                        break;
                                    } else {
                                        // Reset to min
                                        current_named_values.insert(name.clone(), *min);
                                    }
                                }

                                if !incremented {
                                    *exhausted = true;
                                    return None;
                                }

                                // Reset all iterators with new named values
                                iterators.clear();
                                current_values.clear();
                                break; // Go back to initialization
                            }

                            // Reset this iterator and move left
                            iterators[pos] = Self::new_with_context(
                                grammar,
                                &exprs[pos],
                                current_named_values.clone(),
                            );
                            if let Some(val) = iterators[pos].next() {
                                current_values[pos] = val;
                            }
                            pos -= 1;
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn assert_permutations(grammar: &str, expected: &[&str]) {
        let g = Grammar::grammar_from_str(grammar, "cat").unwrap();
        let e = &g.productions.get("P").unwrap().expression;
        let ps: Vec<_> = PermutationIterator::new(&g, e).collect();
        assert_eq!(ps, expected);
    }

    #[test]
    fn seq_and_alt() {
        assert_permutations(
            "P -> `A` (`B` | (`C1` | `C2`) | `D`) `E`",
            &["ABE", "AC1E", "AC2E", "ADE"],
        );
    }

    #[test]
    fn optional() {
        assert_permutations("P -> `A` (`B` | `C`)? `D`", &["AD", "ABD", "ACD"]);
    }

    #[test]
    fn repeat() {
        assert_permutations("P -> (`A` | `B`)*", &["", "A", "B", "AA", "BB"]);
    }

    #[test]
    fn repeat_plus() {
        assert_permutations("P -> (`A` | `B`)+", &["A", "B", "AA", "BB"]);
    }

    #[test]
    fn repeat_range() {
        assert_permutations("P -> (`A` | `B`){0..}", &["", "A", "B", "AA", "BB"]);

        assert_permutations("P -> (`A` | `B`){1..3}", &["A", "B", "AA", "BB"]);

        assert_permutations("P -> (`A` | `B`){2..=3}", &["AA", "BB", "AAA", "BBB"]);
    }

    #[test]
    fn charset() {
        // Test with Terminal and Range
        assert_permutations("P -> [`A` `X`-`Z`]", &["A", "X", "Z"]);

        // Test with just Range
        assert_permutations("P -> [`a`-`c`]", &["a", "c"]);
    }

    #[test]
    fn named_repeat_range() {
        // Test named repeat ranges are synchronized (only min and max values)
        assert_permutations("P -> `A`{n:1..=5} `B` `C`{n}", &["ABC", "AAAAABCCCCC"]);
    }
}
