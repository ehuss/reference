use crate::{Diagnostics, warn_or_err};
use mdbook::book::{Book, BookItem, Chapter};
use regex::{Captures, Regex};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::Write;
use std::path::PathBuf;
use std::sync::LazyLock;

mod parser;
mod renderer;

#[derive(Debug, Default)]
pub struct Grammar {
    // TODO: Add a vec to track order.
    pub productions: HashMap<String, Production>,
}

#[derive(Debug)]
pub struct Production {
    name: String,
    expression: Expression,
    path: PathBuf,
}

#[derive(Debug)]
struct Expression {
    kind: ExpressionKind,
    suffix: Option<String>,
    footnote: Option<String>,
}

#[derive(Debug)]
enum ExpressionKind {
    Grouped(Box<Expression>),
    Alt(Vec<Expression>),
    Sequence(Vec<Expression>),
    Optional(Box<Expression>),
    Repeat(Box<Expression>),
    RepeatNonGreedy(Box<Expression>),
    RepeatPlus(Box<Expression>),
    RepeatPlusNonGreedy(Box<Expression>),
    RepeatN(Box<Expression>, Option<u32>, Option<u32>),
    Nt(String),
    Terminal(String),
    Prose(String),
    Break(usize),
    Charset(Vec<Characters>),
    NegExpression(Box<Expression>),
    Unicode(String),
}

#[derive(Debug)]
enum Characters {
    Named(String),
    Terminal(String),
    Range(char, char),
}

static GRAMMAR_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?ms)^```(lexer|syntax)\n(.*?)^```").unwrap());
static NAMES_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?m)^([A-Za-z0-9_]+) ->").unwrap());

pub fn load_grammar(book: &Book, diag: &mut Diagnostics) -> Grammar {
    let mut grammar = Grammar::default();
    for item in book.iter() {
        let BookItem::Chapter(ch) = item else {
            continue;
        };
        if ch.is_draft_chapter() {
            continue;
        }
        let path = ch.path.as_ref().unwrap().to_owned();
        for cap in GRAMMAR_RE.captures_iter(&ch.content) {
            let input = &cap[2];
            if let Err(e) = parser::parse_grammar(input, &mut grammar, &path) {
                warn_or_err!(diag, "failed to parse grammar in {path:?}: {e}");
            }
        }
    }
    // eprintln!("{grammar:#?}", );
    grammar.find_missing_nt(diag);
    grammar
}

pub fn insert_grammar(grammar: &Grammar, chapter: &Chapter, diag: &mut Diagnostics) -> String {
    let mut content = GRAMMAR_RE
        .replace_all(&chapter.content, |cap: &Captures<'_>| {
            let mut output = String::new();
            if &cap[1] == "lexer" {
                output.push_str("> **<sup>Lexer</sup>**\n");
            } else {
                output.push_str("> **<sup>Syntax</sup>**\n");
            }
            let names: Vec<_> = NAMES_RE
                .captures_iter(&cap[2])
                .map(|cap| cap.get(1).unwrap().as_str())
                .collect();

            if let Err(e) = grammar.to_markdown(&names, &mut output) {
                warn_or_err!(
                    diag,
                    "grammar failed in chapter {:?}: {e}",
                    chapter.source_path.as_ref().unwrap()
                );
            }
            output
        })
        .to_string();
    let current_path = chapter.path.as_ref().unwrap().parent().unwrap();
    // eprintln!("linking {:?}", chapter.path);
    for p in grammar.productions.values() {
        let relative = pathdiff::diff_paths(&p.path, current_path).unwrap();
        // Adjust paths for Windows.
        let relative = relative.display().to_string().replace('\\', "/");
        // eprintln!("[_{name}_]: {relative}",);
        writeln!(
            content,
            "[{name}]: {relative}#grammar-{name}",
            name = p.name
        )
        .unwrap();
    }
    content
}

pub fn collect_grammar_paths(
    book: &Book,
    grammar: &Grammar,
    diag: &mut Diagnostics,
) -> BTreeMap<String, PathBuf> {
    let mut grammar_map = BTreeMap::new();
    for item in book.iter() {
        let BookItem::Chapter(ch) = item else {
            continue;
        };
        if ch.is_draft_chapter() {
            continue;
        }
        let path = ch.path.as_ref().unwrap().to_owned();
        for cap in GRAMMAR_RE.captures_iter(&ch.content) {
            let names = cap["names"].split_whitespace();
            for name in names {
                if let Some(old_path) = grammar_map.insert(name.to_string(), path.clone()) {
                    warn_or_err!(
                        diag,
                        "grammar rule {name} is defined in two places {old_path:?} and {:?}",
                        grammar_map[name]
                    );
                }
            }
        }
    }

    let keys: HashSet<_> = grammar_map.keys().collect();
    let defined: HashSet<_> = grammar.productions.keys().collect();
    let missing: Vec<_> = defined.difference(&keys).collect();
    if !missing.is_empty() {
        warn_or_err!(
            diag,
            "the following grammar names are missing from the book: {missing:?}"
        );
    }

    grammar_map
}
