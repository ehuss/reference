use crate::{warn_or_err, Diagnostics};
use mdbook::book::{Book, BookItem, Chapter};
use std::sync::LazyLock;
use regex::{Captures, Regex};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::Write;
use std::path::{Path, PathBuf};

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
    is_lexer: bool,
}

#[derive(Debug)]
struct Expression {
    kind: ExpressionKind,
    suffix: Option<String>,
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

pub fn load_grammar(diag: &mut Diagnostics) -> Grammar {
    let mut grammar = Grammar::default();
    load_from_file(diag, "rust.txt", &mut grammar, false);
    load_from_file(diag, "lexer.txt", &mut grammar, true);
    grammar.find_missing_nt(diag);
    grammar
}

fn load_from_file(diag: &mut Diagnostics, file: &str, grammar: &mut Grammar, is_lexer: bool) {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("src/grammar").join(file);
    let input = match std::fs::read_to_string(&path) {
        Ok(s) => s,
        Err(e) => {
            panic!("failed to load {path:?}: {e:?}");
        }
    };
    if let Err(e) = parser::parse_grammar(&input, grammar, is_lexer) {
        warn_or_err!(diag, "failed to parse grammar: {e}");
    }
    // eprintln!("{grammar:#?}", );
}

static GRAMMAR_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"\{\{ grammar (?<names>.*) \}\}").unwrap());

pub fn insert_grammar(
    grammar: &Grammar,
    chapter: &Chapter,
    grammar_map: &BTreeMap<String, PathBuf>,
    diag: &mut Diagnostics,
) -> String {
    let mut content = GRAMMAR_RE
        .replace_all(&chapter.content, |caps: &Captures<'_>| {
            let names: Vec<_> = caps["names"].split_whitespace().collect();
            let mut output = String::new();
            if let Err(e) = grammar.to_html(&names, &mut output) {
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
    for (name, path) in grammar_map {
        let relative = pathdiff::diff_paths(path, current_path).unwrap();
        // Adjust paths for Windows.
        let relative = relative.display().to_string().replace('\\', "/");
        // eprintln!("[_{name}_]: {relative}",);
        writeln!(content, "[{name}]: {relative}#grammar-{name}").unwrap();
    }
    content
}

pub fn collect_grammar_paths(book: &Book, grammar: &Grammar, diag: &mut Diagnostics) -> BTreeMap<String, PathBuf> {
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
        warn_or_err!(diag, "the following grammar names are missing from the book: {missing:?}");
    }

    grammar_map
}
