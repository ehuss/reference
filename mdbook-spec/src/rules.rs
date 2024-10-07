//! Handling for rule identifiers.

use crate::Spec;
use mdbook::book::Book;
use mdbook::BookItem;
use once_cell::sync::Lazy;
use regex::{Captures, Regex};
use std::collections::BTreeMap;
use std::path::PathBuf;

/// The Regex for rules like `r[foo]`.
static RULE_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"(?m)^r\[([^]]+)]$").unwrap());

/// The set of rules defined in the reference.
#[derive(Default)]
pub struct Rules {
    /// A mapping from a rule identifier to a tuple of `(source_path, path)`.
    ///
    /// `source_path` is the path to the markdown source file relative to the
    /// `SUMMARY.md`.
    ///
    /// `path` is the same as `source_path`, except filenames like `README.md`
    /// are translated to `index.md`. Which to use depends on if you are
    /// trying to access the source files (`source_path`), or creating links
    /// in the output (`path`).
    pub def_paths: BTreeMap<String, (PathBuf, PathBuf)>,
}

impl Spec {
    /// Collects all rule definitions in the book.
    pub fn collect_rules(&self, book: &Book) -> Rules {
        let mut rules = Rules::default();
        for item in book.iter() {
            let BookItem::Chapter(ch) = item else {
                continue;
            };
            if ch.is_draft_chapter() {
                continue;
            }
            RULE_RE
                .captures_iter(&ch.content)
                .for_each(|caps: Captures<'_>| {
                    let rule_id = &caps[1];
                    let source_path = ch.source_path.clone().unwrap_or_default();
                    let path = ch.path.clone().unwrap_or_default();
                    if let Some((old, _)) = rules
                        .def_paths
                        .insert(rule_id.to_string(), (source_path.clone(), path.clone()))
                    {
                        let message = format!(
                            "rule `{rule_id}` defined multiple times\n\
                             First location: {old:?}\n\
                             Second location: {source_path:?}"
                        );
                        if self.deny_warnings {
                            panic!("error: {message}");
                        } else {
                            eprintln!("warning: {message}");
                        }
                    }
                });
        }

        rules
    }

    /// Converts lines that start with `r[…]` into a "rule" which has special
    /// styling and can be linked to.
    pub fn render_rule_definitions(&self, content: &str) -> String {
        RULE_RE
            .replace_all(content, |caps: &Captures<'_>| {
                let rule_id = &caps[1];
                format!(
                    "<div class=\"rule\" id=\"r-{rule_id}\">\
                     <a class=\"rule-link\" href=\"#r-{rule_id}\">[{rule_id}]</a>\
                     </div>\n"
                )
            })
            .to_string()
    }
}