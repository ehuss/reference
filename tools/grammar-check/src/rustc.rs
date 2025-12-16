use std::fmt::Write as _;
extern crate rustc_ast;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_lexer;
extern crate rustc_parse;
extern crate rustc_session;
extern crate rustc_span;

use lexer::LexError;
use lexer::Token;
use rustc_errors::emitter::HumanReadableErrorType;
use rustc_errors::json::JsonEmitter;
use rustc_errors::{ColorConfig, DiagCtxt};
use rustc_span::FileName;
use rustc_span::fatal_error::FatalError;
use rustc_span::source_map::{FilePathMapping, SourceMap};
use std::ops::Range;
use std::sync::{Arc, Mutex};
// use std::path::Path;
// use rustc_lexer::FrontmatterAllowed;
use rustc_ast::token::TokenKind;
use rustc_parse::lexer::StripTokens;
use rustc_session::parse::ParseSess;
use std::io;
use std::io::Write;
// use rustc_ast::token::Token;

struct Shared<T> {
    data: Arc<Mutex<T>>,
}

impl<T: Write> Write for Shared<T> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.data.lock().unwrap().write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.data.lock().unwrap().flush()
    }
}

pub fn tokenize(src: &str) -> Result<Vec<Token>, LexError> {
    rustc_span::create_session_globals_then(
        rustc_span::edition::Edition::Edition2024,
        &[],
        None,
        || {
            let source_map = Arc::new(SourceMap::new(FilePathMapping::empty()));
            // TODO: probably not needed?
            // source_map.new_source_file(Path::new("test.rs").to_owned().into(), "".to_owned());
            // let translator = Translator::with_fallback_bundle(vec![DEFAULT_LOCALE_RESOURCE], false);
            let translator = rustc_driver::default_translator();
            let output = Arc::new(Mutex::new(Vec::new()));
            let je = JsonEmitter::new(
                Box::new(Shared {
                    data: output.clone(),
                }),
                Some(source_map.clone()),
                translator,
                false, // pretty
                HumanReadableErrorType::Default { short: true },
                ColorConfig::Never,
            );

            let dcx = DiagCtxt::new(Box::new(je));
            let psess = ParseSess::with_dcx(dcx, source_map);
            let strip_tokens = StripTokens::Nothing;
            let source = String::from(src);
            let filename = FileName::Custom("internal".into());
            rustc_driver::catch_fatal_errors(|| {
                let mut parser = match rustc_parse::new_parser_from_source_str(
                    &psess,
                    filename,
                    source,
                    strip_tokens,
                ) {
                    Ok(parser) => parser,
                    Err(e) => {
                        for diag in e {
                            diag.emit();
                        }
                        FatalError.raise();
                    }
                };
                let mut tokens = Vec::new();
                while parser.token.kind != TokenKind::Eof {
                    let source_file = psess
                        .source_map()
                        .lookup_source_file(parser.token.span.lo());
                    let start = source_file
                        .original_relative_byte_pos(parser.token.span.lo())
                        .0 as usize;
                    let end = source_file
                        .original_relative_byte_pos(parser.token.span.hi())
                        .0 as usize;

                    let token = Token {
                        name: format!("{:?}", parser.token.kind),
                        range: Range { start, end },
                    };
                    tokens.push(token);
                    parser.bump();
                }
                // Unfortunately this is handled outside of normal lexing.
                psess.bad_unicode_identifiers.with_lock(|idents| {
                    for (ident, mut spans) in idents.drain(..) {
                        psess
                            .dcx()
                            .emit_err(rustc_interface::errors::EmojiIdentifier { spans, ident });
                    }
                });
                let diags = diagnostics(&output.lock().unwrap());
                if diags.iter().any(|diag| diag.level.starts_with("error")) {
                    FatalError.raise();
                }
                tokens
            })
            .map_err(|_| {
                let mut message = String::new();
                let out = &output.lock().unwrap();
                let diags = diagnostics(out);
                let mut byte_offset = 0;
                for diag in diags {
                    write!(message, "error: {}", diag.rendered).unwrap();
                    if byte_offset == 0 {
                        byte_offset = diag
                            .spans
                            .iter()
                            .find(|sp| sp.is_primary)
                            .unwrap()
                            .byte_start;
                    }
                }
                LexError {
                    byte_offset: byte_offset as usize,
                    message,
                }
            })
        },
    )
}

fn diagnostics(output: &[u8]) -> Vec<Diagnostic> {
    let json = std::str::from_utf8(output).unwrap();
    json.lines()
        .map(|line| serde_json::from_str(line).unwrap())
        .collect()
}

#[derive(serde::Deserialize)]
struct Diagnostic {
    rendered: String,
    level: String,
    spans: Vec<DiagSpan>,
}

#[derive(serde::Deserialize)]
struct DiagSpan {
    is_primary: bool,
    byte_start: u32,
}

pub fn normalize(tokens: &[Token], _src: &str) -> Result<Vec<Token>, LexError> {
    let new_ts = tokens
        .iter()
        // rustc_parse does not retain comments.
        .filter(|token| !matches!(token.name.as_str(), "LINE_COMMENT" | "BLOCK_COMMENT"))
        .map(|token| token.clone())
        .collect();
    Ok(new_ts)
}
