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
use rustc_errors::translation::Translator;
use rustc_errors::{ColorConfig, DiagCtxt};
use rustc_span::FileName;
use rustc_span::source_map::{FilePathMapping, SourceMap};
use std::ops::Range;
use std::sync::{Arc, Mutex};
// use std::path::Path;
use rustc_driver::DEFAULT_LOCALE_RESOURCE;
// use rustc_lexer::FrontmatterAllowed;
use rustc_ast::token::TokenKind;
use rustc_parse::lexer::StripTokens;
use rustc_session::parse::ParseSess;
use std::error::Error;
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
                true, // pretty
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
                    Err(e) => panic!("{e:?}"),
                };
                let mut tokens = Vec::new();
                while parser.token.kind != TokenKind::Eof {
                    let token = Token {
                        name: format!("{:?}", parser.token.kind),
                        range: Range {
                            start: parser.token.span.lo().0 as usize,
                            end: parser.token.span.hi().0 as usize,
                        },
                    };
                    tokens.push(token);
                    parser.bump();
                }
                tokens
            })
            .map_err(|_| {
                let message = format!("{}", std::str::from_utf8(&output.lock().unwrap()).unwrap());
                LexError {
                    byte_offset: 0,
                    message,
                }
            })
        },
    )
}

pub fn normalize(tokens: &[Token], src: &str) -> Vec<Token> {
    tokens
        .iter()
        // rustc_parse does not retain comments.
        .filter(|token| !matches!(token.name.as_str(), "LINE_COMMENT" | "BLOCK_COMMENT"))
        // rustc_parse only keeps 2 continuous ## characters. Odd-number
        // leaves a punctuation as the last character.
        .flat_map(|token| {
            if token.name == "RESERVED_TOKEN" {
                let val = &src[token.range.clone()];
                if val.chars().all(|c| c == '#') {
                    let mut new_tokens = Vec::new();
                    let mut current_pos = token.range.start;
                    let end_pos = token.range.end;
                    while current_pos < end_pos {
                        let next_pos = std::cmp::min(current_pos + 2, end_pos);
                        new_tokens.push(Token {
                            name: token.name.clone(),
                            range: current_pos..next_pos,
                        });
                        current_pos = next_pos;
                    }
                    return new_tokens;
                }
            }
            vec![token.clone()]
        })
        .collect()
}
