#![feature(rustc_private)]
#![allow(unused)]

extern crate rustc_span;

use clap::ArgMatches;
use clap::{Command, arg};
use indicatif::ProgressBar;
use indicatif::ProgressStyle;
use lexer::LexError;
use rustc_span::edition::Edition;
use std::cell::RefCell;
use std::cmp::min;
use std::io::IsTerminal;
use std::io::Read;
use std::ops::Range;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::mpsc::{Receiver, Sender, channel};
use std::time::Duration;
use std::time::Instant;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};
use walkdir::WalkDir;

mod rustc;
// mod rustc_lexer;
mod pm2;
mod test_cases;

enum Message {
    ThreadComplete,
    CtrlC,
}

struct CommonOptions {
    strings: Vec<(String, String)>,
    paths: Vec<PathBuf>,
    tools: Arc<Vec<String>>,
    edition: Option<Edition>,
    coverage: bool,
    test_count: u32,
    thread_count: u32,
    errors: Vec<String>,
    progress: ProgressBar,
    channel: Sender<Message>,
}

impl CommonOptions {
    fn new(
        matches: &clap::ArgMatches,
        default_tools: &[&str],
    ) -> (CommonOptions, Receiver<Message>) {
        fn map_case(case: &String) -> Vec<(String, String)> {
            match case.as_ref() {
                "all" => test_cases::LEX_CASES
                    .iter()
                    .flat_map(|(name, cases)| {
                        cases.iter().map(|c| (name.to_string(), c.to_string()))
                    })
                    .collect(),
                case_pattern => {
                    let cs: Vec<_> = test_cases::LEX_CASES
                        .iter()
                        .filter(|(name, _)| {
                            let name_parts: Vec<_> = name.split("::").collect();
                            let pattern_parts: Vec<_> = case_pattern.split("::").collect();
                            if pattern_parts.len() > name_parts.len() {
                                return false;
                            }
                            name_parts
                                .iter()
                                .zip(pattern_parts.iter())
                                .all(|(n, p)| n == p)
                        })
                        .flat_map(|(name, cases)| {
                            cases.iter().map(|c| (name.to_string(), c.to_string()))
                        })
                        .collect();
                    if cs.is_empty() {
                        eprintln!(
                            "error: case pattern `{case_pattern}` did not match any test cases"
                        );
                        std::process::exit(1);
                    }
                    cs
                }
            }
        }
        fn map_path(path: &String) -> impl Iterator<Item = PathBuf> {
            WalkDir::new(path)
                .into_iter()
                .filter_map(|e| e.ok())
                .filter(|e| e.file_type().is_file())
                .filter(|e| e.path().extension().map(|ext| ext == "rs").unwrap_or(false))
                .map(|e| e.into_path())
        }
        let mut strings: Vec<_> = matches
            .get_many("string")
            .map(|ss| {
                ss.map(|s: &String| ("CLI".to_string(), s.to_string()))
                    .collect()
            })
            .unwrap_or_default();
        let cases: Vec<_> = matches
            .get_many("case")
            .map(|ps| ps.flat_map(map_case).collect())
            .unwrap_or_default();
        strings.extend(cases);
        if matches.get_flag("stdin") {
            let mut buffer = String::new();
            if std::io::stdin().is_terminal() {
                println!("Enter source text:");
            }
            std::io::stdin().read_to_string(&mut buffer).unwrap();
            strings.push(("stdin".to_string(), buffer));
        }
        let paths: Vec<_> = matches
            .get_many("path")
            .map(|ps| ps.flat_map(map_path).collect())
            .unwrap_or_default();
        if strings.is_empty() && paths.is_empty() {
            strings.extend(map_case(&"all".to_string()));
        }
        let tools: Vec<_> = matches
            .get_many("tool")
            .map(|ts| ts.cloned().collect())
            .unwrap_or_else(|| default_tools.iter().map(|s| String::from(*s)).collect());
        let tools = Arc::new(tools);
        let edition = matches
            .get_one::<String>("edition")
            .map(|e| e.parse::<Edition>().unwrap());
        let coverage = matches.get_flag("coverage");
        // TODO: handle zero
        let test_count = ((strings.len() + paths.len()) as u32) * tools.len() as u32;
        let thread_count = min(
            test_count,
            std::thread::available_parallelism().unwrap().get() as u32,
        );
        let progress = ProgressBar::new(test_count as u64);
        progress.enable_steady_tick(Duration::from_millis(200));
        progress.set_message("0");
        let (channel, receiver) = channel();
        let opts = CommonOptions {
            strings,
            paths,
            tools,
            edition,
            coverage,
            test_count,
            thread_count,
            errors: Vec::new(),
            progress,
            channel,
        };
        opts.set_progress_style();
        (opts, receiver)
    }

    // fn iter(&self) -> impl Iterator<Item = (String, String)> + '_ {
    //     self.strings.iter()
    //         .map(|(name, src)| (name.clone(), src.clone()))
    //         .chain(
    //             self.paths.iter()
    //             .map(|path| {
    //                 let contents = std::fs::read_to_string(&path).unwrap();
    //                 let display = format!("{}", path.display());
    //                 (display, contents)
    //             })
    //     )
    // }

    fn next(&mut self) -> Option<(String, String)> {
        if let Some((name, src)) = self.strings.pop() {
            return Some((name, src));
        }
        if let Some(path) = self.paths.pop() {
            // TODO: Switch path to a string, not needed as PathBuf anymore.
            let contents = std::fs::read_to_string(&path).unwrap();
            let display = format!("{}", path.display());
            return Some((display, contents));
        }
        None
    }

    fn set_progress_style(&self) {
        let color = if self.errors.len() == 0 {
            "green"
        } else {
            "red"
        };
        self.progress.set_style(ProgressStyle::with_template(&format!("{{spinner:.green}} [{{elapsed_precise}}] [{{wide_bar:.blue}}] {{pos}}/{{len}} — {{msg:.{color}}} failures")).unwrap()
            .progress_chars("█▉▊▋▌▍▎▏  ")
            .tick_chars("🌑🌒🌓🌔🌕🌖🌗🌘"));
    }

    fn set_progress_err_msg(&self) {
        self.progress.set_message(format!("{}", self.errors.len()));
        self.set_progress_style();
    }
}

const TOOLS: [&str; 3] = ["reference", "rustc_parse", "proc-macro2"];

fn common_args() -> Vec<clap::Arg> {
    vec![
        arg!(--case <CASE> ... "internal test cases to compare"),
        arg!(--string <STRING> ... "source string to tokenize"),
        arg!(--path <PATH> ... "path of rust files to compare"),
        arg!(--tool <TOOLS> ... "tool to compare").value_parser(TOOLS),
        arg!(--edition <EDITION> "edition to use"),
        arg!(--coverage "record coverage data"),
        arg!(--stdin "read input from stdin"),
    ]
}

fn main() {
    let filter = tracing_subscriber::EnvFilter::builder()
        .with_env_var("GRAMMAR_LOG")
        .with_default_directive(tracing_subscriber::filter::LevelFilter::INFO.into())
        .from_env_lossy();

    tracing_subscriber::registry()
        .with(filter)
        .with(
            tracing_tree::HierarchicalLayer::new(2)
                .with_writer(std::io::stderr)
                .with_ansi(std::io::IsTerminal::is_terminal(&std::io::stderr())),
        )
        .init();

    let matches = Command::new("lex-compare")
        .subcommand_required(true)
        .arg_required_else_help(true)
        .subcommand(
            Command::new("compare")
                .about("Compare tokenization between implementations")
                .args(common_args()),
        )
        .subcommand(
            Command::new("tokenize")
                .about("Convert source to tokens")
                .args(common_args()),
        )
        .get_matches();
    match matches.subcommand() {
        Some(("compare", sub_matches)) => {
            compare_parallel(sub_matches);
        }
        Some(("tokenize", sub_matches)) => {
            tokenize(sub_matches);
        }
        _ => unreachable!(),
    }
}

// fn compare(opts: Arc<Mutex<CommonOptions>>) {
//     let mut opts = opts.lock().unwrap();
//     for tool in std::mem::take(&mut opts.tools) {
//         while let Some((name, src)) = opts.next() {
//             compare_src(&src, &tool);
//         }
//     }
// }

thread_local! {
    static PANIC_OUTPUT: RefCell<Option<String>> = RefCell::new(None);
}

fn compare_parallel(matches: &ArgMatches) {
    let start = Instant::now();
    let (opts, receiver) = CommonOptions::new(matches, &TOOLS[1..]);
    if opts.tools.iter().any(|t| t == "reference") {
        panic!("can't compare reference to itself");
    }

    std::panic::set_hook(Box::new(|info| {
        let payload = info.payload();
        let msg = if let Some(s) = payload.downcast_ref::<&str>() {
            s
        } else if let Some(s) = payload.downcast_ref::<String>() {
            s.as_str()
        } else {
            "Box<dyn Any>"
        };
        let location = info
            .location()
            .map(|l| format!("{}:{}:{}", l.file(), l.line(), l.column()))
            .unwrap_or_else(|| "unknown".to_string());
        let thread = std::thread::current();
        let name = thread.name().unwrap_or("<unnamed>");
        let output = format!("thread '{name}' panicked at {location}:\n{msg}");

        // We print it here as well to ensure it is seen if the thread dies unexpectedly.
        // eprintln!("{}", output);

        PANIC_OUTPUT.with(|c| {
            *c.borrow_mut() = Some(output);
        });
    }));

    let sender = opts.channel.clone();
    let mut thread_count = opts.thread_count;
    let opts = Arc::new(Mutex::new(opts));
    for _ in 0..thread_count {
        let opts_c = opts.clone();
        std::thread::spawn(move || {
            compare_loop(opts_c);
        });
    }
    ctrlc::set_handler(move || {
        sender.send(Message::CtrlC).unwrap();
    })
    .unwrap();
    loop {
        match receiver.recv().unwrap() {
            Message::ThreadComplete => {
                thread_count -= 1;
                if thread_count == 0 {
                    break;
                }
            }
            Message::CtrlC => {
                break;
            }
        }
    }
    // let opts_l = opts.lock().unwrap();
    // let (lock, cvar) = &*opts_l.thread_count.clone();
    // drop(opts_l);
    // cvar.wait_while(lock.lock().unwrap(), |count| *count > 0)
    //     .unwrap();
    let opts_l = opts.lock().unwrap();
    opts_l.progress.finish_and_clear();
    if !opts_l.errors.is_empty() {
        eprintln!("------------------------------------------------------------");
        for error in &opts_l.errors {
            eprintln!(
                "{error}\n\
                 ------------------------------------------------------------"
            );
        }
    }
    let n_errs = opts_l.errors.len() as u32;
    eprintln!("passed: {}", opts_l.test_count - n_errs);
    eprintln!("failed: {n_errs}");
    let elapsed = start.elapsed();
    if elapsed.as_secs() < 60 {
        eprintln!("finished in {:.1} seconds", elapsed.as_secs_f64());
    } else {
        eprintln!(
            "finished in {} minutes {} seconds",
            elapsed.as_secs() / 60,
            elapsed.as_secs() % 60
        );
    }
    if !opts_l.errors.is_empty() {
        std::process::exit(1);
    }
}

fn compare_loop(opts: Arc<Mutex<CommonOptions>>) {
    let channel = opts.lock().unwrap().channel.clone();
    loop {
        let mut opts_l = opts.lock().unwrap();
        let Some((name, src)) = opts_l.next() else {
            break;
        };
        let tools = opts_l.tools.clone();
        drop(opts_l);
        for tool in &*tools {
            // TODO: get the panic output somehow?
            match std::panic::catch_unwind(|| compare_src(&name, &src, tool)) {
                Ok(Ok(())) => {}
                Ok(Err(e)) => {
                    let mut opts_l = opts.lock().unwrap();
                    opts_l.errors.push(e);
                    opts_l.set_progress_err_msg();
                }
                Err(_) => {
                    let panic_msg = PANIC_OUTPUT.with(|c| {
                        c.borrow_mut()
                            .take()
                            .unwrap_or_else(|| "unknown panic".to_string())
                    });
                    let mut opts_l = opts.lock().unwrap();
                    opts_l.errors.push(format!(
                        "test {name} for tool {tool} panicked:\n{panic_msg}"
                    ));
                    opts_l.set_progress_err_msg();
                }
            }
            let opts_l = opts.lock().unwrap();
            opts_l.progress.inc(1);
        }
    }
    channel.send(Message::ThreadComplete).unwrap();
}

fn compare_src(name: &str, src: &str, tool: &str) -> Result<(), String> {
    let lexer_result = lexer::tokenize(src);
    let (tool_result, mut lexer_result) = match tool {
        "rustc_parse" => {
            let lexer_result = lexer_result.and_then(|ts| rustc::normalize(&ts, src));
            (rustc::tokenize(src), lexer_result)
        }
        "proc-macro2" => {
            let lexer_result = lexer_result.map(|ts| pm2::normalize(&ts, src));
            (pm2::tokenize(src), lexer_result)
        }
        _ => unreachable!(),
    };
    if let Ok(tokens) = &lexer_result {
        if let Some(reserved) = tokens.iter().find(|token| token.name == "RESERVED_TOKEN") {
            lexer_result = Err(LexError {
                byte_offset: reserved.range.start,
                message: "reserved token".to_string(),
            });
        }
    }

    match (lexer_result, tool_result) {
        (Ok(lex_tokens), Ok(tool_tokens)) => {
            let mut lex_iter = lex_tokens.iter();
            let mut tool_iter = tool_tokens.iter();
            loop {
                let lex_token = lex_iter.next();
                let tool_token = tool_iter.next();
                match (lex_token, tool_token) {
                    (Some(lex_token), Some(tool_token)) => {
                        let lex_text = &src[lex_token.range.clone()];
                        let tool_text = &src[tool_token.range.clone()];
                        if lex_text != tool_text {
                            return Err(format!(
                                "error: token mismatch\n\
                                test: {name}\n\
                                lexer token: {:?} {:?}\n\
                                {}\n\
                                {tool} token: {:?} {:?}\n\
                                {}",
                                lex_token.name,
                                lex_text,
                                display_line(src, &lex_token.range),
                                tool_token.name,
                                tool_text,
                                display_line(src, &tool_token.range),
                            ));
                        }
                    }
                    (None, None) => break,
                    (Some(lex_token), None) => {
                        return Err(format!(
                            "error: lexer has more tokens (compared to {tool})\n\
                            test: {name}\n\
                            lexer token: {:?} {:?}\n\
                            {}",
                            lex_token.name,
                            &src[lex_token.range.clone()],
                            display_line(src, &lex_token.range),
                        ));
                    }
                    (None, Some(tool_token)) => {
                        return Err(format!(
                            "error: {tool} has more tokens (compared to reference grammar)\n\
                            test: {name}\n\
                            {tool} token: {:?} {:?}\n\
                            {}",
                            tool_token.name,
                            &src[tool_token.range.clone()],
                            display_line(src, &tool_token.range),
                        ));
                    }
                }
            }
            return Ok(());
        }
        (Err(e), Ok(tokens)) => {
            return Err(format!(
                "error: lexer failed, {tool} passed\n\
                test: {name}\n\
                lexer error: {}\n\
                {}",
                e.display(src),
                display_line(
                    src,
                    &Range {
                        start: e.byte_offset,
                        end: e.byte_offset + 1
                    }
                )
            ));
            // for tool_token in tokens {
            //     eprintln!("{:?} {:?}", tool_token.name, &src[tool_token.range.clone()]);
            // }
        }
        (Ok(tokens), Err(e)) => {
            return Err(format!(
                "error: {tool} failed, lexer passed\n\
                test: {name}\n\
                {tool} error: {e:?}\n\
                src:\n\
                ----------\n\
                {src}\n\
                ----------",
            ));
            //TODO
            // for token in tokens {
            //     eprintln!("{} {}", token.name, &src[token.range]);
            // }
        }
        (Err(lex_e), Err(tool_e)) => {
            // TODO: Should this check the offset is the same?
            return Ok(());
        }
    }
}

/// Helper to translate a byte index to a `(line, line_no, col_no)` (1-based).
fn translate_position(input: &str, index: usize) -> (&str, usize, usize) {
    if input.is_empty() {
        return ("", 0, 0);
    }
    let index = index.min(input.len());

    let mut line_start = 0;
    let mut line_number = 0;
    for line in input.lines() {
        let line_end = line_start + line.len();
        if index >= line_start && index <= line_end {
            let column_number = index - line_start + 1;
            return (line, line_number + 1, column_number);
        }
        line_start = line_end + 1;
        line_number += 1;
    }
    ("", line_number + 1, 0)
}

fn display_line(src: &str, range: &Range<usize>) -> String {
    let (line, line_no, col_no) = translate_position(src, range.start);
    let prefix = format!("{line_no}: ");
    let indent = col_no.saturating_sub(1);
    let len = (range.end - range.start).min(line.len().saturating_sub(indent));
    let underline = format!("{}{}", " ".repeat(prefix.len() + indent), "━".repeat(len));
    format!("{prefix}{line}\n{underline}\n")
}

fn tokenize(matches: &ArgMatches) {
    let (mut opts, _) = CommonOptions::new(matches, &["reference"]);
    opts.progress.finish_and_clear();
    for tool in &*opts.tools.clone() {
        while let Some((name, src)) = opts.next() {
            tokenize_src(&src, &tool);
        }
    }
}

fn tokenize_src(src: &str, tool: &str) {
    let tokens = match tool {
        "reference" => lexer::tokenize(src),
        "rustc_parse" => rustc::tokenize(src),
        "proc-macro2" => pm2::tokenize(src),
        _ => unreachable!(),
    };
    let tokens = match tokens {
        Ok(tokens) => tokens,
        Err(e) => {
            eprintln!(
                "error: {}\n\
                {}",
                e.message,
                display_line(
                    src,
                    &Range {
                        start: e.byte_offset,
                        end: e.byte_offset + 1
                    }
                )
            );
            std::process::exit(1);
        }
    };
    for token in tokens {
        println!("{}: {}", &src[token.range], token.name);
    }
}
