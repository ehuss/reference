use crate::CommonOptions;
use crate::tools::{pm2, rustc};
use crate::{Message, display_line};
use clap::ArgMatches;
use parser::ParseError;
use parser::lexer::Tokens;
use std::cell::RefCell;
use std::ops::Range;
use std::sync::{Arc, Mutex};
use std::time::Instant;

const DEFAULT_COMPARE_TOOLS: [&str; 2] = ["rustc_parse", "proc-macro2"];

thread_local! {
    static PANIC_OUTPUT: RefCell<Option<String>> = RefCell::new(None);
}

pub fn compare_parallel(matches: &ArgMatches) {
    let start = Instant::now();
    let (opts, receiver) = CommonOptions::new(matches, &DEFAULT_COMPARE_TOOLS);
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
    let lexer_result = parser::lexer::tokenize(src);
    let (tool_result, mut lexer_result) = match tool {
        "rustc_parse" => {
            let lexer_result = lexer_result.and_then(|ts| rustc::normalize(&ts.tokens, src));
            (rustc::tokenize(src), lexer_result)
        }
        "proc-macro2" => {
            // Unfortunately proc-macro2 does not handle shebang or
            // frontmatter. In order to handle files with that, this replaces
            // those with whitespace in order to retain the original byte
            // positions.
            if let Err(ParseError { message, .. }) = &lexer_result
                && message.contains("invalid frontmatter")
            {
                return Ok(());
            }
            let mut stripped_src = String::from(src);
            let mut replace = |range: &Range<usize>| {
                let replacement = "\n".repeat(range.end - range.start);
                stripped_src.replace_range(range.clone(), &replacement);
            };
            if let Ok(Tokens {
                shebang: Some(shebang),
                ..
            }) = &lexer_result
            {
                replace(&shebang.range);
            }
            if let Ok(Tokens {
                frontmatter: Some(frontmatter),
                ..
            }) = &lexer_result
            {
                replace(&frontmatter.range);
            }
            let pm2_result = pm2::tokenize(&stripped_src);
            pm2::normalize(pm2_result, lexer_result, src)
        }
        _ => unreachable!(),
    };
    if let Ok(tokens) = &lexer_result {
        if let Some(invalid) = tokens
            .iter()
            .find(|token| token.name == "RESERVED_TOKEN" || token.name.starts_with("INVALID_"))
        {
            lexer_result = Err(ParseError {
                byte_offset: invalid.range.start,
                message: format!("invalid token {}", invalid.name),
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
                        if lex_text != tool_text || lex_token.name != tool_token.name {
                            return Err(format!(
                                "error: token mismatch\n\
                                test: {name}\n\
                                reference token: {:?} {:?} {:?}\n\
                                {}\n\
                                {tool} token: {:?} {:?} {:?}\n\
                                {}",
                                lex_token.name,
                                lex_text,
                                lex_token.range,
                                display_line(src, &lex_token.range),
                                tool_token.name,
                                tool_text,
                                tool_token.range,
                                display_line(src, &tool_token.range),
                            ));
                        }
                    }
                    (None, None) => break,
                    (Some(lex_token), None) => {
                        return Err(format!(
                            "error: reference has more tokens (compared to {tool})\n\
                            test: {name}\n\
                            reference token: {:?} {:?}\n\
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
        (Err(e), Ok(_)) => {
            return Err(format!(
                "error: reference failed, {tool} passed\n\
                test: {name}\n\
                reference error: {}\n\
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
        }
        (Ok(_), Err(e)) => {
            return Err(format!(
                "error: {tool} failed, reference passed\n\
                test: {name}\n\
                {tool} error: {}\n\
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
        }
        (Err(_), Err(_)) => {
            // Unfortunately getting the error byte offsets to match between
            // the reference lexer and the tools is probably just too much
            // effort. This means that they could be reporting errors for
            // different reasons, but we wouldn't know.
            return Ok(());
        }
    }
}
