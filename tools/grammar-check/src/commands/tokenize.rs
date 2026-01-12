use crate::tools::{pm2, rustc, rustc_lexer};
use crate::{CommonOptions, display_line};
use clap::ArgMatches;
use parser::lexer::Tokens;
use std::ops::Range;

pub fn tokenize(matches: &ArgMatches) {
    let (mut opts, _) = CommonOptions::new(matches, &["reference"]);
    opts.progress.finish_and_clear();
    for tool in &*opts.tools.clone() {
        while let Some((name, src)) = opts.next() {
            println!("------------------------------------------------------------");
            println!("tool `{tool}` token results for `{name}`:");
            tokenize_src(&src, &tool);
            println!("------------------------------------------------------------");
        }
    }
}

fn tokenize_src(src: &str, tool: &str) {
    let tokens = match tool {
        "reference" => {
            let tokens = parser::lexer::tokenize(src);
            if let Ok(Tokens {
                shebang: Some(shebang),
                ..
            }) = &tokens
            {
                println!("Shebang in range: {:?}", shebang);
            }
            if let Ok(Tokens {
                frontmatter: Some(frontmatter),
                ..
            }) = &tokens
            {
                println!("Frontmatter in range: {:?}", frontmatter);
            }
            tokens.map(|ts| ts.tokens)
        }
        "rustc_parse" => rustc::tokenize(src),
        "proc-macro2" => pm2::tokenize(src),
        "rustc_lexer" => rustc_lexer::tokenize(src),
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
        println!("{:?}: {}", &src[token.range], token.name);
    }
}
