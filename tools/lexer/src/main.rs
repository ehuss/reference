use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

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

    let src = r###"r"test""###;

    let ts = match lexer::tokenize(src) {
        Ok(ts) => ts,
        Err(e) => {
            eprintln!("error: {}", e.display(src));
            std::process::exit(1);
        }
    };
    for token in ts.tokens {
        eprintln!(
            "{} {:?}: `{}`",
            token.name,
            token.range.clone(),
            &src[token.range]
        );
    }
}
