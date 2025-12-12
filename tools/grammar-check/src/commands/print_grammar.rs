use clap::ArgMatches;
use diagnostics::Diagnostics;

pub fn print_grammar(matches: &ArgMatches) {
    let debug = matches.get_flag("debug");
    let mut diag = Diagnostics::new();
    let grammar = grammar::load_grammar(&mut diag);

    if debug {
        for name in &grammar.name_order {
            let production = grammar.productions.get(name).unwrap();
            println!("{} ->", name);
            println!("{:#?}", production.expression);
            println!();
        }
    } else {
        for name in &grammar.name_order {
            let production = grammar.productions.get(name).unwrap();
            println!("{} -> {}", name, production.expression);
            println!();
        }
    }
}
