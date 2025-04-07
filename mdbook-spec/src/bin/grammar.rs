use mdbook_spec::Diagnostics;

fn main() {
    let mut diag = Diagnostics::new();
    let grammar = mdbook_spec::grammar::load_grammar(&mut diag);
    let mut output = String::new();
    let names: Vec<_> = grammar.productions.keys().map(|x| x.as_str()).collect();
    grammar.to_html(&names, &mut output).unwrap();
    println!("{output}");
}
