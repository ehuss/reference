use crate::{CommonOptions, display_line};
use clap::ArgMatches;
use std::ops::Range;

pub fn tree(matches: &ArgMatches) {
    let (mut opts, _) = CommonOptions::new(matches, &["reference"]);
    opts.progress.finish_and_clear();
    let production = matches.get_one::<String>("production").unwrap();
    for tool in &*opts.tools.clone() {
        while let Some((name, src)) = opts.next() {
            println!("------------------------------------------------------------");
            println!("tool `{tool}` tree results for `{name}`:");
            display_tree(&src, &tool, &production);
            println!("------------------------------------------------------------");
        }
    }
}

fn display_tree(src: &str, tool: &str, production: &str) {
    match tool {
        "reference" => display_reference_tree(src, production),
        _ => unimplemented!("{tool} not implemented yet"),
    }
}

fn display_reference_tree(src: &str, production: &str) {
    let node = match parser::tree::parse(src, production) {
        Ok(node) => node,
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
    display_tree_node(src, &node, 0);
}

fn display_tree_node(src: &str, node: &parser::Node, indent: usize) {
    let node_text = &src[node.range.clone()];
    let display_text = if node_text.len() > 20 {
        format!("{}…", &node_text[..20])
    } else {
        node_text.to_string()
    };
    println!(
        "{}{} {:?} {:?}",
        " ".repeat(indent),
        node.name,
        node.range,
        display_text
    );
    for child in &node.children.0 {
        display_tree_node(src, child, indent + 2);
    }
}
