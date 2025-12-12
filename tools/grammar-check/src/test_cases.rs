macro_rules! cases {
    ($($name:path => $($s:literal)+)+) => {
        pub static LEX_CASES: &[(&str, &[&str])] = &[
            $(
                (stringify!($name), &[ $($s),* ]),
            )+
        ];
    };
}

cases!{
    empty =>
        ""

    comment::line_comment =>
        "// line comment"
        "////"
        "//// this is a comment"
        "//\n"
    comment::block_comment =>
        "/* block comment */"
    comment::inner_line_doc =>
        "//! inner line doc"
    comment::inner_block_doc =>
        "/*! inner block doc */"
    comment::outer_line_doc =>
        "/// outer line doc"
        "///"
        "///\n"
        "///abc\n"
        "/// ☃"
    comment::outer_block_doc =>
        "/** outer block doc */"

    reserved::pounds =>
        "##"
        "###"
        "####"
        "#####"

    raw_identifier =>
        "r#fn"
    char =>
        "'x'"
    string =>
        "\"string\""
    raw_string =>
        "r\"raw string\""
        "r#\"raw string\"#"
        "r#\"\"\"#"
    byte =>
        "b'x'"
    byte_string =>
        "b\"byte\""
    raw_byte_string =>
        "br\"raw byte\""
        "br#\"raw byte\"#"
    c_string =>
        "c\"c str\""
    raw_c_string =>
        "cr\"raw c str\""
        "cr#\"raw c str\"#"
    float =>
        "1.2"
    integer =>
        "123"
    lifetime =>
        "'a"
    punctuation =>
        "!"
    identifier =>
        "ident"
        "fn"

}
