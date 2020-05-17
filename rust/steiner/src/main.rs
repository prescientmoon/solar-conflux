mod lexer;

fn main() {
    let my_lexer = lexer::Lexer::new();
    let (my_new_string, _) = lexer::skip_whitespace(
        "           Something With whitespace at the start!",
        my_lexer,
    );

    println!("{}", my_new_string);
}
