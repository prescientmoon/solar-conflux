use std::io;

mod lexer;
mod parser;
mod type_checker;

use type_checker::type_::get_type_of;

fn main() {
    loop {
        println!("Enter a string to lex:");
        let mut input = String::new();

        io::stdin()
            .read_line(&mut input)
            .expect("Error reading line");

        let (_, result) =
            lexer::lex(input[..].as_bytes()).expect("An error ocurred while parsing!");

        println!("Finished lexing string successfully!");
        println!("Tokens:");

        for token in &result {
            println!("{:?}", token);
        }

        let (_, result) =
            parser::parse_expression(result).expect("AN error ocurred while parsing!");

        println!("Finished parsing successfully");

        println!("{:?}", result);

        let inferred = get_type_of(result).expect("A type error ocurred while type checking");

        println!("Finished type-checking successfully!");
        println!("The expression has type {:?}", inferred);
    }
}
