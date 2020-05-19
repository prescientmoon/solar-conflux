use std::io;

mod lexer;
mod parser;
mod type_checker;

fn main() {
    loop {
        println!("Enter a string to lex:");
        let mut input = String::new();

        io::stdin()
            .read_line(&mut input)
            .expect("Error reading line");

        match lexer::lex(input[..].as_bytes()) {
            Ok((_, result)) => {
                println!("Finished lexing string successfully!");
                println!("Tokens:");

                for token in &result {
                    println!("{:?}", token);
                }

                match parser::parse_expression(result) {
                    Ok((_, parsing_result)) => {
                        println!("Finished parsing successfully");

                        println!("{:?}", parsing_result);
                    }

                    Err(err) => println!("An error ocurred while parsing {}", err),
                }
            }
            Err(err) => println!("An error ocurred while parsing: {}", err),
        }
    }
}
