use std::io;

mod lexer;

fn main() {
    loop {
        println!("Enter a string to lex:");
        let mut input = String::new();

        io::stdin()
            .read_line(&mut input)
            .expect("Error reading line");

        match lexer::lex(input[..].as_bytes()) {
            Ok((_, result)) => {
                println!("Parsed string successfully!");
                for token in result.iter() {
                    println!("{:?}", token)
                }
            }
            Err(err) => println!("An error ocurred while parsing: {}", err),
        }
    }
}
