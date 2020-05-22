use std::io;

mod parser;
mod type_checker;

use type_checker::type_::get_type_of;

fn run(input: String) -> Result<(), String> {
    let result = match parser::parse_expression(&input) {
        Ok(value) => value,
        Err(err) => return Err(format!("{}", err)),
    };

    println!("Finished parsing successfully");
    // println!("{:?}", result);

    let inferred = match get_type_of(result) {
        Ok(v) => v,
        Err(err) => return Err(format!("{}", err)),
    };

    println!("Finished type-checking successfully!\n");
    println!("The expression has type {}", inferred);

    return Ok(());
}

fn main() {
    loop {
        println!("Enter a string to lex:");
        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Error reading line");
        println!();

        if let Err(message) = run(input) {
            println!("\n{}", message)
        }
        println!()
    }
}
