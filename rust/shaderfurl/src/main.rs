use std::{io, path::PathBuf, rc::Rc, str::FromStr};

use ariadne::Source;

use crate::{
	lexer::{FileId, Lexer, TokenKind},
	parser::Parser,
};

mod ast;
mod cst;
mod database;
mod lexer;
mod parser;

fn main() {
	let mut buffer = String::new();
	let stdin = io::stdin();
	stdin.read_line(&mut buffer).unwrap();

	let file_id = FileId::new(Rc::from(PathBuf::from_str("repl").unwrap().as_path()));
	let mut lexer = Lexer::new(file_id.clone(), &buffer);

	loop {
		let token = lexer.next();

		println!("{:?}: {}", token.kind, lexer.source_span(&token.span));

		if matches!(token.kind, TokenKind::Eof) {
			break;
		}
	}

	let mut parser = Parser::new(file_id.clone(), &buffer);
	let expr = parser.parse_expr_call();

	println!("Parse result: {:?}", expr);

	for report in parser.reports() {
		report
			.eprint((file_id.clone(), Source::from(&buffer)))
			.expect("Failed to print report to console ;-;");
	}
}
