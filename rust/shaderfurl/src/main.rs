use std::{path::PathBuf, rc::Rc, str::FromStr};

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
	let buffer = include_str!("../shaders-idea/example.furl").to_string();

	let file_id = FileId::new(Rc::from(PathBuf::from_str("repl").unwrap().as_path()));
	let mut lexer = Lexer::new(file_id.clone(), &buffer);

	println!("========== Lexing");
	loop {
		let token = lexer.next();

		println!("{:?}: {}", token.kind, lexer.source_span(&token.span));

		if matches!(token.kind, TokenKind::Eof) {
			break;
		}
	}

	let mut parser = Parser::new(file_id.clone(), &buffer);

	let stm = parser.parse_statement();
	if let Some(stm) = stm {
		println!("========== Parsing");
		println!("{:#?}", stm);
		println!("{:?}", parser.stop_on_stack);
	}

	for report in parser.reports() {
		report
			.eprint((file_id.clone(), Source::from(&buffer)))
			.expect("Failed to print report to console ;-;");
	}
}
