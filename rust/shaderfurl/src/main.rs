use std::io;

use crate::lexer::{Lexer, TokenKind};

mod cst;
mod database;
mod lexer;

fn main() {
	let mut buffer = String::new();
	let stdin = io::stdin();
	stdin.read_line(&mut buffer).unwrap();

	let mut lexer = Lexer::new(buffer);

	loop {
		let token = lexer.pull();

		println!(
			"{:?}: {}",
			token.kind,
			&lexer.source[token.span.from.index..token.span.from.index + token.span.length]
		);

		if matches!(token.kind, TokenKind::Eof) {
			break;
		}
	}
}
