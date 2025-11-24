use std::{path::PathBuf, rc::Rc, str::FromStr};

use ariadne::Source;

use crate::{
	elab::ElabContext,
	lexer::{FileId, Lexer, TokenKind},
	lowering::{FromCst, LoweringContext, ModuleId, Name},
	parser::Parser,
};

mod cst;
mod elab;
mod lexer;
mod lowering;
mod parser;

fn main() {
	let buffer = include_str!("../shaders-idea/example.furl").to_string();

	let file_id =
		FileId::new(Rc::from(PathBuf::from_str("repl").unwrap().as_path()));
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

	let file = parser.parse_file();
	println!("========== Parsing");
	println!("{:?}", parser.stop_on_stack);
	println!("{:#?}", file);

	for report in parser.reports() {
		report
			.eprint((file_id.clone(), Source::from(&buffer)))
			.expect("Failed to print report to console ;-;");
	}

	let mut ctx = LoweringContext::default();
	ModuleId::from_cst(&mut ctx, &file.entries);
	println!("{:#?}", ctx);
	let ctx = ElabContext::from_scoping(ctx);

	let second_mod = ctx.lowering_context.inner_module_by_label("second");
	let resolutions = ctx.resolve_path_internally(
		second_mod,
		&[Name::from_str("first"), Name::from_str("something")],
	);

	println!("========== Elaboration");
	for module in resolutions {
		println!("> {}", ctx.print_qualified(module))
	}
}
