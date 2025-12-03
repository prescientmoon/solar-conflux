use std::rc::Rc;

use ariadne::Source;

use crate::{
	elab::ElabContext,
	lexer::{FileId, Lexer, TokenKind},
	lowering::{FromCst, LoweringContext, ModuleId},
	parser::Parser,
};

mod cst;
mod digraph;
mod elab;
mod errors;
mod lexer;
mod lowering;
mod parser;
mod telescope;

#[derive(Default)]
struct PathCache {
	paths: Vec<(Rc<String>, ariadne::Source<String>)>,
}

impl PathCache {
	fn add_file(&mut self, path: &str, contents: &str) -> FileId {
		self.paths.push((
			Rc::new(path.to_string()),
			ariadne::Source::from(contents.to_string()),
		));
		FileId(self.paths.len() - 1)
	}
}

impl ariadne::Cache<FileId> for &PathCache {
	#[allow(refining_impl_trait)]
	fn fetch(&mut self, id: &FileId) -> Result<&Source<Self::Storage>, String> {
		Ok(&self
			.paths
			.get(id.0)
			.ok_or_else(|| format!("Cannot find file with id {id}"))?
			.1)
	}

	fn display<'a>(
		&self,
		id: &'a FileId,
	) -> Option<impl std::fmt::Display + 'a> {
		Some(self.paths.get(id.0)?.0.clone())
	}

	type Storage = String;
}

fn main() {
	let buffer = include_str!("../shaders-idea/example.furl").to_string();

	let mut source_cache = PathCache::default();
	let file_id = source_cache.add_file("repl", &buffer);
	let mut lexer = Lexer::new(file_id, &buffer);

	println!("========== Lexing");
	loop {
		let token = lexer.next();

		println!("{:?}: {}", token.kind, lexer.source_span(&token.span));

		if matches!(token.kind, TokenKind::Eof) {
			break;
		}
	}

	let mut parser = Parser::new(file_id, &buffer);

	let file = parser.parse_file();
	println!("========== Parsing");
	// println!("{:#?}", file);

	for report in parser.reports().iter() {
		report
			.eprint(&source_cache)
			.expect("Failed to print report to console ;-;");
	}

	let mut ctx = LoweringContext::default();
	ModuleId::from_cst(&mut ctx, &file.entries);
	// println!("{:#?}", ctx);
	println!("========== Elaboration");
	let ctx = ElabContext::from_scoping(ctx);
	ctx.elab_toplevel();

	for report in ctx.reports().iter() {
		report
			.eprint(&source_cache)
			.expect("Failed to print report to console ;-;");
	}
}
