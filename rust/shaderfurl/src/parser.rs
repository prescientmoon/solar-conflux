#![allow(unused)]

use crate::lexer::{Lexer, TokenKind};

#[derive(Debug)]
pub struct Parser {
	lexer: Lexer,
	trivia: Vec<crate::lexer::Token>,
	relation: IndentationRelation,
	indentation: usize,
	stop_on_pre: TokenKind,
	stop_on_post: TokenKind,
	reports: Vec<ariadne::Report<'static>>,
}

#[derive(Debug, Clone, Copy)]
pub enum IndentationRelation {
	Gt,
	Gte,
	Eq,
	Any,
}
