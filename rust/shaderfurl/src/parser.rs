#![allow(unused)]

use std::mem;

use ariadne::{ColorGenerator, Label, Report};

use crate::{
	cst::{self, HasTrivia, SeparatedStep, Token},
	lexer::{self, FileId, Lexer, SourcePos, SourceSpan, TokenKind},
};

#[derive(Debug)]
pub struct Parser<'a> {
	lexer: Lexer<'a>,
	token: lexer::Token,
	relation: IndentationRelation,
	indentation: usize,

	// TODO: these should be bitfields
	stop_on_pre: Vec<lexer::TokenKind>,
	stop_on_post: Vec<lexer::TokenKind>,

	trivia: Vec<crate::lexer::Token>,
	reports: Vec<ariadne::Report<'a, SourceSpan>>,
}

#[derive(Debug, Clone, Copy)]
pub enum IndentationRelation {
	Gt,  // >
	Gte, // >=
	Eq,  // =
	Any,
}

impl<'a> Parser<'a> {
	// {{{ Basic constructors / getters / etc
	pub fn new(path: lexer::FileId, source: &'a str) -> Self {
		let mut result = Self {
			lexer: Lexer::new(path.clone(), source),
			indentation: 0,
			relation: IndentationRelation::Any,
			reports: Vec::new(),
			trivia: Vec::new(),
			stop_on_pre: Vec::new(),
			stop_on_post: Vec::new(),

			// NOTE: this is a dummy value that will immediately get replaced by the follow-up
			// .advance call.
			token: lexer::Token {
				span: SourceSpan::new(path, SourcePos::new(0, 1, 1), 0),
				kind: TokenKind::Eof,
			},
		};
		result.advance();
		result
	}

	pub fn reports(&self) -> &[Report<'a, SourceSpan>] {
		&self.reports
	}
	// }}}
	// {{{ Basic combinators
	fn advance(&mut self) -> lexer::Token {
		mem::replace(&mut self.token, self.lexer.next())
	}

	fn expect(&mut self, kind: TokenKind) -> Option<cst::Token> {
		if self.token.kind == kind {
			self.mk_token(|p| {
				p.advance();
				Some(())
			})
		} else {
			None
		}
	}

	fn spanned<T>(
		&mut self,
		parser: impl FnOnce(&mut Self) -> Option<T>,
	) -> Option<(SourceSpan, T)> {
		let start = self.token.span.from;
		let res = parser(self)?;
		let end = self.token.span.from;
		Some((SourceSpan::between(self.lexer.path(), start, end), res))
	}

	fn mk_token<T>(
		&mut self,
		parser: impl FnOnce(&mut Self) -> Option<T>,
	) -> Option<cst::Token<T>> {
		// TODO: check indentation here
		// self.check_indentation(true)

		let (span, value) = self.spanned(parser)?;

		self.relation = IndentationRelation::Gt;
		let mut trivia = mem::take(&mut self.trivia);
		trivia.reverse();

		while matches!(self.token.kind, TokenKind::Junk | TokenKind::Comment) {
			let prev_tok = self.advance();
			self.trivia.push(prev_tok);
		}

		Some(cst::Token {
			trivia,
			span,
			value,
		})
	}

	fn parse_many<T>(&mut self, mut parser: impl Fn(&mut Self) -> Option<T>) -> Vec<T> {
		let mut res = Vec::new();
		while let Some(elem) = parser(self) {
			res.push(elem);
		}

		res
	}
	// }}}
	// {{{ Error recovery building blocks
	fn parse_junk_till<T: HasTrivia>(
		&mut self,
		term: Term,
		mut parser: impl Fn(&mut Self) -> Option<T>,
	) -> T {
		let trivia_amount = self.trivia.len();
		let mut junk = Vec::new();
		let mut res = loop {
			if let Some(res) = parser(self) {
				break res;
			} else {
				junk.push(self.advance());
			}
		};

		if !junk.is_empty() {
			let mut insert_at = trivia_amount;

			self.reports.push(
				Report::build(ariadne::ReportKind::Error, junk.span_of())
					.with_code("Junk")
					.with_message(format!(
						"I was looking for {} when I came across a bunch of unexpected characters.",
						term.articulated
					))
					.with_label(
						Label::new(junk.span_of())
							.with_message("I have no idea what this is supposed to mean..."),
					)
					.finish(),
			);

			for piece in junk {
				if let Some(piece) = res.try_attach_trivia(piece) {
					self.trivia.insert(insert_at, piece);
					insert_at += 1;
				}
			}
		}

		res
	}

	fn try_junk_till<T: HasTrivia>(
		&mut self,
		term: Term,
		mut parser: impl Fn(&mut Self) -> Option<T>,
	) -> Option<T> {
		self.parse_junk_till(term, |p| {
			if p.stop_on_pre.contains(&p.token.kind) {
				Some(None)
			} else if let Some(res) = parser(p) {
				Some(Some(res))
			} else if p.token.kind == TokenKind::Eof || p.stop_on_post.contains(&p.token.kind) {
				Some(None)
			} else {
				None
			}
		})
	}
	// }}}
	// {{{ Repetition combinators
	/// Parse an expression of the form '(' inner ')', where the delimiters can be
	/// any given token.
	fn delimited<T: HasTrivia>(
		&mut self,
		left: TokenKind,
		right: TokenKind,
		inner_term: Term,
		mut parser: impl Fn(&mut Self) -> Option<T>,
	) -> Option<cst::Delimited<Option<T>>> {
		let res_left = self.expect(left)?;
		self.stop_on_post.push(right);
		let res = self.try_junk_till(inner_term, parser);
		self.stop_on_post.pop();
		let res_right = self.try_junk_till(inner_term, |p| p.expect(right));

		Some(cst::Delimited {
			open: res_left,
			close: res_right,
			inner: res,
		})
	}

	/// Parse a list of "things" separated by some other separator.
	///
	/// Not that this parse will keep consuming input until it reaches
	/// an element that satifies the context's Self::stop_on_* conditions.
	fn separated<T: HasTrivia>(
		&mut self,
		sep_term: Term,
		sep: TokenKind,
		inner_term: Term,
		mut parser: impl Fn(&mut Self) -> Option<T>,
	) -> cst::Separated<T> {
		let mut elements = Vec::new();

		while let Some(step) = self.try_junk_till(Term::LIST_ITEM, |p| {
			parser(p)
				.map(SeparatedStep::Element)
				.or_else(|| p.expect(sep).map(SeparatedStep::Separator))
		}) {
			elements.push(step);
		}

		if let Some(SeparatedStep::Separator(sep)) = elements.first() {
			self.reports.push(
				Report::build(ariadne::ReportKind::Error, sep.span_of())
					.with_code("LeadingSeparator")
					.with_message(format!("Leading {} are not allowed.", sep_term.plural))
					.with_label(Label::new(sep.span_of()).with_message(format!(
						"I was expecting to find {} before this {}.",
						inner_term.articulated, sep_term.singular
					)))
					.finish(),
			);
		}

		for win in elements.windows(2) {
			match win {
				[
					SeparatedStep::Element(first),
					SeparatedStep::Element(second),
				] => {
					self.reports.push(
						Report::build(ariadne::ReportKind::Error, first.span_of())
							.with_code("MissingSeparator")
							.with_message(format!(
								"There's a missing {} between two consecutive {}.",
								sep_term.singular, inner_term.plural
							))
							.with_label(
								Label::new(first.span_of())
									.with_message(format!("First {}", inner_term.singular))
									.with_order(1),
							)
							.with_label(
								Label::new(second.span_of())
									.with_message(format!("Second {}", inner_term.singular))
									.with_order(0),
							)
							.finish(),
					);
				}
				[
					SeparatedStep::Separator(first),
					SeparatedStep::Separator(second),
				] => {
					self.reports.push(
						Report::build(ariadne::ReportKind::Error, first.span_of())
							.with_code("MissingElements")
							.with_message(format!(
								"There's a missing {} between two consecutive {}.",
								inner_term.singular, sep_term.plural
							))
							.with_label(
								Label::new(first.span_of())
									.with_message(format!("First {}", sep_term.singular))
									.with_order(1),
							)
							.with_label(
								Label::new(second.span_of())
									.with_message(format!("Second {}", sep_term.singular))
									.with_order(0),
							)
							.finish(),
					);
				}
				_ => continue,
			}
		}

		cst::Separated { elements }
	}

	/// Wrapper around [Self::delimited] and [Self::separated].
	fn delimited_list<T: HasTrivia>(
		&mut self,
		left: TokenKind,
		right: TokenKind,
		sep_term: Term,
		sep: TokenKind,
		inner_term: Term,
		parser: impl Fn(&mut Self) -> Option<T>,
	) -> Option<cst::Delimited<cst::Separated<T>>> {
		let res = self.delimited(left, right, inner_term, |p| {
			Some(p.separated(sep_term, sep, inner_term, &parser))
		})?;
		Some(cst::Delimited {
			open: res.open,
			inner: res.inner.unwrap_or_else(|| cst::Separated {
				elements: Vec::new(),
			}),
			close: res.close,
		})
	}
	// }}}
	// {{{ Expression parsing
	fn parse_int(&mut self) -> Option<i64> {
		if self.token.kind == TokenKind::Integer {
			let tok = self.lexer.source_span(&self.token.span);
			let result = str::parse(tok).ok()?;
			self.advance();
			Some(result)
		} else {
			None
		}
	}

	fn parse_name(&mut self) -> Option<String> {
		let tok = self.expect(TokenKind::Identifier)?;
		Some(self.lexer.source_span(&tok.span).to_string())
	}

	fn parse_int_expr(&mut self) -> Option<cst::Expr> {
		let int = self.mk_token(Self::parse_int)?;
		Some(cst::Expr::Int(int))
	}

	fn parse_var_expr(&mut self) -> Option<cst::Expr> {
		let name = self.mk_token(Self::parse_name)?;
		Some(cst::Expr::Variable(name))
	}

	fn parse_wrapped_expr(&mut self) -> Option<cst::Expr> {
		let result = self.delimited(
			TokenKind::LeftParen,
			TokenKind::RightParen,
			Term::EXPR,
			|p| p.parse_expr().map(Box::new),
		)?;
		Some(cst::Expr::Wrapped(result))
	}

	// Parses a base expression
	fn parse_expr_atom(&mut self) -> Option<cst::Expr> {
		self.parse_var_expr()
			.or_else(|| self.parse_int_expr())
			.or_else(|| self.parse_wrapped_expr())
	}

	pub fn parse_expr_call(&mut self) -> Option<cst::Expr> {
		let callee = self.parse_expr_atom()?;
		let arguments = self.delimited_list(
			TokenKind::LeftParen,
			TokenKind::RightParen,
			Term::COMMA,
			TokenKind::Comma,
			Term::EXPR,
			Self::parse_expr,
		);

		Some(if let Some(arguments) = arguments {
			cst::Expr::Call(cst::Call {
				callee: Box::new(callee),
				arguments,
			})
		} else {
			callee
		})
	}

	fn parse_expr(&mut self) -> Option<cst::Expr> {
		self.parse_expr_call()
	}
	// }}}
}

// {{{ Error reporting
#[derive(Debug, Clone, Copy)]
pub struct Term {
	singular: &'static str,    // train
	plural: &'static str,      // trains
	articulated: &'static str, // a train
}

impl Term {
	pub const fn new(
		singular: &'static str,
		plural: &'static str,
		articulated: &'static str,
	) -> Self {
		Self {
			singular,
			plural,
			articulated,
		}
	}

	pub const COMMA: Self = Self::new("comma", "commas", "a comma");
	pub const EXPR: Self = Self::new("expression", "expressions", "an expression");
	pub const LIST_ITEM: Self = Self::new("list item", "list items", "a list item");
}
// }}}
