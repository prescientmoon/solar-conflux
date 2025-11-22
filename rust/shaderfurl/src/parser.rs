#![allow(unused)]

use std::mem;
use std::str::FromStr;

use ariadne::{ColorGenerator, Label, Report};
use enumset::{EnumSet, enum_set, enum_set_union};

use crate::cst::{
	self, BinaryOperator, CondBranch, HasSpan, SeparatedStep, Token,
};
use crate::lexer::{self, FileId, Lexer, SourcePos, SourceSpan, TokenKind};

// {{{ Error reporting macros
/// Helper for creating a report via ariadne
#[macro_export]
macro_rules! report {
    (
        $self:expr,
        $code:expr,
        $span: expr,
        ($($msg:tt)+),
        $(($lspan: expr, $($lmsg:tt)+),)*
    ) => {{
        use ariadne::{Report, ReportKind, Label};

        let mut report = Report::build(ReportKind::Error, $span.span_of())
            .with_code($code)
            .with_message(format!($($msg)+));

        $(
            report = report.with_label(
                Label::new($lspan.span_of()).with_message(format!($($lmsg)+))
            );
        )*

        $self.reports.push(report.finish());
    }};
}

/// A report where the source spans are all identical.
#[macro_export]
macro_rules! compact_report {
    (
        ($self:expr, $code:expr, $span: expr),
        ($($msg:tt)+),
        $(($($lmsg:tt)+),)*
    ) => {{
        report!(
            $self, $code, $span, ($($msg)+),
            $(($span,$($lmsg)+),)*
        )
    }};
}
// }}}
// {{{ Parsing types
pub type TokenSet = EnumSet<TokenKind>;

#[derive(Debug, Clone, Copy)]
pub struct Block(usize);

#[derive(Debug)]
pub struct Parser<'a> {
	lexer: Lexer<'a>,
	token: lexer::Token,

	/// We've consumed nothing but junk/comments since this index
	trivia_range_start: usize,

	relation: IndentationRelation,
	indentation: usize,

	// TODO: make this private after finishing with debugging
	pub stop_on_stack: Vec<TokenSet>,
	reports: Vec<ariadne::Report<'a, SourceSpan>>,
	tokens: Vec<lexer::Token>,
}

#[derive(Debug, Clone, Copy)]
pub enum IndentationRelation {
	Gt,  // >
	Gte, // >=
	Eq,  // =
	Any, // ⊤
}

#[derive(Default)]
struct StopOnStackKey();

impl Drop for StopOnStackKey {
	fn drop(&mut self) {
		panic!("stop_on_stack keys cannot be dropped")
	}
}
// }}}

impl<'a> Parser<'a> {
	// {{{ Basic constructors / getters / etc
	pub fn new(path: lexer::FileId, source: &'a str) -> Self {
		let mut lexer = Lexer::new(path.clone(), source);
		let token = lexer.next();

		let mut result = Self {
			lexer,
			token,
			indentation: 1,
			relation: IndentationRelation::Any,
			reports: Vec::new(),
			trivia_range_start: 0,
			stop_on_stack: Vec::new(),
			tokens: Vec::new(),
		};

		mem::forget(result.stop_on_push(TokenKind::Eof.into()));
		result
	}

	pub fn reports(&self) -> &[Report<'a, SourceSpan>] {
		&self.reports
	}
	// }}}
	// {{{ stop_on stack
	fn stop_on(&self) -> TokenSet {
		self.stop_on_stack.last().copied().unwrap_or_default()
	}

	#[must_use]
	fn stop_on_push(&mut self, s: TokenSet) -> StopOnStackKey {
		self.stop_on_stack.push(self.stop_on() | s);
		StopOnStackKey::default()
	}

	fn stop_on_pop(&mut self, key: StopOnStackKey) {
		self.stop_on_stack
			.pop()
			.expect("Attempted to pop empty stop_on stack");
		mem::forget(key);
	}

	fn with_stopper<T>(
		&mut self,
		s: TokenSet,
		mut parser: impl FnMut(&mut Self) -> T,
	) -> T {
		let key = self.stop_on_push(s);
		let result = parser(self);
		self.stop_on_pop(key);
		result
	}
	// }}}

	// {{{ Basic combinators
	fn advance(&mut self) {
		let token = mem::replace(&mut self.token, self.lexer.next());
		self.tokens.push(token);
	}

	fn check_indentation(&mut self, should_report: bool) -> (bool, usize) {
		let col = self.token.span.from.col;
		let ok = match self.relation {
			IndentationRelation::Eq => col == self.indentation,
			IndentationRelation::Gt => col > self.indentation,
			IndentationRelation::Gte => col >= self.indentation,
			IndentationRelation::Any => true,
		};

		if should_report && !ok {
			report!(
				self,
				"IllegalIndentation",
				self.token,
				(
					"Expected an indentation level of {} {}. Got {col} instead.",
					match self.relation {
						IndentationRelation::Eq => "precisely",
						IndentationRelation::Gt => "more than",
						IndentationRelation::Gte => "at least",
						IndentationRelation::Any => unreachable!(),
					},
					self.indentation
				),
				(self.token, "Actual indentation"),
			);
		}

		(ok, col)
	}
	// }}}
	// {{{ Error recovery building blocks
	// TODO: allow passing in a flag / set of tokens which report indentation mismatches but keep
	// reading. Useful for things like the "do" keyword for a for-loop.
	fn expect_tolerant(
		&mut self,
		kinds: TokenSet,
	) -> Option<Token<lexer::TokenKind>> {
		loop {
			let (ok, _) = self.check_indentation(false);
			if !ok {
				return None;
			}

			if kinds.contains(self.token.kind) {
				// Steal the current token
				let tok = self.token.clone();
				self.advance();

				// Reset the indentation relation to the default
				self.relation = IndentationRelation::Gt;

				// Save the currently accumulated trivia
				let trivia_range = (self.trivia_range_start, self.tokens.len());

				// Find consecutive ranges of junk input and report them
				let mut span_acc: Option<lexer::SourceSpan> = None;
				for ix in trivia_range.0..trivia_range.1 {
					let tok = &self.tokens[ix];

					if let Some(prev_span_acc) = mem::take(&mut span_acc) {
						if tok.kind == TokenKind::Junk {
							span_acc = Some(prev_span_acc.merge(&tok.span));
						}

						// If we no longer have junk OR this is the end, report the error
						if tok.kind != TokenKind::Junk
							|| ix == trivia_range.1 - 1
						{
							let span =
								span_acc.as_ref().unwrap_or(&prev_span_acc);
							compact_report!(
								(self, "JunkInput", span),
								("Encountered junk input"),
								("I have no idea what this is supposed to mean"),
							);
						}
					} else if tok.kind == TokenKind::Junk {
						span_acc = Some(tok.span_of());
					}
				}

				// Consume follow-up trivia
				self.trivia_range_start = self.tokens.len();
				while matches!(
					self.token.kind,
					TokenKind::Junk | TokenKind::Comment
				) {
					self.advance();
				}

				return Some(cst::Token {
					trivia: trivia_range,
					span: tok.span,
					value: tok.kind,
				});
			}

			// Stoppers automatically stop the parser
			if self.stop_on().contains(self.token.kind) {
				return None;
			}

			// Otherwise, just mark the current token as junk and keep going
			self.advance();
			let kind = &mut self.tokens.last_mut().unwrap().kind;
			if *kind != TokenKind::Comment {
				*kind = TokenKind::Junk;
			}
		}
	}
	// }}}
	// {{{ Blocks
	pub fn parse_block<T>(
		&mut self,
		mut parser: impl FnMut(&mut Self) -> Option<T>,
	) -> Vec<T> {
		let (valid, col) = self.check_indentation(false);
		if !valid {
			return Vec::new();
		}

		let original_indentation = mem::take(&mut self.indentation);
		let mut result = Vec::new();

		loop {
			self.relation = IndentationRelation::Eq;
			mem::replace(&mut self.indentation, col);
			if let Some(t) = parser(self) {
				result.push(t);
			} else {
				break;
			}
		}

		self.relation = IndentationRelation::Gt;
		self.indentation = original_indentation;

		result
	}
	// }}}
	// {{{ Literal helpers
	fn embed_source(&self, tok: Token<TokenKind>) -> Token<String> {
		let source = self.lexer.source_span(&tok.span);
		tok.set(source.to_string())
	}

	fn parse_integer_literal<I: FromStr>(
		&mut self,
		tok: &Token<TokenKind>,
	) -> Option<I> {
		let source = self.lexer.source_span(&tok.span);
		let result = str::parse(source).ok();
		if let Some(result) = result {
			Some(result)
		} else {
			compact_report!(
				(self, "InvalidInteger", tok),
				("'{source}' is not a valid integer literal"),
				("This is not a valid integer"),
			);
			None
		}
	}
	// }}}

	// {{{ Expression parsing
	const ATOM_EXPR_MARKER: TokenSet = enum_set!(
		TokenKind::Integer
			| TokenKind::Float
			| TokenKind::Identifier
			| TokenKind::LeftParen
	);

	// Parses an atom expression
	fn parse_expr_atom(&mut self) -> Option<cst::Expr> {
		let tok = self.expect_tolerant(Self::ATOM_EXPR_MARKER)?;

		let source = self.lexer.source_span(&tok.span);
		match tok.value {
			TokenKind::Integer => {
				let result = self.parse_integer_literal(&tok);
				if let Some(result) = result {
					Some(cst::Expr::Int(tok.set(result)))
				} else {
					Some(cst::Expr::Error(tok.set(())))
				}
			}
			TokenKind::Float => {
				let result = str::parse(source).ok();
				if let Some(result) = result {
					Some(cst::Expr::Float(tok.set(result)))
				} else {
					compact_report!(
						(self, "InvalidFloat", tok),
						("'{source}' is not a valid floating point literal"),
						("This is not a valid floating point number"),
					);
					Some(cst::Expr::Error(tok.set(())))
				}
			}
			TokenKind::Identifier => {
				Some(cst::Expr::Variable(self.embed_source(tok)))
			}
			TokenKind::LeftParen => {
				let inner = self
					.with_stopper(TokenKind::RightParen.into(), |parser| {
						parser.parse_expr()
					});

				let close = self.expect_tolerant(TokenKind::RightParen.into());

				if close.is_none() {
					self.reports.push(
						Report::build(
							ariadne::ReportKind::Error,
							tok.span_of(),
						)
						.with_code("MissingClosingParenthesis")
						.with_message("Expected )")
						.with_labels(inner.as_ref().map(|i| {
							Label::new(i.span_of())
								.with_message(
									"This is the expression being wrapped",
								)
								.with_order(0)
						}))
						.with_label(
							Label::new(tok.span_of())
								.with_message(
									"This parenthesis is never closed",
								)
								.with_order(1),
						)
						.finish(),
					);
				}

				let inner_exists = inner.is_some();
				let right_paren_exists = close.is_some();
				let expr = cst::Expr::Wrapped(cst::Delimited {
					open: tok.set(()),
					inner: inner.map(Box::new),
					close: close.map(|c| c.set(())),
				});

				if !inner_exists && right_paren_exists {
					compact_report!(
						(self, "MissingExpression", expr),
						("Expected parenthesis to wrap expression"),
						("I was expecting an expression to lie here"),
					);
				}

				Some(expr)
			}
			_ => unreachable!(),
		}
	}

	const PROP_EXPR_MARKER: TokenSet =
		enum_set!(Self::ATOM_EXPR_MARKER | TokenKind::Property);

	fn parse_prop_access(&mut self) -> Option<cst::Expr> {
		let mut expr = self
			.with_stopper(TokenKind::Property.into(), |parser| {
				parser.parse_expr_atom()
			});

		loop {
			let prop = self.expect_tolerant(TokenKind::Property.into());

			if let Some(prop) = prop {
				let source = self.lexer.source_span(&prop.span);

				if expr.is_none() {
					compact_report!(
						(self, "MissingExpression", expr),
						("Attempting to access a property of a missing expression"),
						("I was expecting an expression before this property access"),
					);
				}

				expr = Some(cst::Expr::Property(
					expr.map(Box::new),
					prop.set(source[1..].to_string()),
				));
			} else {
				return expr;
			}
		}
	}

	fn parse_call(&mut self) -> Option<cst::Expr> {
		self.with_stopper(Self::ATOM_EXPR_MARKER, |parser| {
			let base = parser.parse_prop_access();
			if let Some(base) = base {
				let mut args = Vec::new();

				while let Some(expr) = parser.parse_prop_access() {
					args.push(expr);
				}

				if args.is_empty() {
					Some(base)
				} else {
					Some(cst::Expr::Call(
						Box::new(base),
						args.into_boxed_slice(),
					))
				}
			} else {
				None
			}
		})
	}

	const UNARY_OP_MARKER: TokenSet = enum_set!(
		TokenKind::Plus
			| TokenKind::Minus
			| TokenKind::Not
			| TokenKind::BitwiseNot
	);
	const UNARY_EXPR_MARKER: TokenSet =
		enum_set!(Self::UNARY_OP_MARKER | Self::PROP_EXPR_MARKER);

	fn parse_unary(&mut self) -> Option<cst::Expr> {
		let mut ops = Vec::new();

		self.with_stopper(Self::PROP_EXPR_MARKER, |parser| {
			while let Some(op) = parser.expect_tolerant(Self::UNARY_OP_MARKER) {
				let unary =
					cst::UnaryOperator::from_token_kind(op.value).unwrap();
				ops.push(op.set(unary));
			}
		});

		let mut expr = self.parse_call();
		if let Some(last_op) = ops.last().as_ref()
			&& expr.is_none()
		{
			compact_report!(
				(self, "MissingExpression", last_op),
				("Unary operator {} found without expression", last_op.value),
				("I was expecting this operator to precede an expression"),
			);
		}

		while let Some(op) = ops.pop() {
			expr = Some(cst::Expr::Unary(op, expr.map(Box::new)))
		}

		expr
	}

	// Operator precedences!
	//
	// Higher power means the operator binds tighter. For example, multiplication
	// has a higher power than addition.
	const BIN_OP_POWERS: &'static [(TokenKind, usize)] = &[
		// Multiplicative
		(TokenKind::Multiply, 1000),
		(TokenKind::Divide, 1000),
		// Additive
		(TokenKind::Plus, 900),
		(TokenKind::Minus, 900),
		// Min/max
		(TokenKind::Meet, 850),
		(TokenKind::Join, 800),
		// Bitwise
		(TokenKind::LeftShift, 700),
		(TokenKind::RightShift, 700),
		// Comparison
		(TokenKind::DoubleEqual, 600),
		(TokenKind::NotEqual, 600),
		(TokenKind::GreaterThan, 600),
		(TokenKind::LesserThan, 600),
		(TokenKind::GreaterOrEqual, 600),
		(TokenKind::LesserOrEqual, 600),
		// Logical
		(TokenKind::And, 550),
		(TokenKind::Or, 500),
		(TokenKind::Xor, 500),
	];

	const BIN_OP_MARKERS: TokenSet = enum_set!(
		TokenKind::Multiply
			| TokenKind::Divide
			| TokenKind::Plus
			| TokenKind::Minus
			| TokenKind::Meet
			| TokenKind::Join
			| TokenKind::LeftShift
			| TokenKind::RightShift
			| TokenKind::DoubleEqual
			| TokenKind::NotEqual
			| TokenKind::GreaterThan
			| TokenKind::LesserThan
			| TokenKind::GreaterOrEqual
			| TokenKind::LesserOrEqual
			| TokenKind::And
			| TokenKind::Or
			| TokenKind::Xor
	);

	const BIN_EXPR_MARKER: TokenSet =
		enum_set!(Self::BIN_OP_MARKERS | Self::UNARY_EXPR_MARKER);

	fn parse_binary(&mut self, lower_power_bound: usize) -> Option<cst::Expr> {
		let mut marker = TokenSet::default();

		for (op, power) in Self::BIN_OP_POWERS {
			if *power > lower_power_bound {
				marker.insert(*op);
			}
		}

		self.with_stopper(marker, |parser| {
			let mut lhs = parser.parse_unary();

			// The marker guarantees that we respect the given lower bound
			while let Some(op_tok) = parser.expect_tolerant(marker) {
				let bin_op =
					cst::BinaryOperator::from_token_kind(op_tok.value).unwrap();
				let power = Self::BIN_OP_POWERS
					.iter()
					.find(|(k, _)| *k == op_tok.value)
					.unwrap()
					.1;

				if lhs.is_none() {
					compact_report!(
						(parser, "MissingExpression", op_tok),
						("Binary operator {bin_op} has no left hand side"),
						("I was expecting this operator to follow an expression"),
					);
				}

				let rhs = parser.parse_binary(power);

				if rhs.is_none() {
					compact_report!(
						(parser, "MissingExpression", op_tok),
						("Binary operator {bin_op} has no right hand side"),
						("I was expecting this operator to precede an expression"),
					);
				}

				lhs = Some(cst::Expr::Binary(
					lhs.map(Box::new),
					op_tok.set(bin_op),
					rhs.map(Box::new),
				));
			}

			lhs
		})
	}

	const TERNARY_OP_MARKER: TokenSet = enum_set!(TokenKind::QuestionMark);
	const TERNARY_EXPR_MARKER: TokenSet =
		enum_set!(Self::TERNARY_OP_MARKER | Self::BIN_EXPR_MARKER);
	fn parse_ternary(&mut self) -> Option<cst::Expr> {
		let lhs = self.with_stopper(Self::TERNARY_OP_MARKER, |parser| {
			parser.parse_binary(0)
		});
		let (qm, ihs) = self.with_stopper(TokenKind::Colon.into(), |parser| {
			let qm = parser.expect_tolerant(TokenKind::QuestionMark.into());
			let ihs = if qm.is_some() {
				parser.parse_ternary()
			} else {
				None
			};

			(qm, ihs)
		});

		let colon = if qm.is_some() {
			self.expect_tolerant(TokenKind::Colon.into())
		} else {
			None
		};

		let rhs = if colon.is_some() {
			self.parse_ternary()
		} else {
			None
		};

		if qm.is_some() && lhs.is_none() {
			compact_report!(
				(self, "MissingExpression", qm),
				("Ternary operator is missing condition"),
				("I was expecting an expression before this question mark"),
			);
		}

		if qm.is_some() && ihs.is_none() {
			compact_report!(
				(self, "MissingExpression", qm),
				("Ternary operator is missing <then> branch"),
				("I was expecting an expression after this question mark"),
			);
		}

		if colon.is_some() && rhs.is_none() {
			compact_report!(
				(self, "MissingExpression", colon),
				("Ternary operator is missing <else> branch"),
				("I was expecting an expression after this colon"),
			);
		}

		if qm.is_some() && colon.is_none() {
			compact_report!(
				(self, "MissingPunctuation", qm),
				("Ternary operator is missing colon"),
				("I was expecting an associated : to this ?"),
			);
		}

		if qm.is_some() || colon.is_some() {
			Some(cst::Expr::Ternary(
				lhs.map(Box::new),
				qm.map(|v| v.set(())),
				ihs.map(Box::new),
				colon.map(|v| v.set(())),
				rhs.map(Box::new),
			))
		} else {
			lhs
		}
	}

	const EXPR_MARKER: TokenSet = Self::TERNARY_EXPR_MARKER;
	pub fn parse_expr(&mut self) -> Option<cst::Expr> {
		self.parse_ternary()
	}
	// }}}
	// {{{ Type parsing
	const TYPE_MARKER: TokenSet = enum_set!(
		TokenKind::LeftBracket | TokenKind::Identifier | TokenKind::Struct
	);

	fn parse_type(&mut self) -> Option<cst::Type> {
		let tok = self.expect_tolerant(Self::TYPE_MARKER)?;

		match tok.value {
			TokenKind::Identifier => {
				Some(cst::Type::Named(self.embed_source(tok)))
			}
			TokenKind::LeftBracket => {
				let (dims, close) = self.with_stopper(Self::TYPE_MARKER, |parser| {
					let dims = parser.with_stopper(TokenKind::RightBracket.into(), |parser| {
						let first = parser.with_stopper(TokenKind::Comma.into(), |parser| {
							let tok = parser.expect_tolerant(TokenKind::Integer.into())?;
							let inner = parser.parse_integer_literal(&tok)?;
							Some(tok.set(inner))
						})?;

						let comma = parser.expect_tolerant(TokenKind::Comma.into());
						let second =
							parser
								.expect_tolerant(TokenKind::Integer.into())
								.and_then(|tok| {
									parser
										.parse_integer_literal(&tok)
										.map(|inner| tok.set(inner))
								});

						if comma.is_none() && second.is_some() {
							compact_report!(
								(parser, "MissingComma", (&first, &second)),
								("Missing comma between array dimensions"),
								("The given dimensions must be separated by a comma"),
							);
						} else if comma.is_some() && second.is_none() {
							compact_report!(
								(parser, "MissingDimension", comma),
								("Expected second array dimension after comma"),
								("I was expecting a second integer dimension after this comma"),
							);
						}

						Some(cst::ArrayDimensions {
							first,
							comma: comma.map(|t| t.set(())),
							second,
						})
					});

					let close = parser.expect_tolerant(TokenKind::RightBracket.into());

					if dims.is_some() && close.is_none() {
						compact_report!(
							(parser, "MissingClosingBracket", dims),
							("Missing closing bracket for array dimensions"),
							("I was expecting a ']' after the list of array dimensions"),
						);
					} else if dims.is_none() && close.is_some() {
						compact_report!(
							(parser, "MissingArrayDimensions", (&tok, &close)),
							("Array type is missing dimensions"),
							("I was expecting array-dimensions to be specified between these brackets"),
						);
					}

					(dims, close)
				});

				let inner = self.parse_type();
				if inner.is_none() {
					compact_report!(
						(self, "MissingType", (&tok, &dims, &close)),
						("Array element type is missing"),
						("These array dimensions are missing an inner type"),
					);
				}

				Some(cst::Type::Array(cst::Array {
					dimensions: cst::Delimited {
						open: tok.set(()),
						inner: dims,
						close: close.map(|c| c.set(())),
					},
					ty: inner.map(Box::new),
				}))
			}
			TokenKind::Struct => {
				let fields = self.parse_block(|parser| {
					let name = parser.with_stopper(
						Self::TYPE_MARKER | TokenKind::Colon,
						|parser| {
							parser.expect_tolerant(TokenKind::Identifier.into())
						},
					);
					let colon =
						parser.with_stopper(Self::TYPE_MARKER, |parser| {
							parser.expect_tolerant(TokenKind::Colon.into())
						});
					let ty = parser.parse_type();

					if name.is_none() && colon.is_none() && ty.is_none() {
						return None;
					}

					if name.is_none() {
						compact_report!(
							(parser, "MissingName", (&colon, &ty)),
							("Missing struct field name"),
							("I was expecting a struct field's name here"),
						);
					} else if colon.is_none() {
						compact_report!(
							(parser, "MissingColon", &name),
							("Missing colon"),
							("I was expecting a colon for the given struct field"),
						);
					} else if ty.is_none() {
						compact_report!(
							(parser, "MissingType", (&name, &colon)),
							("Missing type"),
							("I was expecting a type for the given struct field"),
						);
					}

					Some(cst::StructField {
						name: name.map(|tok| parser.embed_source(tok)),
						colon: colon.map(|t| t.set(())),
						ty,
					})
				});

				if fields.is_empty() {
					compact_report!(
						(self, "MissingFields", (&tok)),
						("Empty structs are not supported"),
						("I was expecting a list of fields"),
					);
				}

				Some(cst::Type::Struct(cst::Struct { fields }))
			}
			_ => unreachable!(),
		}
	}
	// }}}
	// {{{ Statement parsing
	const NON_EXPR_STATEMENT_MARKER: TokenSet = enum_set!(
		TokenKind::Break
			| TokenKind::Discard
			| TokenKind::Continue
			| TokenKind::Return
			| TokenKind::For
			| TokenKind::If
	);

	const ASSIGNMENT_MARKER: TokenSet = enum_set!(
		TokenKind::SingleEqual
			| TokenKind::PlusEqual
			| TokenKind::MinusEqual
			| TokenKind::MultiplyEqual
			| TokenKind::DivideEqual
	);

	const STATEMENT_MARKER: TokenSet = enum_set!(
		Self::NON_EXPR_STATEMENT_MARKER
			| Self::TERNARY_EXPR_MARKER
			| Self::ASSIGNMENT_MARKER
			| TokenKind::Colon
	);

	pub fn parse_statement(&mut self) -> Option<cst::Statement> {
		let non_expr_marker_key =
			self.stop_on_push(Self::NON_EXPR_STATEMENT_MARKER);

		let expr = self.with_stopper(
			TokenKind::Colon | Self::ASSIGNMENT_MARKER,
			|parser| parser.parse_expr(),
		);

		let result = if let Some(op_tok) =
			self.expect_tolerant(TokenKind::Colon | Self::ASSIGNMENT_MARKER)
		{
			match op_tok.value {
				TokenKind::Colon => {
					// The fact we allow full-blown expressions here is bad
					// (as in, we might be able to get better results by
					// being error-tolerant), although it is the simplest solution
					// for now. Implementing this "properly" requires backtracking,
					// which can be arbitrarily expensive because our context contains
					// vecs of arbitrary length.
					if !matches!(&expr, Some(cst::Expr::Variable(name))) {
						self.reports.push(
							Report::build(
								ariadne::ReportKind::Error,
								op_tok.span_of(),
							)
							.with_code("InvalidDeclarationName")
							.with_message(
								"Expected name on the left hand side of a declaration",
							)
							.with_label(
								Label::new(op_tok.span_of()).with_message(
									"I was expecting a name before this :",
								),
							)
							.finish(),
						)
					}

					let ty = self.with_stopper(
						TokenKind::SingleEqual | Self::EXPR_MARKER,
						|parser| parser.parse_type(),
					);

					let eq_tok =
						self.with_stopper(Self::EXPR_MARKER, |parser| {
							parser
								.expect_tolerant(TokenKind::SingleEqual.into())
						});

					let rhs = self.parse_expr();
					if eq_tok.is_some() && rhs.is_none() {
						self.reports.push(
							Report::build(
								ariadne::ReportKind::Error,
								(&expr, &op_tok, &eq_tok).span_of(),
							)
							.with_code("MissingExpression")
							.with_message("Declaration has no right hand side")
							.with_label(
								Label::new(op_tok.span_of()).with_message(
									"I was expecting an expression after this declaration",
								),
							)
							.finish(),
						);
					}

					Some(cst::Statement::Declaration(cst::LocalDeclaration {
						variable: expr,
						colon: op_tok.set(()),
						ty,
						equals: eq_tok.map(|v| v.set(())),
						value: rhs,
					}))
				}
				other => {
					let bin_op = match other {
						TokenKind::SingleEqual => None,
						TokenKind::PlusEqual => Some(BinaryOperator::Plus),
						TokenKind::MinusEqual => Some(BinaryOperator::Minus),
						TokenKind::MultiplyEqual => {
							Some(BinaryOperator::Multiply)
						}
						TokenKind::DivideEqual => Some(BinaryOperator::Divide),
						_ => unreachable!(),
					};

					if expr.is_none() {
						compact_report!(
							(self, "MissingSemicolon", &op_tok),
							("Assignment has no left hand side"),
							("I was expecting an expression before this ="),
						);
					}

					let rhs = self.parse_expr();
					if rhs.is_none() {
						compact_report!(
							(self, "MissingSemicolon", &op_tok),
							("Assignment has no left hand side"),
							("I was expecting an expression after this ="),
						);
					}

					Some(cst::Statement::Assignment(
						expr,
						op_tok.set(bin_op),
						rhs,
					))
				}
			}
		} else {
			expr.map(cst::Statement::Expression)
		};

		self.stop_on_pop(non_expr_marker_key);
		if result.is_some() {
			return result;
		}

		// We're done looking for expression-like statements!
		let tok = self.expect_tolerant(Self::NON_EXPR_STATEMENT_MARKER)?;
		match tok.value {
			TokenKind::Discard => Some(cst::Statement::Discard(tok.set(()))),
			TokenKind::Break => Some(cst::Statement::Break(tok.set(()))),
			TokenKind::Continue => Some(cst::Statement::Continue(tok.set(()))),
			TokenKind::Return => {
				let expr = self.parse_expr();
				Some(cst::Statement::Return(tok.set(()), expr))
			}
			TokenKind::For => {
				let steps = self.with_stopper(TokenKind::Do.into(), |parser| {
					let mut steps = cst::Separated {
						elements: Vec::new(),
					};

					for i in 0..3 {
						let statement = parser.with_stopper(
							TokenKind::Semicolon.into(),
							|parser| parser.parse_statement(),
						);

						let statement_span = statement.try_span_of();
						if let Some(statement) = statement {
							steps
								.elements
								.push(cst::SeparatedStep::Element(statement));
						}

						// No semicolon required at the end!
						if i == 2 {
							break;
						}

						let semicolon = parser.with_stopper(
							Self::STATEMENT_MARKER,
							|parser| {
								parser.expect_tolerant(
									TokenKind::Semicolon.into(),
								)
							},
						);

						if let Some(semicolon) = semicolon {
							steps.elements.push(cst::SeparatedStep::Separator(
								semicolon.set(()),
							));
						} else if let Some(span) = statement_span {
							compact_report!(
								(parser, "MissingSemicolon", span),
								("Expected for loop steps to be separated by semicolons"),
								("I was expecting a semicolon after this statement"),
							);
						} else {
							compact_report!(
								(
									parser,
									"MissingForSteps",
									(&tok.span, &steps)
								),
								("For-loop header contains less than three statements"),
								("I was expecting three semicolon-separated statements here"),
							);
							break;
						}
					}

					steps
				});

				let tok_do = self.expect_tolerant(TokenKind::Do.into());
				let mut block = None;

				if tok_do.is_none() {
					compact_report!(
						(self, "MissingKeyword", (&tok, &steps)),
						("Missing 'do' after loop header"),
						("I was expecting a 'do' after this section"),
					);
				} else {
					let result = self.parse_statement_block();
					if result.statements.is_empty() {
						compact_report!(
							(self, "EmptyBlock", (&tok, &steps, &tok_do)),
							("Encountered empty for-loop body"),
							("This for-loop has an empty body"),
						);
					}
					block = Some(result);
				}

				Some(cst::Statement::For(cst::For {
					tok_for: tok.set(()),
					steps,
					tok_do: tok_do.map(|v| v.set(())),
					block,
				}))
			}
			TokenKind::If => {
				let mut branches = Vec::new();
				loop {
					let leading = if branches.is_empty() {
						// first iteration
						tok.clone()
					} else if let Some(tok) =
						self.expect_tolerant(TokenKind::Elif | TokenKind::Else)
					{
						tok
					} else {
						break;
					};

					let is_else = leading.value == TokenKind::Else;

					let (cond, tok_then) = if is_else {
						(None, None)
					} else {
						let cond = self.with_stopper(
							TokenKind::Then | TokenKind::Elif | TokenKind::Else,
							|parser| parser.parse_expr(),
						);
						let tok_then = self.with_stopper(
							TokenKind::Elif | TokenKind::Else,
							|parser| {
								parser.expect_tolerant(TokenKind::Then.into())
							},
						);
						(cond, tok_then)
					};

					if !is_else && cond.is_none() {
						compact_report!(
							(self, "MissingCondition", &leading),
							("Missing condition for branch"),
							("I was expecting a condition after this token"),
						);
					}

					let block = if !is_else && tok_then.is_none() {
						compact_report!(
							(self, "MissingKeyword", (&leading, &cond)),
							("Missing 'then' after conditional branch"),
							("I was expecting a 'then' after this section"),
						);

						None
					} else {
						let result = self.with_stopper(
							TokenKind::Elif | TokenKind::Else,
							|parser| parser.parse_statement_block(),
						);

						if result.statements.is_empty() {
							compact_report!(
								(
									self,
									"EmptyBlock",
									(&leading, &cond, &tok_then)
								),
								("Encountered empty conditional branch body"),
								("This branch has no body"),
							);
						}

						Some(result)
					};

					branches.push(CondBranch {
						tok_leading: leading.set(()),
						condition: cond,
						tok_then: tok_then.map(|t| t.set(())),
						block,
					});

					if is_else {
						break;
					}
				}

				Some(cst::Statement::If(cst::If { branches }))
			}
			_ => unreachable!(),
		}
	}

	pub fn parse_statement_block(&mut self) -> cst::StatementBlock {
		let statements = self
			.parse_block(|parser| parser.parse_statement())
			.into_boxed_slice();
		cst::StatementBlock { statements }
	}
	// }}}
	// {{{ Module parsing
	const MODULE_ENTRY_MARKER: TokenSet = enum_set!(
		TokenKind::Import
			| TokenKind::Module
			| TokenKind::Identifier
			| TokenKind::Property
	);

	fn parse_module_entry(&mut self) -> Option<cst::ModuleEntry> {
		let tok = self.expect_tolerant(Self::MODULE_ENTRY_MARKER)?;

		match tok.value {
			TokenKind::Import => {
				let path = self.parse_qualified_name();
				if path.0.is_empty() {
					compact_report!(
						(self, "MissingPath", &tok),
						("Missing import path"),
						("This import statement is missing a path to import code from"),
					);
				}

				Some(cst::ModuleEntry::Import(cst::ImportStatement {
					import: tok.set(()),
					path: if path.0.is_empty() { None } else { Some(path) },
				}))
			}
			TokenKind::Module => {
				let path = self.with_stopper(
					Self::MODULE_ENTRY_MARKER | TokenKind::Where,
					|parser| parser.parse_qualified_name(),
				);

				if path.0.is_empty() {
					compact_report!(
						(self, "MissingModuleName", &tok),
						("Missing module name"),
						("I was expecting a name for this nested module"),
					);
				}

				let where_ = self
					.with_stopper(Self::MODULE_ENTRY_MARKER, |parser| {
						parser.expect_tolerant(TokenKind::Where.into())
					});

				if !path.0.is_empty() && where_.is_none() {
					compact_report!(
						(self, "MissingWhere", &path),
						("Missing 'where' keyword"),
						("I was expecting the 'where' keyword after this module path"),
					);
				}

				let entries =
					self.parse_block(|parser| parser.parse_module_entry());

				Some(cst::ModuleEntry::Module(cst::NestedModule {
					module: tok.set(()),
					name: if path.0.is_empty() { None } else { Some(path) },
					where_: where_.map(|tok| tok.set(())),
					entries,
				}))
			}
			TokenKind::Identifier | TokenKind::Property => {
				let mut path = self.with_stopper(
					TokenKind::Colon | Self::DECL_VALUE_MARKER,
					|parser| {
						parser
							.continue_parsing_qualified_name(vec![tok.clone()])
					},
				);

				let (first_colon, ty, second_colon) =
					self.with_stopper(Self::DECL_VALUE_MARKER, |parser| {
						let first_colon =
							parser.expect_tolerant(TokenKind::Colon.into());
						let ty = parser
							.with_stopper(TokenKind::Colon.into(), |parser| {
								parser.parse_type()
							});
						let second_colon =
							parser.expect_tolerant(TokenKind::Colon.into());

						if first_colon.is_none() {
							compact_report!(
								(parser, "MisingColon", &path),
								("Declaration names must be followed by colons"),
								("I was expecting a colon here"),
							);
						} else if second_colon.is_none() {
							compact_report!(
								(
									parser,
									"MisingColon",
									(&path, &first_colon, &ty)
								),
								("Declaration values must be preceded by a colon"),
								("I was expecting a second colon here"),
							);
						}

						(first_colon, ty, second_colon)
					});

				let value = self.parse_decl_value();
				if value.is_none() {
					compact_report!(
						(
							self,
							"MissingValue",
							(&path, &first_colon, &ty, &second_colon)
						),
						("Module declarations must contain a concrete value"),
						("I was expecting a value here"),
					);
				}

				match value {
					Some(cst::DeclValue::External(_)) if ty.is_none() => {
						compact_report!(
							(
								self,
								"MissingType",
								(&path, &first_colon, &ty, &second_colon)
							),
							("Encountered untyped declaration"),
							("I was expecting a type annotation for this declaration"),
						)
					}
					Some(
						cst::DeclValue::Type(_)
						| cst::DeclValue::Proc(_)
						| cst::DeclValue::Type(_)
						| cst::DeclValue::Alias(_),
					) if ty.is_some() => {
						compact_report!(
							(
								self,
								"UnexpectedType",
								(&path, &first_colon, &ty, &second_colon)
							),
							("Encountered over-typed declaration"),
							("I was not expecting a type annotation for this declaration"),
						)
					}
					_ => {}
				};

				Some(cst::ModuleEntry::Declaration(cst::Declaration {
					name: path,
					first_colon: first_colon.map(|t| t.set(())),
					ty,
					second_colon: second_colon.map(|t| t.set(())),
					value,
				}))
			}
			_ => unreachable!(),
		}
	}

	fn parse_qualified_name(&mut self) -> cst::QualifiedName {
		self.continue_parsing_qualified_name(Vec::new())
	}

	fn continue_parsing_qualified_name(
		&mut self,
		mut toks: Vec<Token<TokenKind>>,
	) -> cst::QualifiedName {
		while let Some(tok) =
			self.expect_tolerant(TokenKind::Identifier | TokenKind::Property)
		{
			toks.push(tok)
		}

		let mut out = Vec::new();
		for tok in toks {
			let kind = tok.value;

			if out.is_empty() && kind == TokenKind::Property {
				compact_report!(
					(self, "UnexpectedDot", &tok),
					("Qualified path started with dot"),
					("I was expecting this path to start without a dot"),
				);
			} else if !out.is_empty() && kind == TokenKind::Identifier {
				compact_report!(
					(self, "MissingDot", &tok),
					("Qualified path is missing dot"),
					("I was expecting a dot before this path section"),
				);
			}

			let source = self.lexer.source_span(&tok.span);

			out.push(tok.set(match kind {
				TokenKind::Identifier => source.to_string(),
				TokenKind::Property => source[1..].to_string(),
				_ => unreachable!(),
			}));
		}

		cst::QualifiedName(out.into_boxed_slice())
	}

	const NON_TYPE_DECL_VALUE_MARKER: TokenSet = enum_set!(
		TokenKind::Proc
			| TokenKind::Varying
			| TokenKind::Attribute
			| TokenKind::Uniform
			| TokenKind::Buffer
			| TokenKind::Identifier
	);

	const DECL_VALUE_MARKER: TokenSet =
		enum_set!(Self::NON_TYPE_DECL_VALUE_MARKER | Self::TYPE_MARKER);

	fn parse_decl_value(&mut self) -> Option<cst::DeclValue> {
		if let Some(tok) = self.with_stopper(Self::TYPE_MARKER, |parser| {
			parser.expect_tolerant(Self::NON_TYPE_DECL_VALUE_MARKER)
		}) {
			match tok.value {
				TokenKind::Varying => Some(cst::DeclValue::External(
					tok.set(cst::ExternalValue::Varying),
				)),
				TokenKind::Attribute => Some(cst::DeclValue::External(
					tok.set(cst::ExternalValue::Attribute),
				)),
				TokenKind::Buffer => Some(cst::DeclValue::External(
					tok.set(cst::ExternalValue::Buffer),
				)),
				TokenKind::Uniform => {
					if self.expect_tolerant(TokenKind::Buffer.into()).is_some()
					{
						Some(cst::DeclValue::External(
							tok.set(cst::ExternalValue::UniformBuffer),
						))
					} else {
						Some(cst::DeclValue::External(
							tok.set(cst::ExternalValue::Uniform),
						))
					}
				}
				TokenKind::Identifier => {
					let name = self.continue_parsing_qualified_name(vec![tok]);
					Some(cst::DeclValue::Alias(name))
				}
				TokenKind::Proc => {
					let args = if let Some(args_open) =
						self.expect_tolerant(TokenKind::LeftParen.into())
					{
						let mut elements = Vec::new();
						self.with_stopper(TokenKind::RightParen | TokenKind::Arrow, |parser| {
							let stoppers =
								TokenKind::Identifier | TokenKind::Colon | Self::TYPE_MARKER;
							loop {
								if let Some(comma) = parser.with_stopper(stoppers, |parser| {
									parser.expect_tolerant(TokenKind::Comma.into())
								}) {
									elements.push(cst::SeparatedStep::Separator(comma.set(())));
									continue;
								};

								let (name, colon, ty) =
									parser.with_stopper(TokenKind::Comma.into(), |parser| {
										let name = parser.with_stopper(
											TokenKind::Colon | Self::TYPE_MARKER,
											|parser| {
												parser.expect_tolerant(TokenKind::Identifier.into())
											},
										);
										let colon =
											parser.with_stopper(Self::TYPE_MARKER, |parser| {
												parser.expect_tolerant(TokenKind::Colon.into())
											});
										let ty = parser.parse_type();
										(name, colon, ty)
									});

								if name.is_some() || colon.is_some() || ty.is_some() {
									if name.is_none() {
										compact_report!(
											(parser, "MissingName", (&colon, &ty)),
											("Missing argument name"),
											("I was expecting this argument to have a name"),
										);
									} else if colon.is_none() && ty.is_some() {
										compact_report!(
											(parser, "MissingColon", (&name, &ty)),
											("Missing colon"),
											("I was expecting the argument name and its type to be separated by a colon"),
										);
									} else if colon.is_some() && ty.is_none() {
										compact_report!(
											(parser, "MissingType", (&name, &colon)),
											("Missing type"),
											("I was expecting this argument to be followed by a type"),
										);
									}

									elements.push(cst::SeparatedStep::Element(cst::ProcArg {
										name: name.map(|tok| parser.embed_source(tok)),
										colon: colon.map(|tok| tok.set(())),
										ty,
									}));
									continue;
								}

								break;
							}
						});
						let args_close = self.with_stopper(
							TokenKind::Arrow.into(),
							|parser| {
								parser.expect_tolerant(
									TokenKind::RightParen.into(),
								)
							},
						);

						// Detect bad separator orders
						for i in 0..elements.len().saturating_sub(1) {
							let a = &elements[i];
							let b = &elements[i + 1];
							if a.is_separator() && b.is_separator() {
								compact_report!(
									(self, "MissingArgument", (&a, &b)),
									("Missing argument"),
									("I was expecting there to be an argument between these commas"),
								);
							} else if !a.is_separator() && !b.is_separator() {
								compact_report!(
									(self, "MissingComma", (&a, &b)),
									("Missing comma"),
									("I was expecting these two arguments to be separated by a comma"),
								);
							}
						}

						Some(cst::Delimited {
							open: args_open.set(()),
							inner: cst::Separated { elements },
							close: args_close.map(|t| t.set(())),
						})
					} else {
						None
					};

					let ret_arrow = self.with_stopper(
						Self::STATEMENT_MARKER | TokenKind::String,
						|parser| {
							parser.expect_tolerant(TokenKind::Arrow.into())
						},
					);
					let ret_type = if ret_arrow.is_some() {
						let ty = self.with_stopper(
							Self::STATEMENT_MARKER | TokenKind::String,
							|parser| parser.parse_type(),
						);
						if ty.is_none() {
							compact_report!(
								(self, "MissingType", &ret_arrow),
								("Missing return type"),
								("I was expecting this arrow to be followed by the proc's return type"),
							);
						}
						ty
					} else {
						None
					};

					let native_name =
						self.with_stopper(Self::STATEMENT_MARKER, |parser| {
							parser.expect_tolerant(TokenKind::String.into())
						});
					let body = if let Some(name) = native_name {
						cst::ProcBody::Native(self.embed_source(name))
					} else {
						let body = self.parse_statement_block();

						if body.statements.is_empty() {
							compact_report!(
								(
									self,
									"EmptyProcBody",
									(&tok, &args, &ret_arrow, &ret_type)
								),
								("Empty procedure body"),
								("I was expecting at least one statement inside this procedure"),
							);
						}

						cst::ProcBody::Implemented(body)
					};

					Some(cst::DeclValue::Proc(cst::Proc {
						proc: tok.set(()),
						args,
						ret_arrow: ret_arrow.map(|tok| tok.set(())),
						ret_type,
						body,
					}))
				}
				_ => unreachable!(),
			}
		} else {
			let ty = self.parse_type()?;
			Some(cst::DeclValue::Type(ty))
		}
	}

	pub fn parse_file(&mut self) -> cst::File {
		self.relation = IndentationRelation::Eq;
		self.indentation = 1;
		let entries = self.parse_block(|parser| parser.parse_module_entry());
		self.relation = IndentationRelation::Any;
		let eof = self
			.expect_tolerant(TokenKind::Eof.into())
			.expect("Failed to find an end of file token");

		cst::File {
			entries,
			eof: eof.set(()),
		}
	}
	// }}}
}
