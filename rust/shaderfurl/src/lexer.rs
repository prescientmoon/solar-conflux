use std::{cmp, path::Path, rc::Rc};

use ariadne::Span;
use enumset::EnumSetType;

// {{{ Source positions
#[derive(Clone, Copy, Eq)]
pub struct SourcePos {
	pub index: usize,
	pub line: usize,
	pub col: usize,
}

impl std::fmt::Debug for SourcePos {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}:{}", self.line, self.col)
	}
}

impl SourcePos {
	pub fn new(index: usize, line: usize, col: usize) -> Self {
		Self { index, line, col }
	}
}

impl PartialEq for SourcePos {
	fn eq(&self, other: &Self) -> bool {
		self.index == other.index
	}
}

impl PartialOrd for SourcePos {
	fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
		Some(self.cmp(other))
	}
}

impl Ord for SourcePos {
	fn cmp(&self, other: &Self) -> cmp::Ordering {
		self.index.cmp(&other.index)
	}
}
// }}}
// {{{ File IDs
// TODO: change this to whatever the LSP uses
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileId(Rc<Path>);

impl FileId {
	pub fn new(path: Rc<Path>) -> Self {
		Self(path)
	}
}

impl std::fmt::Display for FileId {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:?}", self.0)
	}
}
// }}}
// {{{ Source spans
#[derive(Clone)]
pub struct SourceSpan {
	pub path: FileId,
	pub from: SourcePos,
	pub length: usize,
}

impl std::fmt::Debug for SourceSpan {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}:{:?}-{}", self.path, self.from, self.length)
	}
}

impl SourceSpan {
	pub fn new(path: FileId, from: SourcePos, length: usize) -> Self {
		Self { from, length, path }
	}

	pub fn between(path: FileId, from: SourcePos, to: SourcePos) -> Self {
		Self::new(path, from, to.index - from.index)
	}

	pub fn merge(&self, other: &Self) -> Self {
		assert_eq!(self.path, other.path);
		let start = self.from.min(other.from);
		let end = self.end().max(other.end());
		Self::new(self.path.clone(), start, end - start.index)
	}

	pub fn merge_options(first: Option<Self>, second: Option<Self>) -> Option<Self> {
		match (first, second) {
			(None, None) => None,
			(Some(a), None) => Some(a),
			(None, Some(b)) => Some(b),
			(Some(a), Some(b)) => Some(a.merge(&b)),
		}
	}
}

impl ariadne::Span for SourceSpan {
	type SourceId = FileId;

	fn source(&self) -> &Self::SourceId {
		&self.path
	}

	fn start(&self) -> usize {
		self.from.index
	}

	fn end(&self) -> usize {
		self.start() + self.length
	}
}
// }}}

// {{{ Tokens
#[derive(Debug, EnumSetType)]
pub enum TokenKind {
	// Keywords
	Module,
	Import,
	Proc,
	Return,
	Discard,
	Continue,
	Break,
	Struct,
	Uniform,
	Attribute,
	Varying,
	Buffer,
	If,
	Then,
	Else,
	Elif,
	For,
	Do,

	// Punctuation
	Colon,          // :
	Semicolon,      // ;
	SingleEqual,    // =
	NotEqual,       // !=
	DoubleEqual,    // ==
	Comma,          // ,
	Arrow,          // ->
	LeftCurly,      // {
	RightCurly,     // }
	LeftBracket,    // [
	RightBracket,   // ]
	LeftParen,      // (
	RightParen,     // )
	Plus,           // +
	Minus,          // -
	Multiply,       // *
	Divide,         // /
	And,            // &
	Or,             // |
	Xor,            // ^
	Not,            // !
	BitwiseNot,     // ~
	Meet,           // /\
	Join,           // \/
	LeftShift,      // <<
	RightShift,     // >>
	GreaterThan,    // >
	LesserThan,     // <
	GreaterOrEqual, // >=
	LesserOrEqual,  // <=
	QuestionMark,   // ?
	PlusEqual,      // +=
	MinusEqual,     // -=
	MultiplyEqual,  // *=
	DivideEqual,    // /=

	// Other
	Integer,    // 0
	Float,      // 0.0
	Identifier, // foo
	Property,   // .foo
	String,     // "foo"
	Comment,    // // foo
	Junk,
	Eof,
	Sof, // Start of file:
}

#[derive(Clone)]
pub struct Token {
	pub kind: TokenKind,
	pub span: SourceSpan,
}

impl std::fmt::Debug for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:?}({:?})", self.kind, self.span)
	}
}
// }}}
// {{{ Lexing
// NOTE: cloning this is very cheap
#[derive(Debug, Clone)]
pub struct Lexer<'a> {
	path: FileId,
	source: &'a str,

	pos: SourcePos,
	curr: char,
	next_index: usize,
}

impl<'a> Lexer<'a> {
	pub fn new(path: FileId, source: &'a str) -> Self {
		let mut lexer = Lexer {
			path,
			source,
			pos: SourcePos {
				index: 0,
				line: 1,
				col: 0, // This will get bumped by the following .advance()
			},
			curr: '\0', // Will be set by initial advance
			next_index: 0,
		};

		lexer.advance(); // Initialize curr and pos
		lexer
	}

	fn advance(&mut self) {
		if self.next_index < self.source.len() {
			self.pos.index = self.next_index;
			if self.curr == '\n' {
				self.pos.col = 1;
				self.pos.line += 1;
			} else {
				self.pos.col += 1;
			}

			let ch = self.source[self.next_index..].chars().next().unwrap();
			self.curr = ch;
			self.next_index += ch.len_utf8();
		} else {
			self.pos.index = self.source.len();

			if self.curr == '\n' {
				self.pos.line += 1;
				self.pos.col = 1;
			}

			self.next_index = usize::MAX;
			self.curr = '\0';
		}
	}

	fn peek_char(&self) -> char {
		let mut copy = self.clone();
		copy.advance();
		copy.curr
	}

	#[allow(unused)]
	pub fn next(&mut self) -> Token {
		while matches!(self.curr, ' ' | '\r' | '\t' | '\n') {
			self.advance();
		}

		let mut tok = Token {
			kind: TokenKind::Eof,
			span: SourceSpan::new(self.path.clone(), self.pos, 0),
		};

		let mut end_offset = 0;
		let nch = self.peek_char();

		match self.curr {
			'\0' => tok.kind = TokenKind::Eof,
			// {{{ Identifiers / keywords
			ch if ch.is_alphabetic() || ch == '_' => {
				while self.curr.is_alphanumeric() || self.curr == '_' {
					self.advance();
				}

				let lit = &self.source[tok.span.from.index..self.pos.index];
				tok.kind = TokenKind::Identifier;

				match lit {
					"module" => tok.kind = TokenKind::Module,
					"import" => tok.kind = TokenKind::Import,
					"proc" => tok.kind = TokenKind::Proc,
					"return" => tok.kind = TokenKind::Return,
					"discard" => tok.kind = TokenKind::Discard,
					"continue" => tok.kind = TokenKind::Continue,
					"break" => tok.kind = TokenKind::Break,
					"struct" => tok.kind = TokenKind::Struct,
					"uniform" => tok.kind = TokenKind::Uniform,
					"attribute" => tok.kind = TokenKind::Attribute,
					"varying" => tok.kind = TokenKind::Varying,
					"buffer" => tok.kind = TokenKind::Buffer,
					"if" => tok.kind = TokenKind::If,
					"then" => tok.kind = TokenKind::Then,
					"else" => tok.kind = TokenKind::Else,
					"elif" => tok.kind = TokenKind::Elif,
					"for" => tok.kind = TokenKind::For,
					"do" => tok.kind = TokenKind::Do,
					_ => {}
				}
			}
			// }}}
			// {{{ Properties
			'.' if nch.is_alphabetic() => {
				self.advance();
				while self.curr.is_alphanumeric() {
					self.advance();
				}

				tok.kind = TokenKind::Property;
			}
			// }}}
			// {{{ Numbers
			ch if ch.is_ascii_digit() => {
				while self.curr.is_ascii_digit() {
					self.advance();
				}

				// Handle floats
				if self.curr == '.' {
					self.advance();
					while self.curr.is_ascii_digit() {
						self.advance();
					}
					tok.kind = TokenKind::Float;
				} else {
					tok.kind = TokenKind::Integer;
				}
			}
			'.' if nch.is_ascii_digit() => {
				self.advance();
				while self.curr.is_ascii_digit() {
					self.advance();
				}

				tok.kind = TokenKind::Float;
			}
			// }}}
			// {{{ Comments
			'/' if nch == '/' => {
				self.advance();
				self.advance();

				while self.curr != '\n' && self.curr != '\0' {
					self.advance();
				}

				// Strip CR from line comments
				while self.source[self.pos.index - end_offset - 1..].starts_with('\r') {
					end_offset += 1
				}

				tok.kind = TokenKind::Comment
			}
			// }}}
			// {{{ Strings
			'"' => {
				self.advance();

				while self.curr != '"' && self.curr != '\0' {
					self.advance();
				}

				// Skip the other quote
				self.advance();

				tok.kind = TokenKind::String
			}
			// }}}
			// {{{ Punctuation
			ch => {
				self.advance();
				use TokenKind::*;
				match (ch, nch) {
					(',', _) => tok.kind = Comma,
					(':', _) => tok.kind = Colon,
					(';', _) => tok.kind = Semicolon,
					('(', _) => tok.kind = LeftParen,
					(')', _) => tok.kind = RightParen,
					('[', _) => tok.kind = LeftBracket,
					(']', _) => tok.kind = RightBracket,
					('{', _) => tok.kind = LeftCurly,
					('}', _) => tok.kind = RightCurly,
					('?', _) => tok.kind = QuestionMark,
					('^', _) => tok.kind = Xor,
					('~', _) => tok.kind = BitwiseNot,
					('|', _) => tok.kind = Or,
					('&', _) => tok.kind = And,
					#[rustfmt::skip]
					('/', '\\') => { tok.kind = Meet; self.advance(); }
					#[rustfmt::skip]
					('\\', '/') => { tok.kind = Join; self.advance(); }
					#[rustfmt::skip]
					('<', '<')  => { tok.kind = LeftShift; self.advance(); }
					#[rustfmt::skip]
					('>', '>')  => { tok.kind = RightShift; self.advance(); }
					#[rustfmt::skip]
					('>', '=')  => { tok.kind = GreaterOrEqual; self.advance(); }
					#[rustfmt::skip]
					('<', '=')  => { tok.kind = LesserOrEqual; self.advance(); }
					#[rustfmt::skip]
					('=', '=') => { tok.kind = DoubleEqual; self.advance(); }
					#[rustfmt::skip]
					('!', '=')  => { tok.kind = NotEqual; self.advance(); }
					#[rustfmt::skip]
					('-', '>')  => { tok.kind = Arrow; self.advance(); }
					#[rustfmt::skip]
					('+', '=') => { tok.kind = PlusEqual; self.advance(); }
					#[rustfmt::skip]
					('-', '=') => { tok.kind = MinusEqual; self.advance(); }
					#[rustfmt::skip]
					('*', '=') => { tok.kind = MultiplyEqual; self.advance(); }
					#[rustfmt::skip]
					('/', '=') => { tok.kind = DivideEqual; self.advance(); }

					('>', _) => tok.kind = GreaterThan,
					('<', _) => tok.kind = LesserThan,
					('=', _) => tok.kind = SingleEqual,
					('-', _) => tok.kind = Minus,
					('/', _) => tok.kind = Divide,
					('!', _) => tok.kind = Not,
					('+', _) => tok.kind = Plus,
					('*', _) => tok.kind = Multiply,

					// Junk! (eat it all 😋)
					_ => {
						while !matches!(self.curr, ' ' | '\r' | '\t' | '\n' | '\0') {
							self.advance();
						}

						tok.kind = Junk;
					}
				}
			} // }}}
		}

		tok.span.length = self.pos.index - tok.span.from.index - end_offset;
		tok
	}

	// Returns the piece of text a token spans
	pub fn source_span(&self, span: &SourceSpan) -> &str {
		&self.source[span.start()..span.end()]
	}
}
// }}}
