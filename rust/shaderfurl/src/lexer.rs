#[derive(Debug, Clone, Copy)]
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
	Matrix,
	If,
	Then,
	Else,
	For,
	Do,

	// Punctuation
	Colon,          // :
	Semicolon,      // ;
	SingleEqual,    // =
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

	// Other
	Integer,    // 0
	Float,      // 0.0
	Identifier, // foo
	Property,   // .foo
	String,     // "foo"
	Comment,    // // foo
	Junk,
	Eof,
}

#[derive(Debug, Clone, Copy)]
pub struct SourcePos {
	pub index: usize,
	pub line: usize,
	pub col: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct SourceSpan {
	pub from: SourcePos,
	pub length: usize,
}

impl SourceSpan {
	pub fn new(from: SourcePos, length: usize) -> Self {
		Self { from, length }
	}
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
	pub kind: TokenKind,
	pub span: SourceSpan,
}

#[derive(Debug, Clone, Copy)]
pub struct LexerState {
	pos: SourcePos,
	curr: char,
	next_index: usize,
}

#[derive(Debug, Clone)]
pub struct Lexer {
	pub source: String,
	state: LexerState,
}

impl Lexer {
	#[allow(unused)]
	pub fn new(source: String) -> Self {
		let mut lexer = Lexer {
			source,
			state: LexerState {
				pos: SourcePos {
					index: 0,
					line: 1,
					col: 0, // This will get bumped by the following .advance()
				},
				curr: '\0', // Will be set by initial advance
				next_index: 0,
			},
		};

		lexer.advance(); // Initialize curr and pos
		lexer
	}

	fn advance(&mut self) {
		if self.state.next_index < self.source.len() {
			self.state.pos.index = self.state.next_index;
			if self.state.curr == '\n' {
				self.state.pos.col = 1;
				self.state.pos.line += 1;
			} else {
				self.state.pos.col += 1;
			}

			let ch = self.source[self.state.next_index..].chars().next().unwrap();
			self.state.curr = ch;
			self.state.next_index += ch.len_utf8();
		} else {
			self.state.pos.index = self.source.len();

			if self.state.curr == '\n' {
				self.state.pos.line += 1;
				self.state.pos.col = 1;
			}

			self.state.next_index = usize::MAX;
			self.state.curr = '\0';
		}
	}

	fn peek(&mut self) -> LexerState {
		let initial_state = self.state;
		self.advance();
		let state = self.state;
		self.state = initial_state;
		state
	}

	#[allow(unused)]
	pub fn pull(&mut self) -> Token {
		while matches!(self.state.curr, ' ' | '\r' | '\t' | '\n') {
			self.advance();
		}

		let mut tok = Token {
			kind: TokenKind::Eof,
			span: SourceSpan::new(self.state.pos, 0),
		};

		let mut end_offset = 0;
		let nch = self.peek().curr;

		match self.state.curr {
			'\0' => tok.kind = TokenKind::Eof,
			// {{{ Identifiers / keywords
			ch if ch.is_alphabetic() => {
				while self.state.curr.is_alphanumeric() {
					self.advance();
				}

				let lit = &self.source[tok.span.from.index..self.state.pos.index];
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
					"matrix" => tok.kind = TokenKind::Matrix,
					"if" => tok.kind = TokenKind::If,
					"then" => tok.kind = TokenKind::Then,
					"else" => tok.kind = TokenKind::Else,
					"for" => tok.kind = TokenKind::For,
					"do" => tok.kind = TokenKind::Do,
					_ => {}
				}
			}
			// }}}
			// {{{ Properties
			'.' if nch.is_alphabetic() => {
				self.advance();
				while self.state.curr.is_alphanumeric() {
					self.advance();
				}

				tok.kind = TokenKind::Property;
			}
			// }}}
			// {{{ Numbers
			ch if ch.is_ascii_digit() => {
				while self.state.curr.is_ascii_digit() {
					self.advance();
				}

				// Handle floats
				if self.state.curr == '.' {
					self.advance();
					while self.state.curr.is_ascii_digit() {
						self.advance();
					}
					tok.kind = TokenKind::Float;
				} else {
					tok.kind = TokenKind::Integer;
				}
			}
			'.' if nch.is_ascii_digit() => {
				self.advance();
				while self.state.curr.is_ascii_digit() {
					self.advance();
				}

				tok.kind = TokenKind::Float;
			}
			// }}}
			// {{{ Comments
			'/' if nch == '/' => {
				self.advance();
				self.advance();

				while self.state.curr != '\n' && self.state.curr != '\0' {
					self.advance();
				}

				// Strip CR from line comments
				while self.source[self.state.pos.index - end_offset - 1..].starts_with('\r') {
					end_offset += 1
				}

				tok.kind = TokenKind::Comment
			}
			// }}}
			// {{{ Strings
			'"' => {
				self.advance();

				while self.state.curr != '"' && self.state.curr != '\0' {
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
					('+', _) => tok.kind = Plus,
					('*', _) => tok.kind = Multiply,
					('?', _) => tok.kind = QuestionMark,
					('^', _) => tok.kind = Xor,
					('!', _) => tok.kind = Not,
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
					('=', '=')  => { tok.kind = DoubleEqual; self.advance(); }
					#[rustfmt::skip]
					('-', '>')  => { tok.kind = Arrow; self.advance(); }

					('>', _) => tok.kind = GreaterThan,
					('<', _) => tok.kind = LesserThan,
					('=', _) => tok.kind = SingleEqual,
					('-', _) => tok.kind = Minus,
					('/', _) => tok.kind = Divide,

					// Junk! (eat it all 😋)
					_ => {
						while !matches!(self.state.curr, ' ' | '\r' | '\t' | '\n' | '\0') {
							self.advance();
						}

						tok.kind = Junk;
					}
				}
			} // }}}
		}

		tok.span.length = self.state.pos.index - tok.span.from.index - end_offset;
		tok
	}
}
