#![allow(unused)]
#![allow(clippy::large_enum_variant)]
use crate::lexer::{self, SourcePos, SourceSpan};

// {{{ Building blocks
#[derive(Debug, Clone)]
pub struct Token<T = ()> {
	pub span: SourceSpan,
	pub value: T,

	// This is inefficient af, although it'll only allocate if trivia
	// is actually attached, so it's not the end of the world...
	pub trivia: Vec<lexer::Token>,
}

#[derive(Debug, Clone)]
pub struct Delimited<T> {
	pub open: Token,
	pub inner: T,
	pub close: Option<Token>,
}

#[derive(Debug, Clone)]
pub struct Separated<T> {
	pub elements: Vec<SeparatedStep<T>>,
}

#[derive(Debug, Clone)]
pub enum SeparatedStep<T> {
	Separator(Token),
	Element(T),
}

impl<T> SeparatedStep<T> {
	pub fn is_separator(&self) -> bool {
		match self {
			Self::Separator(_) => true,
			Self::Element(_) => false,
		}
	}
}
// }}}
// {{{ Modules
#[derive(Debug, Clone)]
pub struct File {
	entries: Vec<ModuleEntry>,

	// You might be wondering — why am I saving the EOF token out of all things?
	//
	// Well, it's because I currently always attach trivia to the following node,
	// so the EOF token will contain all the trivia saved after the end of the last
	// successfully parsed expression.
	eof: Token,
}

#[derive(Debug, Clone)]
pub enum ModuleEntry {
	Import(ImportStatement),
	Module(NestedModule),
	Declaration(Declaration),
}

#[derive(Debug, Clone)]
pub struct ImportStatement {
	import: Token,
	path: Option<Token<String>>,
}

#[derive(Debug, Clone)]
pub struct NestedModule {
	module: Token,
	name: Option<Token<String>>,
	entries: Vec<ModuleEntry>,
}

#[derive(Debug, Clone)]
pub struct Declaration {
	name: Option<Token<String>>,
	first_colon: Option<Token>,
	ty: Option<Type>,
	second_colon: Option<Token>,
	value: Option<DeclValue>,
}

#[derive(Debug, Clone)]
pub enum DeclValue {
	Proc(Proc),
	Type(Type),
	Alias(Token<String>), // We don't yet know what this is aliasing to!
	Varying,
	Attribute,
	Uniform,
	UniformBuffer, // UBO
	Buffer,        // SSBO
}
// }}}
// {{{ Procs
#[derive(Debug, Clone)]
pub struct Proc {
	proc: Token,
	args: Option<Delimited<Separated<ProcArg>>>,
	ret: Option<(Token, Type)>,
	body: ProcBody,
}

#[derive(Debug, Clone)]
pub struct ProcArg {
	// At least one of these needs to be present!
	name: Option<Token<String>>,
	colon: Option<Token>,
	ty: Option<Type>,
}

#[derive(Debug, Clone)]
pub enum ProcBody {
	Native(Option<Token<String>>),
	Implemented(Token, Option<ExprBlock>), // do ...
}
// }}}
// {{{ Statements
#[derive(Debug, Clone)]
pub struct ExprBlock {
	statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
	Expression(Expr),
	Assignment(Assignment),
	Declaration(LocalDeclaration),
	If(If),
	For(For),
	Discard,
	Break,
	Continue,
	Return,
}

#[derive(Debug, Clone)]
pub struct Assignment {
	variable: Option<Expr>,
	equals: Token,
	value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct LocalDeclaration {
	variable: Option<String>,
	colon: Token,
	ty: Option<Type>,
	equals: Token,
	value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct If {
	tok_if: Token,
	condition: Option<Expr>,
	tok_then: Option<Token>,
	block: Option<ExprBlock>,
	else_if_branches: Vec<ElseIf>,
	else_branch: Option<Else>,
}

#[derive(Debug, Clone)]
pub struct ElseIf {
	tok_else: Token,
	tok_if: Token,
	condition: Option<Expr>,
	tok_then: Option<Token>,
	block: Option<ExprBlock>,
}

#[derive(Debug, Clone)]
pub struct Else {
	tok_else: Token,
	block: Option<ExprBlock>,
}

#[derive(Debug, Clone)]
pub struct For {
	tok_for: Token,
	steps: Separated<Statement>,
	tok_do: Option<Token>,
	block: Option<ExprBlock>,
}
// }}}
// {{{ Expressions
#[derive(Debug, Clone)]
pub enum Expr {
	Int(Token<i64>),
	Float(Token<f64>),
	Variable(Token<String>),
	Property(Option<Box<Expr>>, Token<String>),
	Call(Call),
	Unary(UnaryOperator, Option<Box<Expr>>),
	Binary(Option<Box<Expr>>, BinaryOperator, Option<Box<Expr>>),
	Ternary(
		Option<Box<Expr>>, // condition
		Option<Token>,     // ?
		Option<Box<Expr>>, // if_true
		Option<Token>,     // :
		Option<Box<Expr>>, // if_false
	), // condition ? if_true : if_false
	Wrapped(Delimited<Option<Box<Expr>>>), // ( expression )
}

#[derive(Debug, Clone)]
pub struct Call {
	pub callee: Box<Expr>,
	pub arguments: Delimited<Separated<Expr>>,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
	Plus,       // +
	Minus,      // -
	Not,        // !
	BitwiseNot, // ~
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
	DoubleEqual,    // ==
	Plus,           // +
	Minus,          // -
	Multiply,       // *
	Divide,         // /
	And,            // &
	Or,             // |
	Xor,            // ^
	Meet,           // /\
	Join,           // \/
	LeftShift,      // <<
	RightShift,     // >>
	GreaterThan,    // >
	LesserThan,     // <
	GreaterOrEqual, // >=
	LesserOrEqual,  // <=
}
// }}}
// {{{ Types
#[derive(Debug, Clone)]
pub enum Type {
	// TODO: perhaps allow qualified names here?
	Named(Token<String>),
	Struct(Struct),
	Array(Array),
}

#[derive(Debug, Clone)]
pub struct Struct {
	fields: Vec<StructField>,
}

#[derive(Debug, Clone)]
pub struct StructField {
	name: Option<Token<String>>,
	colon: Option<Token>,
	ty: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct Array {
	dimensions: Delimited<Option<ArrayDimensions>>,
	ty: Option<Box<Type>>,
}

#[derive(Debug, Clone)]
pub struct ArrayDimensions {
	first: Token<usize>,
	comma: Option<Token>,
	second: Option<Token<usize>>,
}
// }}}

// {{{ The HasTrivia trait
pub trait HasTrivia {
	fn try_span_of(&self) -> Option<SourceSpan>;
	fn span_of(&self) -> SourceSpan {
		self.try_span_of().unwrap()
	}

	/// Attempts to attach a token as trivia. Returns the token on failure.
	fn try_attach_trivia(&mut self, trivia: lexer::Token) -> Option<lexer::Token>;
}

impl<T: HasTrivia> HasTrivia for Option<T> {
	fn try_span_of(&self) -> Option<SourceSpan> {
		self.as_ref()?.try_span_of()
	}

	fn try_attach_trivia(&mut self, trivia: lexer::Token) -> Option<lexer::Token> {
		if let Some(s) = self.as_mut() {
			s.try_attach_trivia(trivia)
		} else {
			Some(trivia)
		}
	}
}

impl<T: HasTrivia> HasTrivia for Box<T> {
	fn try_span_of(&self) -> Option<SourceSpan> {
		self.as_ref().try_span_of()
	}

	fn try_attach_trivia(&mut self, trivia: lexer::Token) -> Option<lexer::Token> {
		self.as_mut().try_attach_trivia(trivia)
	}
}

impl<T: HasTrivia> HasTrivia for Vec<T> {
	fn try_span_of(&self) -> Option<SourceSpan> {
		let mut result = None;

		for s in self {
			result = SourceSpan::merge_options(result, s.try_span_of());
		}

		result
	}

	fn try_attach_trivia(&mut self, mut trivia: lexer::Token) -> Option<lexer::Token> {
		for element in self {
			if let Some(still_there) = element.try_attach_trivia(trivia) {
				trivia = still_there;
			} else {
				return None;
			}
		}

		Some(trivia)
	}
}

/// NOTE: this implementation doesn't look at the inner value, hence why the
/// inner type is not required to implement [HasTrivia] as well.
impl<T> HasTrivia for Token<T> {
	fn try_span_of(&self) -> Option<SourceSpan> {
		Some(self.span.clone())
	}

	fn try_attach_trivia(&mut self, trivia: lexer::Token) -> Option<lexer::Token> {
		self.trivia.push(trivia);
		None
	}
}

impl HasTrivia for lexer::Token {
	fn try_span_of(&self) -> Option<SourceSpan> {
		Some(self.span.clone())
	}

	fn try_attach_trivia(&mut self, trivia: lexer::Token) -> Option<lexer::Token> {
		Some(trivia)
	}
}

impl<T: HasTrivia> HasTrivia for SeparatedStep<T> {
	fn try_span_of(&self) -> Option<SourceSpan> {
		match self {
			Self::Separator(v) => v.try_span_of(),
			Self::Element(v) => v.try_span_of(),
		}
	}

	fn try_attach_trivia(&mut self, trivia: lexer::Token) -> Option<lexer::Token> {
		match self {
			Self::Separator(v) => v.try_attach_trivia(trivia),
			Self::Element(v) => v.try_attach_trivia(trivia),
		}
	}
}

impl<T: HasTrivia> HasTrivia for Separated<T> {
	fn try_span_of(&self) -> Option<SourceSpan> {
		self.elements.try_span_of()
	}

	fn try_attach_trivia(&mut self, mut trivia: lexer::Token) -> Option<lexer::Token> {
		self.elements.try_attach_trivia(trivia)
	}
}

impl<T: HasTrivia> HasTrivia for Delimited<T> {
	fn try_span_of(&self) -> Option<SourceSpan> {
		SourceSpan::merge_options(
			self.open.try_span_of(),
			SourceSpan::merge_options(self.inner.try_span_of(), self.close.try_span_of()),
		)
	}

	fn try_attach_trivia(&mut self, mut trivia: lexer::Token) -> Option<lexer::Token> {
		let trivia = self.open.try_attach_trivia(trivia)?;
		let trivia = self.inner.try_attach_trivia(trivia)?;
		self.close.try_attach_trivia(trivia)
	}
}

impl HasTrivia for Expr {
	fn try_span_of(&self) -> Option<SourceSpan> {
		match self {
			Expr::Int(token) => token.try_span_of(),
			Expr::Float(token) => token.try_span_of(),
			Expr::Variable(token) => token.try_span_of(),
			Expr::Property(expr, token) => {
				SourceSpan::merge_options(token.try_span_of(), token.try_span_of())
			}
			Expr::Call(call) => call.try_span_of(),
			Expr::Unary(unary_operator, expr) => todo!(),
			Expr::Binary(expr, binary_operator, expr1) => todo!(),
			Expr::Ternary(expr, token, expr1, token1, expr2) => todo!(),
			Expr::Wrapped(delimited) => todo!(),
		}
	}

	fn try_attach_trivia(&mut self, trivia: lexer::Token) -> Option<lexer::Token> {
		match self {
			Expr::Int(token) => token.try_attach_trivia(trivia),
			Expr::Float(token) => token.try_attach_trivia(trivia),
			Expr::Variable(token) => token.try_attach_trivia(trivia),
			Expr::Property(expr, token) => {
				let trivia = expr.try_attach_trivia(trivia)?;
				token.try_attach_trivia(trivia)
			}
			Expr::Call(call) => call.try_attach_trivia(trivia),
			Expr::Unary(unary_operator, expr) => todo!(),
			Expr::Binary(expr, binary_operator, expr1) => todo!(),
			Expr::Ternary(expr, token, expr1, token1, expr2) => todo!(),
			Expr::Wrapped(delimited) => todo!(),
		}
	}
}

impl HasTrivia for Call {
	fn try_span_of(&self) -> Option<SourceSpan> {
		SourceSpan::merge_options(self.callee.try_span_of(), self.arguments.try_span_of())
	}

	fn try_attach_trivia(&mut self, trivia: lexer::Token) -> Option<lexer::Token> {
		let trivia = self.callee.try_attach_trivia(trivia)?;
		self.arguments.try_attach_trivia(trivia)
	}
}
// }}}
