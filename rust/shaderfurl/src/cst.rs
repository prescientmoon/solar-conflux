#![allow(clippy::large_enum_variant)]
#![allow(dead_code)]
use crate::lexer::{self, SourceSpan, TokenKind};

// {{{ Building blocks
#[derive(Debug, Clone)]
pub struct Token<T = ()> {
	// TODO(2025-10-22): Document whether this includes the trivia's span
	pub span: SourceSpan,
	/// A range of token indices for the trivia associated with the token.
	pub trivia: (usize, usize),
	pub value: T,
}

impl<T> Token<T> {
	pub fn set<O>(self, other: O) -> Token<O> {
		Token {
			span: self.span,
			trivia: self.trivia,
			value: other,
		}
	}
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

impl<T> Separated<T> {
	pub fn iter(&self) -> impl Iterator<Item = &T> {
		self.elements.iter().filter_map(|s| s.as_element())
	}
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

	pub fn as_element(&self) -> Option<&T> {
		match self {
			Self::Separator(_) => None,
			Self::Element(e) => Some(e),
		}
	}
}

#[derive(Debug, Clone, Default)]
pub struct QualifiedName(pub Box<[Token<String>]>);
// }}}
// {{{ Modules
#[derive(Debug, Clone)]
pub struct File {
	pub entries: Vec<ModuleEntry>,

	// You might be wondering — why am I saving the EOF token out of all things?
	//
	// Well, it's because I currently always attach trivia to the following node,
	// so the EOF token will contain all the trivia saved after the end of the last
	// successfully parsed expression.
	pub eof: Token,
}

#[derive(Debug, Clone)]
pub enum ModuleEntry {
	Import(ImportStatement),
	Module(NestedModule),
	Declaration(Declaration),
}

#[derive(Debug, Clone)]
pub struct ImportStatement {
	pub import: Token,
	pub path: Option<QualifiedName>,
}

#[derive(Debug, Clone)]
pub struct NestedModule {
	pub module: Token,
	pub name: Option<QualifiedName>,
	pub where_: Option<Token>,
	pub entries: Vec<ModuleEntry>,
}

#[derive(Debug, Clone)]
pub struct Declaration {
	pub name: QualifiedName,
	pub first_colon: Option<Token>,
	pub ty: Option<Type>,
	pub second_colon: Option<Token>,
	pub value: Option<DeclValue>,
}

#[derive(Debug, Clone, Copy)]
pub enum ExternalValue {
	Uniform,
	Attribute,
	Varying,
	UniformBuffer, // UBO
	Buffer,        // SSBO
}

#[derive(Debug, Clone)]
pub enum DeclValue {
	Proc(Proc),
	Type(Type),
	Alias(QualifiedName), // We don't yet know what this is aliasing to!
	External(Token<ExternalValue>),
}
// }}}
// {{{ Procs
#[derive(Debug, Clone)]
pub struct Proc {
	pub proc: Token,
	pub args: Option<Delimited<Separated<ProcArg>>>,
	pub ret_arrow: Option<Token>,
	pub ret_type: Option<Type>,
	pub body: ProcBody,
}

#[derive(Debug, Clone)]
pub struct ProcArg {
	// At least one of these needs to be present!
	pub name: Option<Token<String>>,
	pub colon: Option<Token>,
	pub ty: Option<Type>,
}

#[derive(Debug, Clone)]
pub enum ProcBody {
	Native(Token<String>),
	Implemented(StatementBlock),
}
// }}}
// {{{ Statements
#[derive(Debug, Clone, Default)]
pub struct StatementBlock {
	pub statements: Box<[Statement]>,
}

#[derive(Debug, Clone)]
pub enum Statement {
	Expression(Expr),
	Assignment(Option<Expr>, Token<Option<BinaryOperator>>, Option<Expr>),
	Declaration(LocalDeclaration),
	If(If),
	For(For),
	Discard(Token),
	Break(Token),
	Continue(Token),
	Return(Token, Option<Expr>),
}

#[derive(Debug, Clone)]
pub struct LocalDeclaration {
	// This is very loose, might change later. It's currently set up
	// this way so the parser doesn't have to ever backtrack.
	pub variable: Option<Expr>,
	pub colon: Token,
	pub ty: Option<Type>,
	pub equals: Option<Token>,
	pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct If {
	pub branches: Vec<CondBranch>,
}

#[derive(Debug, Clone)]
pub struct CondBranch {
	pub tok_leading: Token,      // if/elif/else
	pub condition: Option<Expr>, // absent for `else` branches
	pub tok_then: Option<Token>, // absent for `else` branches
	pub block: Option<StatementBlock>,
}

#[derive(Debug, Clone)]
pub struct For {
	pub tok_for: Token,
	pub steps: Separated<Statement>,
	pub tok_do: Option<Token>,
	pub block: Option<StatementBlock>,
}
// }}}
// {{{ Expressions
#[derive(Clone, Debug)]
pub enum Expr {
	Int(Token<i64>),
	Float(Token<f64>),
	Variable(Token<String>),
	Property(Option<Box<Expr>>, Token<String>),
	Call(Box<Expr>, Box<[Expr]>),
	Unary(Token<UnaryOperator>, Option<Box<Expr>>),
	Binary(Option<Box<Expr>>, Token<BinaryOperator>, Option<Box<Expr>>),
	Ternary(
		// At least one argument must be present!
		Option<Box<Expr>>, // condition
		Option<Token>,     // ?
		Option<Box<Expr>>, // if_true
		Option<Token>,     // :
		Option<Box<Expr>>, // if_false
	), // condition ? if_true : if_false
	Wrapped(Delimited<Option<Box<Expr>>>), // ( expression )
	Error(Token),
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
	Plus,       // +
	Minus,      // -
	Not,        // !
	BitwiseNot, // ~
}

impl UnaryOperator {
	pub fn from_token_kind(kind: TokenKind) -> Option<Self> {
		match kind {
			TokenKind::Plus => Some(Self::Plus),
			TokenKind::Minus => Some(Self::Minus),
			TokenKind::Not => Some(Self::Not),
			TokenKind::BitwiseNot => Some(Self::BitwiseNot),
			_ => None,
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
	DoubleEqual,    // ==
	NotEqual,       // !=
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

impl BinaryOperator {
	pub fn from_token_kind(kind: TokenKind) -> Option<Self> {
		match kind {
			TokenKind::DoubleEqual => Some(Self::DoubleEqual),
			TokenKind::NotEqual => Some(Self::NotEqual),
			TokenKind::Plus => Some(Self::Plus),
			TokenKind::Minus => Some(Self::Minus),
			TokenKind::Multiply => Some(Self::Multiply),
			TokenKind::Divide => Some(Self::Divide),
			TokenKind::And => Some(Self::And),
			TokenKind::Or => Some(Self::Or),
			TokenKind::Xor => Some(Self::Xor),
			TokenKind::Meet => Some(Self::Meet),
			TokenKind::Join => Some(Self::Join),
			TokenKind::LeftShift => Some(Self::LeftShift),
			TokenKind::RightShift => Some(Self::RightShift),
			TokenKind::GreaterThan => Some(Self::GreaterThan),
			TokenKind::LesserThan => Some(Self::LesserThan),
			TokenKind::GreaterOrEqual => Some(Self::GreaterOrEqual),
			TokenKind::LesserOrEqual => Some(Self::LesserOrEqual),
			_ => None,
		}
	}
}

// }}}
// {{{ Types
#[derive(Debug, Clone)]
pub enum Type {
	Named(Token<String>),
	Struct(Struct),
	Array(Array),
}

#[derive(Debug, Clone)]
pub struct Struct {
	pub fields: Vec<StructField>,
}

#[derive(Debug, Clone)]
pub struct StructField {
	pub name: Option<Token<String>>,
	pub colon: Option<Token>,
	pub ty: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct Array {
	pub dimensions: Delimited<Option<ArrayDimensions>>,
	pub ty: Option<Box<Type>>,
}

#[derive(Debug, Clone)]
pub struct ArrayDimensions {
	pub first: Token<usize>,
	pub comma: Option<Token>,
	pub second: Option<Token<usize>>,
}
// }}}
// {{{ Pretty printing
impl std::fmt::Display for UnaryOperator {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Plus => write!(f, "+"),
			Self::Minus => write!(f, "-"),
			Self::Not => write!(f, "!"),
			Self::BitwiseNot => write!(f, "~"),
		}
	}
}

impl std::fmt::Display for BinaryOperator {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::DoubleEqual => write!(f, "=="),
			Self::NotEqual => write!(f, "!="),
			Self::Plus => write!(f, "+"),
			Self::Minus => write!(f, "-"),
			Self::Multiply => write!(f, "*"),
			Self::Divide => write!(f, "/"),
			Self::And => write!(f, "&"),
			Self::Or => write!(f, "|"),
			Self::Xor => write!(f, "^"),
			Self::Meet => write!(f, "/\\"),
			Self::Join => write!(f, "\\/"),
			Self::LeftShift => write!(f, "<<"),
			Self::RightShift => write!(f, ">>"),
			Self::GreaterThan => write!(f, ">"),
			Self::LesserThan => write!(f, "<"),
			Self::GreaterOrEqual => write!(f, ">="),
			Self::LesserOrEqual => write!(f, "<="),
		}
	}
}

impl std::fmt::Display for Expr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Error(_) => write!(f, "<error>"),
			Self::Int(tok) => write!(f, "{}", tok.value),
			Self::Float(tok) => write!(f, "{}", tok.value),
			Self::Variable(tok) => write!(f, "{}", tok.value),
			Self::Property(None, tok) => write!(f, "<expr>.{}", tok.value),
			Self::Property(Some(e), tok) => write!(f, "{e}.{}", tok.value),
			Self::Call(callee, args) => {
				write!(f, "{callee}")?;
				for arg in args {
					write!(f, " {arg}")?;
				}
				Ok(())
			}
			Self::Unary(op, expr) => {
				write!(f, "(")?;
				write!(f, "{}", op.value)?;

				if let Some(expr) = expr {
					write!(f, "{expr}")?;
				} else {
					write!(f, "<expr>")?;
				}
				write!(f, ")")?;

				Ok(())
			}
			Self::Binary(lhs, op, rhs) => {
				write!(f, "(")?;
				if let Some(lhs) = lhs {
					write!(f, "{lhs}")?;
				} else {
					write!(f, "<expr>")?;
				}

				write!(f, " {} ", op.value)?;

				if let Some(rhs) = rhs {
					write!(f, "{rhs}")?;
				} else {
					write!(f, "<expr>")?;
				}
				write!(f, ")")?;

				Ok(())
			}
			Self::Ternary(lhs, qm, ihs, colon, rhs) => {
				write!(f, "(")?;
				if let Some(lhs) = lhs {
					write!(f, "{lhs}")?;
				} else {
					write!(f, "<expr>")?;
				}

				if qm.is_some() {
					write!(f, " ? ")?;
				} else {
					write!(f, " <?> ")?;
				}

				if let Some(ihs) = ihs {
					write!(f, "{ihs}")?;
				} else {
					write!(f, "<expr>")?;
				}

				if colon.is_some() {
					write!(f, " : ")?;
				} else {
					write!(f, " <:> ")?;
				}

				if let Some(rhs) = rhs {
					write!(f, "{rhs}")?;
				} else {
					write!(f, "<expr>")?;
				}
				write!(f, ")")?;

				Ok(())
			}
			Self::Wrapped(Delimited {
				open: _,
				inner,
				close,
			}) => {
				write!(f, "(")?;
				if let Some(inner) = inner {
					write!(f, "{inner}")?;
				} else {
					write!(f, "<expr>")?;
				}

				if close.is_some() {
					write!(f, ")")?;
				} else {
					write!(f, "<)>")?;
				}

				Ok(())
			}
		}
	}
}
// }}}

// {{{ The HasSpan trait
pub trait HasSpan {
	fn try_span_of(&self) -> Option<SourceSpan>;
	fn span_of(&self) -> SourceSpan {
		self.try_span_of().unwrap()
	}
}

impl HasSpan for SourceSpan {
	fn try_span_of(&self) -> Option<SourceSpan> {
		Some(self.clone())
	}
}

impl<T: HasSpan> HasSpan for Option<T> {
	fn try_span_of(&self) -> Option<SourceSpan> {
		self.as_ref()?.try_span_of()
	}
}

impl<T: HasSpan> HasSpan for Box<T> {
	fn try_span_of(&self) -> Option<SourceSpan> {
		self.as_ref().try_span_of()
	}
}

impl<T: HasSpan> HasSpan for Vec<T> {
	fn try_span_of(&self) -> Option<SourceSpan> {
		let mut result = None;

		for s in self {
			result = SourceSpan::merge_options(result, s.try_span_of());
		}

		result
	}
}

impl<T: HasSpan> HasSpan for Box<[T]> {
	fn try_span_of(&self) -> Option<SourceSpan> {
		let mut result = None;

		for s in self {
			result = SourceSpan::merge_options(result, s.try_span_of());
		}

		result
	}
}

/// NOTE: this implementation doesn't look at the inner value, hence why the
/// inner type is not required to implement [HasSpan] as well.
impl<T> HasSpan for Token<T> {
	fn try_span_of(&self) -> Option<SourceSpan> {
		Some(self.span.clone())
	}
}

impl HasSpan for lexer::Token {
	fn try_span_of(&self) -> Option<SourceSpan> {
		Some(self.span.clone())
	}
}

impl<T: HasSpan> HasSpan for SeparatedStep<T> {
	fn try_span_of(&self) -> Option<SourceSpan> {
		match self {
			Self::Separator(v) => v.try_span_of(),
			Self::Element(v) => v.try_span_of(),
		}
	}
}

impl<T: HasSpan> HasSpan for Separated<T> {
	fn try_span_of(&self) -> Option<SourceSpan> {
		self.elements.try_span_of()
	}
}

impl<T: HasSpan> HasSpan for Delimited<T> {
	fn try_span_of(&self) -> Option<SourceSpan> {
		SourceSpan::merge_options(
			self.open.try_span_of(),
			SourceSpan::merge_options(
				self.inner.try_span_of(),
				self.close.try_span_of(),
			),
		)
	}
}

impl HasSpan for Expr {
	fn try_span_of(&self) -> Option<SourceSpan> {
		match self {
			Expr::Int(token) => token.try_span_of(),
			Expr::Float(token) => token.try_span_of(),
			Expr::Variable(token) => token.try_span_of(),
			Expr::Property(expr, token) => SourceSpan::merge_options(
				expr.try_span_of(),
				token.try_span_of(),
			),
			Expr::Call(f, args) => {
				SourceSpan::merge_options(f.try_span_of(), args.try_span_of())
			}
			Expr::Unary(unary_operator, expr) => SourceSpan::merge_options(
				unary_operator.try_span_of(),
				expr.try_span_of(),
			),
			Expr::Binary(lhs, binary_operator, rhs) => {
				SourceSpan::merge_options(
					SourceSpan::merge_options(
						lhs.try_span_of(),
						binary_operator.try_span_of(),
					),
					rhs.try_span_of(),
				)
			}
			Expr::Ternary(lhs, qm, ihs, colon, rhs) => {
				SourceSpan::merge_options(
					SourceSpan::merge_options(
						SourceSpan::merge_options(
							lhs.try_span_of(),
							qm.try_span_of(),
						),
						SourceSpan::merge_options(
							ihs.try_span_of(),
							colon.try_span_of(),
						),
					),
					rhs.try_span_of(),
				)
			}
			Expr::Wrapped(delimited) => delimited.try_span_of(),
			Expr::Error(i) => i.try_span_of(),
		}
	}
}

impl HasSpan for Statement {
	fn try_span_of(&self) -> Option<SourceSpan> {
		match self {
			Statement::Expression(expr) => expr.try_span_of(),
			Statement::Assignment(lhs, eq, rhs) => {
				(&lhs, &eq, &rhs).try_span_of()
			}
			Statement::Declaration(decl) => decl.try_span_of(),
			Statement::If(if_) => if_.try_span_of(),
			Statement::For(for_) => for_.try_span_of(),
			Statement::Discard(token) => token.try_span_of(),
			Statement::Break(token) => token.try_span_of(),
			Statement::Continue(token) => token.try_span_of(),
			Statement::Return(token, value) => (token, value).try_span_of(),
		}
	}
}

impl HasSpan for LocalDeclaration {
	fn try_span_of(&self) -> Option<SourceSpan> {
		SourceSpan::merge_options(
			SourceSpan::merge_options(
				SourceSpan::merge_options(
					self.variable.try_span_of(),
					self.colon.try_span_of(),
				),
				SourceSpan::merge_options(
					self.ty.try_span_of(),
					self.equals.try_span_of(),
				),
			),
			self.value.try_span_of(),
		)
	}
}

impl HasSpan for If {
	fn try_span_of(&self) -> Option<SourceSpan> {
		self.branches.try_span_of()
	}
}

impl HasSpan for StatementBlock {
	fn try_span_of(&self) -> Option<SourceSpan> {
		self.statements.try_span_of()
	}
}

impl HasSpan for CondBranch {
	fn try_span_of(&self) -> Option<SourceSpan> {
		SourceSpan::merge_options(
			SourceSpan::merge_options(
				self.tok_leading.try_span_of(),
				self.condition.try_span_of(),
			),
			SourceSpan::merge_options(
				self.tok_then.try_span_of(),
				self.block.try_span_of(),
			),
		)
	}
}

impl HasSpan for For {
	fn try_span_of(&self) -> Option<SourceSpan> {
		SourceSpan::merge_options(
			SourceSpan::merge_options(
				self.tok_for.try_span_of(),
				self.steps.try_span_of(),
			),
			SourceSpan::merge_options(
				self.tok_do.try_span_of(),
				self.block.try_span_of(),
			),
		)
	}
}

impl HasSpan for Type {
	fn try_span_of(&self) -> Option<SourceSpan> {
		match self {
			Type::Named(token) => token.try_span_of(),
			Type::Array(array) => array.try_span_of(),
			Type::Struct(s) => s.try_span_of(),
		}
	}
}

impl HasSpan for Struct {
	fn try_span_of(&self) -> Option<SourceSpan> {
		self.fields.try_span_of()
	}
}

impl HasSpan for StructField {
	fn try_span_of(&self) -> Option<SourceSpan> {
		(&self.name, &self.colon, &self.ty).try_span_of()
	}
}

impl HasSpan for Array {
	fn try_span_of(&self) -> Option<SourceSpan> {
		(&self.dimensions, &self.ty).try_span_of()
	}
}

impl HasSpan for ArrayDimensions {
	fn try_span_of(&self) -> Option<SourceSpan> {
		(&self.first, &self.comma, &self.second).try_span_of()
	}
}

impl HasSpan for ProcArg {
	fn try_span_of(&self) -> Option<SourceSpan> {
		(&self.name, &self.colon, &self.ty).try_span_of()
	}
}

impl HasSpan for QualifiedName {
	fn try_span_of(&self) -> Option<SourceSpan> {
		self.0.try_span_of()
	}
}

impl<A: HasSpan, B: HasSpan> HasSpan for (A, B) {
	fn try_span_of(&self) -> Option<SourceSpan> {
		SourceSpan::merge_options(self.0.try_span_of(), self.1.try_span_of())
	}
}

impl<A: HasSpan, B: HasSpan, C: HasSpan> HasSpan for (A, B, C) {
	fn try_span_of(&self) -> Option<SourceSpan> {
		((&self.0, &self.1), &self.2).try_span_of()
	}
}

impl<A: HasSpan, B: HasSpan, C: HasSpan, D: HasSpan> HasSpan for (A, B, C, D) {
	fn try_span_of(&self) -> Option<SourceSpan> {
		((&self.0, &self.1), (&self.2, &self.3)).try_span_of()
	}
}

impl<A: HasSpan, B: HasSpan, C: HasSpan, D: HasSpan, E: HasSpan> HasSpan
	for (A, B, C, D, E)
{
	fn try_span_of(&self) -> Option<SourceSpan> {
		((&self.0, &self.1), (&self.2, &self.3), &self.4).try_span_of()
	}
}

impl<A: HasSpan> HasSpan for &A {
	fn try_span_of(&self) -> Option<SourceSpan> {
		(*self).try_span_of()
	}
}
// }}}
