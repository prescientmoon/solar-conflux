#![allow(unused)]
#![allow(clippy::large_enum_variant)]
use crate::lexer;

// {{{ Building blocks
#[derive(Debug, Clone)]
pub struct Token<T = ()> {
	tok: lexer::Token,
	value: T,

	// This is inefficient af, although it'll only allocate if trivia
	// is actually attached, so it's not the end of the world...
	trivia: Vec<lexer::Token>,
}

#[derive(Debug, Clone)]
pub struct Delimited<T> {
	open: Token,
	inner: T,
	close: Option<T>,
}

#[derive(Debug, Clone)]
pub struct Separated<Sep, T> {
	start: lexer::SourcePos,
	elements: Vec<SeparatedStep<Sep, T>>,
}

#[derive(Debug, Clone)]
pub enum SeparatedStep<Sep, T> {
	Separator(Sep),
	Element(T),
}
// }}}
// {{{ Modules
#[derive(Debug, Clone)]
pub struct File {
	entries: Vec<ModuleEntry>,
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
	args: Option<Delimited<Separated<Token, ProcArg>>>,
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
	steps: Separated<Token, Statement>,
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
	callee: Box<Expr>,
	arguments: Delimited<Separated<Token, Expr>>,
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
