#![allow(unused)]
#![allow(clippy::large_enum_variant)]
use crate::lexer;

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
pub enum Type {}

#[derive(Debug, Clone)]
pub enum DeclValue {
	Proc(Proc),
	Type(Type),
	Alias(Token<String>), // We don't yet know what this is aliasing to!
}

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
	Implemented(ExprBlock),
}

#[derive(Debug, Clone)]
pub struct ExprBlock {
	statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {}
