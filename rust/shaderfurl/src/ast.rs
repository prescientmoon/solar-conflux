#![allow(clippy::large_enum_variant)]
#![allow(dead_code)]
use crate::cst::{BinaryOperator, Token, UnaryOperator};

pub type Name = Option<Token<String>>;

// {{{ Modules
#[derive(Debug, Clone)]
pub struct File {
	entries: Vec<ModuleEntry>,
}

#[derive(Debug, Clone)]
pub enum ModuleEntry {
	Import(Token, Option<Token<String>>),
	Module(Name, Vec<ModuleEntry>),
	Declaration(Declaration),
}

#[derive(Debug, Clone)]
pub struct Declaration {
	name: Name,
	ty: Option<Type>,
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
	args: Vec<(Name, Option<Type>)>,
	ret: Option<Type>,
	body: ProcBody,
}

#[derive(Debug, Clone)]
pub enum ProcBody {
	Native(Option<Token<String>>),
	Implemented(ExprBlock),
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
	Assignment(Expr, Expr),
	Declaration(Name, Option<Type>, Expr),
	If(If),
	For(For),
	Discard,
	Break,
	Continue,
	Return,
}

#[derive(Debug, Clone)]
pub struct If {
	branches: Vec<(Expr, ExprBlock)>,
	else_branch: Option<ExprBlock>,
}

#[derive(Debug, Clone)]
pub struct For {
	steps: Box<(Statement, Statement, Statement)>,
	block: ExprBlock,
}
// }}}
// {{{ Expressions
#[derive(Debug, Clone)]
pub enum Expr {
	Unknown,
	Int(i64),
	Float(f64),
	Variable(Token<String>),
	Property(Box<Expr>, Token<String>),
	Call(Box<Expr>, Vec<Expr>),
	Unary(UnaryOperator, Box<Expr>),
	Binary(Box<Expr>, BinaryOperator, Box<Expr>),
	Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
}
// }}}
// {{{ Types
#[derive(Debug, Clone)]
pub enum Type {
	Unknown,
	Named(Token<String>),
	Struct(Vec<(Name, Type)>),
	Array(Option<(usize, Option<usize>)>, Box<Type>),
}
// }}}
