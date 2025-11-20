#![allow(clippy::get_first)]
use std::rc::Rc;

use crate::cst::{BinaryOperator, UnaryOperator};

// I should probably intern the strings properly, but I'm laaaazy
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum Identifier {
	#[default]
	Unknown,
	// The equivalent of a "." inside a directory
	Here,
	Name(Rc<String>),
}

impl Identifier {
	fn from_cst(cst: &crate::cst::Token<String>) -> Self {
		Self::Name(Rc::new(cst.value.clone()))
	}

	fn from_cst_option(cst: &Option<crate::cst::Token<String>>) -> Self {
		cst.as_ref().map(Self::from_cst).unwrap_or_default()
	}
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct QualifiedIdentifier(Box<[Identifier]>);

impl QualifiedIdentifier {
	fn from_cst(cst: &crate::cst::QualifiedName) -> Self {
		Self(cst.0.iter().map(Identifier::from_cst).collect())
	}

	fn from_cst_option(cst: &Option<crate::cst::QualifiedName>) -> Self {
		cst.as_ref().map(Self::from_cst).unwrap_or_default()
	}
}

#[derive(Clone, Debug)]
pub enum Expr {
	Unit,
	Bool(bool),
	Int(i64),
	Float(f64),
	Variable(Identifier),
	Property(Box<Expr>, Identifier),
	Call(Box<Expr>, Vec<Expr>),
	Unary(UnaryOperator, Box<Expr>),
	Binary(Box<(Expr, BinaryOperator, Expr)>),
	Ternary(Box<(Expr, Expr, Expr)>),
	Unknown,
}

impl Expr {
	fn from_cst_option(cst: &Option<crate::cst::Expr>) -> Self {
		cst.as_ref().map_or(Self::Unknown, Self::from_cst)
	}

	fn from_cst_option_box(cst: &Option<Box<crate::cst::Expr>>) -> Self {
		cst.as_ref().map_or(Self::Unknown, |b| Self::from_cst(b))
	}

	fn from_cst(cst: &crate::cst::Expr) -> Self {
		match cst {
			crate::cst::Expr::Int(token) => Self::Int(token.value),
			crate::cst::Expr::Float(token) => Self::Float(token.value),
			crate::cst::Expr::Variable(token) => {
				Self::Variable(Identifier::from_cst(token))
			}
			crate::cst::Expr::Property(expr, token) => Self::Property(
				Box::new(Self::from_cst_option_box(expr)),
				Identifier::from_cst(token),
			),
			crate::cst::Expr::Call(f, args) => Self::Call(
				Box::new(Self::from_cst(f)),
				args.iter().map(Self::from_cst).collect(),
			),
			crate::cst::Expr::Unary(token, expr) => Self::Unary(
				token.value,
				Box::new(Self::from_cst_option_box(expr)),
			),
			crate::cst::Expr::Binary(left, token, right) => {
				Self::Binary(Box::new((
					Self::from_cst_option_box(left),
					token.value,
					Self::from_cst_option_box(right),
				)))
			}
			crate::cst::Expr::Ternary(cond, _, then, _, otherwise) => {
				Self::Ternary(Box::new((
					Self::from_cst_option_box(cond),
					Self::from_cst_option_box(then),
					Self::from_cst_option_box(otherwise),
				)))
			}
			crate::cst::Expr::Wrapped(delimited) => {
				Self::from_cst_option_box(&delimited.inner)
			}
			crate::cst::Expr::Error(_) => Self::Unknown,
		}
	}
}

#[derive(Debug, Clone, Default)]
pub enum Type {
	#[default]
	Unknown,
	Unit,
	Shared(Rc<Type>),
	Named(Identifier),
	Struct(Vec<(Identifier, Type)>),
	Array((usize, usize), Box<Type>),
}

impl Type {
	fn shared(self) -> Self {
		Self::Shared(Rc::new(self))
	}

	fn from_cst_option(cst: &Option<crate::cst::Type>) -> Self {
		cst.as_ref().map_or(Self::Unknown, Self::from_cst)
	}

	fn from_cst_option_box(cst: &Option<Box<crate::cst::Type>>) -> Self {
		cst.as_ref().map_or(Self::Unknown, |b| Self::from_cst(b))
	}

	fn from_cst(cst: &crate::cst::Type) -> Self {
		match cst {
			crate::cst::Type::Named(token) => {
				Self::Named(Identifier::from_cst(token))
			}
			crate::cst::Type::Struct(st) => Self::Struct(
				st.fields
					.iter()
					.map(|field| {
						(
							Identifier::from_cst_option(&field.name),
							Self::from_cst_option(&field.ty),
						)
					})
					.collect(),
			),
			crate::cst::Type::Array(array) => Self::Array(
				(
					array
						.dimensions
						.inner
						.as_ref()
						.map_or(0, |d| d.first.value),
					array.dimensions.inner.as_ref().map_or(0, |d| {
						d.second.as_ref().map_or(1, |s| s.value)
					}),
				),
				Box::new(Self::from_cst_option_box(&array.ty)),
			),
		}
	}
}

#[derive(Clone, Debug)]
pub enum Statement {
	Unknown,
	Discard,
	Break,
	Continue,
	Return(Expr),
	Expression(Expr),
	If(Vec<(Expr, Block)>),
	For(Box<(Statement, Statement, Statement)>, Block),
	Assignment(Expr, Option<BinaryOperator>, Expr),
	Declaration(Identifier, Option<Type>, Option<Expr>),
}

impl Statement {
	fn from_cst_option(cst: Option<&crate::cst::Statement>) -> Self {
		cst.map_or(Self::Unknown, Self::from_cst)
	}

	fn from_cst(cst: &crate::cst::Statement) -> Self {
		match cst {
			crate::cst::Statement::Expression(expr) => {
				Self::Expression(Expr::from_cst(expr))
			}
			crate::cst::Statement::Assignment(left, op, right) => {
				Self::Assignment(
					Expr::from_cst_option(left),
					op.value,
					Expr::from_cst_option(right),
				)
			}
			crate::cst::Statement::Declaration(local_declaration) => {
				Self::Declaration(
					match &local_declaration.variable {
						Some(crate::cst::Expr::Variable(v)) => {
							Identifier::from_cst(v)
						}
						_ => Identifier::default(),
					},
					local_declaration.ty.as_ref().map(Type::from_cst),
					local_declaration.value.as_ref().map(Expr::from_cst),
				)
			}
			crate::cst::Statement::If(branches) => Self::If(
				branches
					.branches
					.iter()
					.map(|branch| {
						(
							branch
								.condition
								.as_ref()
								.map_or(Expr::Bool(true), Expr::from_cst),
							Block::from_cst_option(&branch.block),
						)
					})
					.collect(),
			),
			crate::cst::Statement::For(for_) => {
				let steps: Vec<_> = for_.steps.iter().take(3).collect();
				Self::For(
					Box::new((
						Statement::from_cst_option(steps.get(0).copied()),
						Statement::from_cst_option(steps.get(1).copied()),
						Statement::from_cst_option(steps.get(2).copied()),
					)),
					Block::from_cst_option(&for_.block),
				)
			}
			crate::cst::Statement::Discard(_) => Self::Discard,
			crate::cst::Statement::Break(_) => Self::Break,
			crate::cst::Statement::Continue(_) => Self::Continue,
			crate::cst::Statement::Return(_, expr) => {
				Self::Return(expr.as_ref().map_or(Expr::Unit, Expr::from_cst))
			}
		}
	}
}

#[derive(Clone, Debug, Default)]
pub struct Block(Vec<Statement>);

impl Block {
	fn from_cst(cst: &crate::cst::StatementBlock) -> Self {
		Self(cst.statements.iter().map(Statement::from_cst).collect())
	}

	fn from_cst_option(cst: &Option<crate::cst::StatementBlock>) -> Self {
		cst.as_ref().map(Self::from_cst).unwrap_or_default()
	}
}

#[derive(Clone, Debug)]
pub struct Proc {
	args: Vec<(Identifier, Type)>,
	ret: Type,
	body: ProcBody,
}

#[derive(Clone, Debug)]
pub enum ProcBody {
	Native(String),
	Implemented(Block),
}

impl Proc {
	fn from_cst(cst: &crate::cst::Proc) -> Self {
		let mut args = Vec::new();
		if let Some(d) = &cst.args {
			let mut untyped = Vec::new();
			for arg in d.inner.iter() {
				let id = Identifier::from_cst_option(&arg.name);
				if let Some(ty) = &arg.ty {
					let ty = Type::shared(Type::from_cst(ty)); // Avoid deep copies
					for old in std::mem::take(&mut untyped) {
						args.push((old, ty.clone()));
					}
					args.push((id, ty));
				} else {
					untyped.push(id)
				}
			}

			for old in untyped {
				args.push((old, Type::Unknown));
			}
		}
		Self {
			args,
			ret: cst.ret_type.as_ref().map_or(Type::Unit, Type::from_cst),
			body: match &cst.body {
				crate::cst::ProcBody::Native(token) => {
					ProcBody::Native(token.value.clone())
				}
				crate::cst::ProcBody::Implemented(block) => {
					ProcBody::Implemented(Block::from_cst(block))
				}
			},
		}
	}
}

#[derive(Debug, Clone)]
pub enum ScopeMember {
	Unknown,
	Uniform(Type),
	UniformBuffer(Type),
	Buffer(Type),
	Attribute(Type),
	Varying(Type),
	Alias(Identifier, Option<Type>),
	Type(Type),
	Proc(Proc),
}

pub type Module = Scope;

#[derive(Debug, Clone, Default)]
pub struct Scope {
	imports: Vec<QualifiedIdentifier>,
	default: Vec<ScopeMember>,
	/// Module names are allowed to appear more than once!
	/// [Identifier::Here] is also allowed!
	nested: Vec<(Identifier, Module)>,
}

impl Scope {
	fn get_nested_mut(&mut self, target: &Identifier) -> &mut Self {
		match target {
			Identifier::Here => self,
			_ => {
				if !self
					.nested
					.iter()
					.any(|(i, m)| m.imports.is_empty() && i == target)
				{
					return self.get_nested_fresh_mut(target);
				}

				self.nested
					.iter_mut()
					.rev() // In case we've just pushed one
					.find(|(i, m)| m.imports.is_empty() && i == target)
					.map(|(_, m)| m)
					.unwrap()
			}
		}
	}

	fn get_nested_fresh_mut(&mut self, target: &Identifier) -> &mut Self {
		self.nested.push((target.clone(), Self::default()));
		&mut self.nested.last_mut().unwrap().1
	}

	fn get_hyper_nested_mut(&mut self, path: &[Identifier]) -> &mut Self {
		match path {
			[] => self,
			[head, rest @ ..] => {
				self.get_nested_mut(head).get_hyper_nested_mut(rest)
			}
		}
	}

	fn get_hyper_nested_fresh_mut(&mut self, path: &[Identifier]) -> &mut Self {
		match path {
			[] => self.get_nested_fresh_mut(&Identifier::Here),
			[head, rest @ ..] => self
				.get_nested_fresh_mut(head)
				.get_hyper_nested_fresh_mut(rest),
		}
	}

	fn insert(&mut self, at: &Identifier, member: ScopeMember) {
		self.get_nested_mut(at).default.push(member)
	}

	fn insert_at(&mut self, path: &[Identifier], member: ScopeMember) {
		self.get_hyper_nested_mut(path).default.push(member)
	}

	fn merge(&mut self, mut other: Self) {
		self.default.append(&mut other.default);
		for (at, member) in other.nested {
			self.get_nested_mut(&at).merge(member);
		}
	}

	pub fn from_cst(cst: &Vec<crate::cst::ModuleEntry>) -> Self {
		let mut out = Self::default();
		for entry in cst {
			match entry {
				crate::cst::ModuleEntry::Import(import_statement) => {
					out.imports.push(QualifiedIdentifier::from_cst_option(
						&import_statement.path,
					));
				}
				crate::cst::ModuleEntry::Module(nested_module) => {
					let path = QualifiedIdentifier::from_cst_option(
						&nested_module.name,
					);

					out.get_hyper_nested_fresh_mut(&path.0)
						.merge(Self::from_cst(&nested_module.entries));
				}
				crate::cst::ModuleEntry::Declaration(declaration) => {
					let id = QualifiedIdentifier::from_cst(&declaration.name);
					let ty = declaration.ty.as_ref().map(Type::from_cst);
					match &declaration.value {
						None => out.insert_at(&id.0, ScopeMember::Unknown),
						Some(crate::cst::DeclValue::Alias(token)) => out
							.insert_at(
								&id.0,
								ScopeMember::Alias(
									Identifier::from_cst(token),
									ty,
								),
							),
						Some(crate::cst::DeclValue::Type(ty)) => out.insert_at(
							&id.0,
							ScopeMember::Type(Type::from_cst(ty)),
						),
						Some(crate::cst::DeclValue::Varying(_)) => out
							.insert_at(
								&id.0,
								ScopeMember::Varying(ty.unwrap_or_default()),
							),
						Some(crate::cst::DeclValue::Attribute(_)) => out
							.insert_at(
								&id.0,
								ScopeMember::Attribute(ty.unwrap_or_default()),
							),
						Some(crate::cst::DeclValue::Uniform(_)) => out
							.insert_at(
								&id.0,
								ScopeMember::Uniform(ty.unwrap_or_default()),
							),
						Some(crate::cst::DeclValue::UniformBuffer(_)) => out
							.insert_at(
								&id.0,
								ScopeMember::UniformBuffer(
									ty.unwrap_or_default(),
								),
							),
						Some(crate::cst::DeclValue::Buffer(_)) => out
							.insert_at(
								&id.0,
								ScopeMember::Buffer(ty.unwrap_or_default()),
							),
						Some(crate::cst::DeclValue::Proc(proc)) => out
							.insert_at(
								&id.0,
								ScopeMember::Proc(Proc::from_cst(proc)),
							),
					};
				}
			};
		}

		out
	}
}
