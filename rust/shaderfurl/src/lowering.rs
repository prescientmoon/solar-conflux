#![allow(clippy::get_first)]
#![allow(dead_code)]
use std::{fmt::Display, ops::Index, rc::Rc};

use crate::cst::{BinaryOperator, UnaryOperator};

// {{{ Structs
#[derive(Clone, Copy, Debug)]
pub struct StructId(usize);

#[derive(Clone, Debug)]
pub struct Struct {
	pub fields: Box<[(Identifier, Type)]>,
}
// }}}
// {{{ Lowering contexts
// The context in which this pass runs
#[derive(Default, Debug)]
pub struct LoweringContext {
	structs: Vec<Struct>,
	// (module, name, parent)
	pub modules: Vec<(Module, Option<Identifier>, Option<ModuleId>)>,
}

impl LoweringContext {
	fn register_struct(&mut self, s: Struct) -> StructId {
		let id = StructId(self.structs.len());
		self.structs.push(s);
		id
	}

	fn mark_parent(&mut self, module: ModuleId, parent: ModuleId) {
		self.modules.get_mut(module.0).unwrap().2 = Some(parent);
	}

	fn mark_label(&mut self, module: ModuleId, label: Identifier) {
		self.modules.get_mut(module.0).unwrap().1 = Some(label);
	}

	fn register_module(&mut self, module: Module) -> ModuleId {
		let id = ModuleId(self.modules.len());

		// Fuck you, borrow checker ;-;
		self.modules.push((module.clone(), None, None));

		match module.clone() {
			Module::Toplevel(_) => {}
			Module::Import(_, inner) => self.mark_parent(inner, id),
			Module::Fork(inner) => {
				for (label, member) in inner {
					self.mark_parent(member, id);
					self.mark_label(member, label);
				}
			}
		}

		id
	}

	pub fn module_parent(&self, module: ModuleId) -> Option<ModuleId> {
		self.modules.get(module.0).unwrap().2
	}

	pub fn module_label(&self, module: ModuleId) -> Option<&Identifier> {
		self.modules.get(module.0).unwrap().1.as_ref()
	}

	// To be used for debugging only
	pub fn inner_module_by_label(&self, label: &str) -> ModuleId {
		let id = Identifier::from_str(label);
		let outer = self
			.modules
			.iter()
			.enumerate()
			.find(|(_, (_, mod_label, _))| mod_label.as_ref() == Some(&id))
			.unwrap()
			.0;

		let mut outer = ModuleId(outer);
		while let Module::Import(_, inner) = self[outer] {
			outer = inner;
		}
		outer
	}
}

impl Index<StructId> for LoweringContext {
	type Output = Struct;
	fn index(&self, index: StructId) -> &Self::Output {
		&self.structs[index.0]
	}
}

impl Index<ModuleId> for LoweringContext {
	type Output = Module;
	fn index(&self, index: ModuleId) -> &Self::Output {
		&self.modules[index.0].0
	}
}
// }}}
// {{{ The `FromCst` trait
pub trait FromCst<Cst>: Sized {
	fn from_cst(ctx: &mut LoweringContext, cst: &Cst) -> Self;
	fn from_cst_option(ctx: &mut LoweringContext, cst: Option<&Cst>) -> Self
	where
		Self: Default,
	{
		Self::from_cst_option_or(ctx, cst, Self::default())
	}

	fn from_cst_option_or(
		ctx: &mut LoweringContext,
		cst: Option<&Cst>,
		def: Self,
	) -> Self {
		cst.as_ref().map(|v| Self::from_cst(ctx, v)).unwrap_or(def)
	}
}

impl<Cst, T: FromCst<Cst> + Default> FromCst<Option<Cst>> for T {
	fn from_cst(ctx: &mut LoweringContext, cst: &Option<Cst>) -> Self {
		Self::from_cst_option(ctx, cst.as_ref())
	}
}

impl<Cst, T: FromCst<Cst>> FromCst<Box<Cst>> for T {
	fn from_cst(ctx: &mut LoweringContext, cst: &Box<Cst>) -> Self {
		Self::from_cst(ctx, cst)
	}
}

impl<Cst, T: FromCst<Cst>> FromCst<Vec<Cst>> for Vec<T> {
	fn from_cst(ctx: &mut LoweringContext, cst: &Vec<Cst>) -> Self {
		cst.iter().map(|v| T::from_cst(ctx, v)).collect()
	}
}

impl<Cst, T: FromCst<Cst>> FromCst<Box<[Cst]>> for Box<[T]> {
	fn from_cst(ctx: &mut LoweringContext, cst: &Box<[Cst]>) -> Self {
		cst.iter().map(|v| T::from_cst(ctx, v)).collect()
	}
}
// }}}

// `FromCst` implementation
// {{{ Names
// I should probably intern the strings properly, but I'm laaaazy
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Name(pub Rc<String>);

impl Name {
	pub fn from_str(s: &str) -> Self {
		Self(Rc::new(s.to_string()))
	}
}

impl Display for Name {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.0)
	}
}

impl FromCst<crate::cst::Token<String>> for Name {
	fn from_cst(
		_: &mut LoweringContext,
		cst: &crate::cst::Token<String>,
	) -> Self {
		Self(Rc::new(cst.value.clone()))
	}
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum Identifier {
	#[default]
	Unknown,
	Name(Name),
}

impl Identifier {
	pub fn from_str(s: &str) -> Self {
		Self::Name(Name::from_str(s))
	}

	pub fn to_name(&self) -> Option<&Name> {
		match self {
			Self::Unknown => None,
			Self::Name(name) => Some(name),
		}
	}
}

impl FromCst<crate::cst::Token<String>> for Identifier {
	fn from_cst(
		ctx: &mut LoweringContext,
		cst: &crate::cst::Token<String>,
	) -> Self {
		Self::Name(Name::from_cst(ctx, cst))
	}
}

#[derive(Debug, Clone, Default)]
pub struct QualifiedIdentifier(pub Box<[Name]>);

impl FromCst<crate::cst::QualifiedName> for QualifiedIdentifier {
	fn from_cst(
		ctx: &mut LoweringContext,
		cst: &crate::cst::QualifiedName,
	) -> Self {
		Self(FromCst::from_cst(ctx, &cst.0))
	}
}
// }}}
// {{{ Expressions
#[derive(Clone, Debug, Default)]
pub enum Expr {
	#[default]
	Unknown,
	Unit,
	Bool(bool),
	Int(i64),
	Float(f64),
	Variable(Identifier),
	Property(Box<Expr>, Identifier),
	Call(Box<Expr>, Box<[Expr]>),
	Unary(UnaryOperator, Box<Expr>),
	Binary(Box<(Expr, BinaryOperator, Expr)>),
	Ternary(Box<(Expr, Expr, Expr)>),
}

impl FromCst<crate::cst::Expr> for Expr {
	fn from_cst(ctx: &mut LoweringContext, cst: &crate::cst::Expr) -> Self {
		match cst {
			crate::cst::Expr::Int(token) => Self::Int(token.value),
			crate::cst::Expr::Float(token) => Self::Float(token.value),
			crate::cst::Expr::Variable(token) => {
				Self::Variable(Identifier::from_cst(ctx, token))
			}
			crate::cst::Expr::Property(expr, token) => Self::Property(
				Box::new(Self::from_cst(ctx, expr)),
				Identifier::from_cst(ctx, token),
			),
			crate::cst::Expr::Call(f, args) => Self::Call(
				Box::new(Self::from_cst(ctx, f)),
				FromCst::from_cst(ctx, args),
			),
			crate::cst::Expr::Unary(token, expr) => {
				Self::Unary(token.value, Box::new(Self::from_cst(ctx, expr)))
			}
			crate::cst::Expr::Binary(left, token, right) => {
				Self::Binary(Box::new((
					Self::from_cst(ctx, left),
					token.value,
					Self::from_cst(ctx, right),
				)))
			}
			crate::cst::Expr::Ternary(cond, _, then, _, otherwise) => {
				Self::Ternary(Box::new((
					Self::from_cst(ctx, cond),
					Self::from_cst(ctx, then),
					Self::from_cst(ctx, otherwise),
				)))
			}
			crate::cst::Expr::Wrapped(delimited) => {
				Self::from_cst(ctx, &delimited.inner)
			}
			crate::cst::Expr::Error(_) => Self::Unknown,
		}
	}
}
// }}}
// {{{ Types
#[derive(Debug, Clone, Default)]
pub enum Type {
	#[default]
	Unknown,
	Unit,
	Shared(Rc<Type>),
	Named(Identifier),
	Array((usize, usize), Box<Type>),
	Struct(StructId),
}

impl Type {
	fn shared(self) -> Self {
		Self::Shared(Rc::new(self))
	}
}

impl FromCst<crate::cst::Type> for Type {
	fn from_cst(ctx: &mut LoweringContext, cst: &crate::cst::Type) -> Self {
		match cst {
			crate::cst::Type::Named(token) => {
				Self::Named(Identifier::from_cst(ctx, token))
			}
			crate::cst::Type::Struct(st) => {
				let fields = st
					.fields
					.iter()
					.map(|field| {
						(
							Identifier::from_cst(ctx, &field.name),
							Self::from_cst(ctx, &field.ty),
						)
					})
					.collect();
				let s = Struct { fields };
				Self::Struct(ctx.register_struct(s))
			}
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
				Box::new(Self::from_cst(ctx, &array.ty)),
			),
		}
	}
}
// }}}
// {{{ Statements
#[derive(Clone, Debug, Default)]
pub enum Statement {
	#[default]
	Unknown,
	Discard,
	Break,
	Continue,
	Return(Expr),
	Expression(Expr),
	If(Box<[(Expr, Block)]>),
	For(Box<(Statement, Statement, Statement)>, Block),
	Assignment(Expr, Option<BinaryOperator>, Expr),
	Declaration(Identifier, Option<Type>, Option<Expr>),
}

impl FromCst<crate::cst::Statement> for Statement {
	fn from_cst(
		ctx: &mut LoweringContext,
		cst: &crate::cst::Statement,
	) -> Self {
		match cst {
			crate::cst::Statement::Expression(expr) => {
				Self::Expression(Expr::from_cst(ctx, expr))
			}
			crate::cst::Statement::Assignment(left, op, right) => {
				Self::Assignment(
					Expr::from_cst_option(ctx, left.as_ref()),
					op.value,
					Expr::from_cst_option(ctx, right.as_ref()),
				)
			}
			crate::cst::Statement::Declaration(local_declaration) => {
				Self::Declaration(
					match &local_declaration.variable {
						Some(crate::cst::Expr::Variable(v)) => {
							Identifier::from_cst(ctx, v)
						}
						_ => Identifier::default(),
					},
					local_declaration
						.ty
						.as_ref()
						.map(|t| Type::from_cst(ctx, t)),
					local_declaration
						.value
						.as_ref()
						.map(|v| Expr::from_cst(ctx, v)),
				)
			}
			crate::cst::Statement::If(branches) => Self::If(
				branches
					.branches
					.iter()
					.map(|branch| {
						(
							Expr::from_cst_option_or(
								ctx,
								branch.condition.as_ref(),
								Expr::Bool(true),
							),
							Block::from_cst(ctx, &branch.block),
						)
					})
					.collect(),
			),
			crate::cst::Statement::For(for_) => {
				let steps: Vec<_> = for_.steps.iter().take(3).collect();
				Self::For(
					Box::new((
						Self::from_cst_option(ctx, steps.get(0).copied()),
						Self::from_cst_option(ctx, steps.get(1).copied()),
						Self::from_cst_option(ctx, steps.get(2).copied()),
					)),
					Block::from_cst(ctx, &for_.block),
				)
			}
			crate::cst::Statement::Discard(_) => Self::Discard,
			crate::cst::Statement::Break(_) => Self::Break,
			crate::cst::Statement::Continue(_) => Self::Continue,
			crate::cst::Statement::Return(_, expr) => Self::Return(
				FromCst::from_cst_option_or(ctx, expr.as_ref(), Expr::Unit),
			),
		}
	}
}

#[derive(Clone, Debug, Default)]
pub struct Block(Box<[Statement]>);

impl FromCst<crate::cst::StatementBlock> for Block {
	fn from_cst(
		ctx: &mut LoweringContext,
		cst: &crate::cst::StatementBlock,
	) -> Self {
		Self(FromCst::from_cst(ctx, &cst.statements))
	}
}
// }}}
// {{{ Procedures
#[derive(Clone, Debug, Default)]
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

impl Default for ProcBody {
	fn default() -> Self {
		Self::Implemented(Block::default())
	}
}

impl FromCst<crate::cst::Proc> for Proc {
	fn from_cst(ctx: &mut LoweringContext, cst: &crate::cst::Proc) -> Self {
		let mut args = Vec::new();
		if let Some(d) = &cst.args {
			let mut untyped = Vec::new();
			for arg in d.inner.iter() {
				let id = Identifier::from_cst(ctx, &arg.name);
				if let Some(ty) = &arg.ty {
					let ty = Type::shared(Type::from_cst(ctx, ty)); // Avoid deep copies
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
			ret: Type::from_cst_option_or(
				ctx,
				cst.ret_type.as_ref(),
				Type::Unit,
			),
			body: match &cst.body {
				crate::cst::ProcBody::Native(token) => {
					ProcBody::Native(token.value.clone())
				}
				crate::cst::ProcBody::Implemented(block) => {
					ProcBody::Implemented(Block::from_cst(ctx, block))
				}
			},
		}
	}
}
// }}}
// {{{ Modules
#[derive(Debug, Clone, Default)]
pub enum ModuleMember {
	#[default]
	Unknown,
	External(crate::cst::ExternalValue, Type),
	Alias(QualifiedIdentifier),
	Type(Type),
	Proc(Proc),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ModuleId(pub usize);

#[derive(Clone, Debug)]
pub enum Module {
	// import ...
	// ...
	Import(QualifiedIdentifier, ModuleId),
	// A single top-level declaration
	Toplevel(ModuleMember),
	// Contains a series of modules qualified by a single identifier.
	// The same name might appear more than once!
	Fork(Box<[(Identifier, ModuleId)]>),
}

impl ModuleId {
	fn qualify(
		self,
		ctx: &mut LoweringContext,
		path: QualifiedIdentifier,
	) -> (Identifier, ModuleId) {
		let mut path = path.0.into_vec();
		let mut toplevel = self;

		while let Some(last) = path.pop() {
			if path.is_empty() {
				return (Identifier::Name(last), toplevel);
			} else {
				toplevel = ctx.register_module(Module::Fork(Box::new([(
					Identifier::Name(last),
					toplevel,
				)])))
			}
		}

		(Identifier::Unknown, toplevel)
	}
}

impl FromCst<Vec<crate::cst::ModuleEntry>> for ModuleId {
	fn from_cst(
		ctx: &mut LoweringContext,
		cst: &Vec<crate::cst::ModuleEntry>,
	) -> Self {
		let mut out = Vec::new();
		for entry in cst {
			match entry {
				crate::cst::ModuleEntry::Import(_) => {}
				crate::cst::ModuleEntry::Module(nested_module) => {
					let path =
						QualifiedIdentifier::from_cst(ctx, &nested_module.name);

					out.push(
						Self::from_cst(ctx, &nested_module.entries)
							.qualify(ctx, path),
					)
				}
				crate::cst::ModuleEntry::Declaration(declaration) => {
					let id =
						QualifiedIdentifier::from_cst(ctx, &declaration.name);
					let ty =
						declaration.ty.as_ref().map(|v| Type::from_cst(ctx, v));

					let toplevel = match &declaration.value {
						None => ModuleMember::Unknown,
						Some(crate::cst::DeclValue::Alias(name)) => {
							ModuleMember::Alias(QualifiedIdentifier::from_cst(
								ctx, name,
							))
						}
						Some(crate::cst::DeclValue::Type(ty)) => {
							ModuleMember::Type(Type::from_cst(ctx, ty))
						}
						Some(crate::cst::DeclValue::External(v)) => {
							ModuleMember::External(
								v.value,
								ty.unwrap_or_default(),
							)
						}
						Some(crate::cst::DeclValue::Proc(proc)) => {
							ModuleMember::Proc(Proc::from_cst(ctx, proc))
						}
					};

					out.push(
						ctx.register_module(Module::Toplevel(toplevel))
							.qualify(ctx, id),
					)
				}
			};
		}

		let mut out = ctx.register_module(Module::Fork(out.into_boxed_slice()));
		for entry in cst.iter().rev() {
			if let crate::cst::ModuleEntry::Import(import) = entry {
				let ident = QualifiedIdentifier::from_cst(ctx, &import.path);
				out = ctx.register_module(Module::Import(ident, out))
			}
		}
		out
	}
}
// }}}
