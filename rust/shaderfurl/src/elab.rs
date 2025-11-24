#![allow(dead_code)]
use std::{
	cell::RefCell,
	collections::{HashMap, HashSet},
	fmt::Write,
};

use crate::{
	cst::{BinaryOperator, UnaryOperator},
	lowering::{
		Identifier, LoweringContext, ModuleId, Name, QualifiedIdentifier,
		StructId,
	},
};

// {{{ The elaboration context type
#[derive(Clone, Copy, Debug)]
pub struct BinderId(usize);

#[derive(Clone, Copy, Debug)]
pub struct PropId(usize);

#[derive(Debug, Clone)]
pub enum External {
	Uniform,
	UniformBuffer,
	Buffer,
	Attribute,
	Varying,
}

#[derive(Debug, Clone)]
pub enum Toplevel {
	Unknown,
	External(External),
	Proc(Proc),
}

pub struct Binder {
	// The original name in the source code.
	// Useful for error messages.
	name: String,
	ty: Type,
}

#[derive(Default)]
pub struct ElabContext {
	pub lowering_context: LoweringContext,
	structs: Vec<Struct>,
	props: Vec<Type>,
	binders: Vec<Binder>,
	toplevel: HashMap<BinderId, Toplevel>,
	aliases: HashMap<QualifiedIdentifier, Option<QualifiedIdentifier>>,
	types: HashMap<QualifiedIdentifier, Type>,
	imports: RefCell<HashMap<ModuleId, Box<[ModuleId]>>>,
}

impl ElabContext {
	pub fn from_scoping(scoping_context: LoweringContext) -> Self {
		Self { lowering_context: scoping_context, ..Default::default() }
	}
}
// }}}
// {{{ Types & expressions & statements
#[derive(Debug, Clone, Default)]
pub enum Type {
	#[default]
	Unknown,
	Unit,
	Struct(StructId),
	Array((usize, usize), Box<Type>),
	Proc(Box<[Type]>, Box<Type>),
}

#[derive(Clone, Debug)]
pub struct Struct {
	pub fields: Box<[PropId]>,
}

#[derive(Clone, Debug)]
pub enum Expr {
	// Base values
	Unknown,
	Unit,
	Bool(bool),
	Int(i64, Type),
	Float(f64, Type),
	Variable(BinderId),

	// Compound values
	Property(Box<Expr>, PropId),
	Call(BinderId, Vec<Expr>),
	Unary(UnaryOperator, Box<Expr>),
	Binary(Box<(Expr, BinaryOperator, Expr)>),
	Ternary(Box<(Expr, Expr, Expr)>),
}

#[derive(Clone, Debug)]
pub enum Statement {
	Unknown,
	Discard,
	Break,
	Continue,
	Return(Expr),
	Expression(Expr),
	If(Box<[(Expr, Block)]>),
	For(Box<(Statement, Statement, Statement)>, Block),
	Assignment(Expr, Option<BinaryOperator>, Expr),
	Declaration(BinderId, Option<Expr>),
}

#[derive(Clone, Debug, Default)]
pub struct Block(Box<[Statement]>);

#[derive(Clone, Debug)]
pub enum Proc {
	Native(String),
	Implemented(Box<[BinderId]>, Block),
}
// }}}
// {{{ Name resolution
type ModuleResolution = HashSet<ModuleId>;
impl ElabContext {
	fn resolve_name_externally(
		&self,
		module: ModuleId,
		name: &Name,
	) -> ModuleResolution {
		match &self.lowering_context[module] {
			crate::lowering::Module::Toplevel(_) => Default::default(),
			crate::lowering::Module::Import(_, inner) => {
				self.resolve_name_externally(*inner, name)
			}
			crate::lowering::Module::Fork(items) => items
				.into_iter()
				.filter(|(mod_name, _)| mod_name.to_name() == Some(name))
				.map(|(_, inner)| *inner)
				.collect(),
		}
	}

	pub fn resolve_path_externally(
		&self,
		module: ModuleId,
		name: &[Name],
	) -> ModuleResolution {
		match name {
			[] => HashSet::from([module]),
			[head, tail @ ..] => self
				.resolve_name_externally(module, head)
				.into_iter()
				.flat_map(|res| self.resolve_path_externally(res, tail))
				.collect(),
		}
	}

	fn resolve_name_internally_at_parent(
		&self,
		module: ModuleId,
		name: &Name,
	) -> ModuleResolution {
		if let Some(last) = self.lowering_context.module_parent(module) {
			self.resolve_name_internally(last, name)
		} else {
			Default::default()
		}
	}

	fn resolve_import(&self, module: ModuleId) -> Box<[ModuleId]> {
		if let Some(res) = self.imports.borrow().get(&module) {
			return res.clone(); // Not pretty, but oh well...
		}

		let crate::lowering::Module::Import(path, inner) =
			&self.lowering_context[module]
		else {
			panic!("Expected module to be an import")
		};

		let boxed: Box<[_]> = match path.0.as_ref() {
			[] => Box::new([]),
			[head, tail @ ..] => {
				let below = self.resolve_name_externally(*inner, head);
				let above =
					self.resolve_name_internally_at_parent(module, head);

				below
					.into_iter()
					.chain(above)
					.flat_map(|res| self.resolve_path_externally(res, tail))
					.collect()
			}
		};

		self.imports.borrow_mut().insert(module, boxed.clone());
		boxed
	}

	fn resolve_name_internally(
		&self,
		module: ModuleId,
		name: &Name,
	) -> ModuleResolution {
		let locally: ModuleResolution = match &self.lowering_context[module] {
			crate::lowering::Module::Toplevel(_) => Default::default(),
			crate::lowering::Module::Import(_, inner) => self
				.resolve_import(module)
				.into_iter()
				.flat_map(|res| self.resolve_name_externally(res, name))
				.chain(self.resolve_name_externally(*inner, name))
				.collect(),
			crate::lowering::Module::Fork(_) => {
				self.resolve_name_externally(module, name)
			}
		};

		let above = self.resolve_name_internally_at_parent(module, name);
		locally.into_iter().chain(above).collect()
	}

	pub fn resolve_path_internally(
		&self,
		module: ModuleId,
		name: &[Name],
	) -> ModuleResolution {
		match name {
			[] => HashSet::from([module]),
			[head, tail @ ..] => self
				.resolve_name_internally(module, head)
				.into_iter()
				.flat_map(|res| self.resolve_path_externally(res, tail))
				.collect(),
		}
	}

	pub fn print_qualified(&self, module: ModuleId) -> String {
		let mut out = String::new();

		let mut current = module;
		let mut telescope = vec![module];
		while let Some(parent) = self.lowering_context.module_parent(current) {
			telescope.push(parent);
			current = parent;
		}

		for (i, label) in telescope
			.iter()
			.rev()
			.filter_map(|elem| self.lowering_context.module_label(*elem))
			.enumerate()
		{
			if i > 0 {
				write!(&mut out, ".").unwrap();
			}

			match label {
				Identifier::Unknown => {
					write!(&mut out, "?").unwrap();
				}
				Identifier::Name(name) => {
					write!(&mut out, "{name}").unwrap();
				}
			}
		}

		out
	}
}
// }}}
