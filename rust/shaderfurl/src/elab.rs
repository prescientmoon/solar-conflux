#![allow(dead_code)]
use std::{
	cell::RefCell,
	collections::{HashMap, HashSet},
	fmt::Write,
};

use crate::{
	compact_report,
	cst::{BinaryOperator, HasSpan, UnaryOperator},
	lexer::SourceSpan,
	lowering::{Identifier, LoweringContext, ModuleId, Name, StructId},
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
pub struct ElabContext<'a> {
	pub lowering_context: LoweringContext,
	structs: Vec<Struct>,
	props: Vec<Type>,
	binders: Vec<Binder>,
	toplevel: HashMap<BinderId, Toplevel>,
	imports: RefCell<HashMap<ModuleId, Box<[ModuleId]>>>,
	types: RefCell<HashMap<ModuleId, Type>>,
	reports: RefCell<Vec<ariadne::Report<'a, SourceSpan>>>,
}

impl<'a> ElabContext<'a> {
	pub fn from_scoping(scoping_context: LoweringContext) -> Self {
		Self { lowering_context: scoping_context, ..Default::default() }
	}

	pub fn reports(
		&self,
	) -> std::cell::Ref<'_, Vec<ariadne::Report<'a, SourceSpan>>> {
		self.reports.borrow()
	}

	fn push_report(&self, report: ariadne::Report<'a, SourceSpan>) {
		self.reports.borrow_mut().push(report)
	}
}
// }}}
// {{{ Types & expressions & statements
#[derive(Debug, Clone, Copy, Default)]
pub enum PrimitiveType {
	#[default]
	Unknown,
	Unit,
	F32,
	F64,
	I32,
	I64,
	Bool,
}

#[derive(Debug, Clone)]
pub enum Type {
	Primitive(PrimitiveType),
	Struct(StructId),
	Array((usize, usize), Box<Type>),
	Proc(Box<[Type]>, Box<Type>),
}

impl Default for Type {
	fn default() -> Self {
		Self::Primitive(Default::default())
	}
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
impl ElabContext<'_> {
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

impl ElabContext<'_> {
	// {{{ Checking of types
	pub fn check_types(&self) {
		for module in self.lowering_context.module_ids() {
			if let crate::lowering::Module::Toplevel(
				crate::lowering::ModuleMember::Type(ty),
			) = &self.lowering_context[module]
			{
				self.elab_type_defined_at(module, ty, &mut Vec::new());
			}
		}
	}

	fn elab_type_defined_at(
		&self,
		module: ModuleId,
		ty: &crate::lowering::Type,

		// The types we were checking while we got here
		telescope: &mut Vec<ModuleId>,
	) -> Type {
		if telescope.contains(&module) {
			self.report_cycle(&telescope);
			Type::default()
		} else {
			if let Some(elaborated) = self.types.borrow().get(&module) {
				elaborated.clone()
			} else {
				telescope.push(module);
				let elaborated = self.elab_type(module, ty, telescope);
				telescope.pop();
				self.types.borrow_mut().insert(module, elaborated.clone());
				elaborated
			}
		}
	}

	fn elab_type(
		&self,
		module: ModuleId,
		ty: &crate::lowering::Type,
		telescope: &mut Vec<ModuleId>,
	) -> Type {
		match ty {
			crate::lowering::Type::Unknown => {
				Type::Primitive(PrimitiveType::Unknown)
			}
			crate::lowering::Type::Unit => Type::Primitive(PrimitiveType::Unit),
			crate::lowering::Type::Shared(inner) => {
				self.elab_type(module, inner, telescope)
			}
			crate::lowering::Type::Named(name) => {
				if name.0.len() == 1 {
					match name.0[0].name.as_str() {
						"f32" => return Type::Primitive(PrimitiveType::F32),
						"f64" => return Type::Primitive(PrimitiveType::F64),
						"i32" => return Type::Primitive(PrimitiveType::I32),
						"i64" => return Type::Primitive(PrimitiveType::I64),
						"bool" => return Type::Primitive(PrimitiveType::Bool),
						"unit" => return Type::Primitive(PrimitiveType::Unit),
						_ => {}
					};
				}

				let ids: ModuleResolution = self
					.resolve_path_internally(module, &name.0)
					.into_iter()
					.filter(|n| {
						matches!(
							&self.lowering_context[*n],
							crate::lowering::Module::Toplevel(_)
						)
					})
					.collect();

				if ids.is_empty() {
					compact_report!(
						(self, "NameNotFound", name),
						("Name \"{}\" not defined", name),
						("I could not resolve this name"),
					);

					Type::default()
				} else if ids.len() > 1 {
					self.report_ambiguous_name(name, &ids);
					Type::default()
				} else {
					let the_module = *ids.iter().next().unwrap();
					let crate::lowering::Module::Toplevel(toplevel) =
						&self.lowering_context[the_module]
					else {
						unreachable!()
					};

					match toplevel {
						crate::lowering::ModuleMember::Unknown => {
							Type::default()
						}
						crate::lowering::ModuleMember::Alias(
							qualified_identifier,
						) => self.elab_type_defined_at(
							the_module,
							&crate::lowering::Type::Named(
								qualified_identifier.clone(),
							),
							telescope,
						),
						crate::lowering::ModuleMember::Type(ty) => {
							self.elab_type_defined_at(the_module, ty, telescope)
						}
						_ => {
							self.report_not_a_type(name, the_module);
							Type::default()
						}
					}
				}
			}
			crate::lowering::Type::Array(dims, inner) => {
				let inner = self.elab_type(module, inner, telescope);
				// TODO: error out on certain sizes
				Type::Array(*dims, Box::new(inner))
			}
			crate::lowering::Type::Struct(struct_id) => {
				Type::Struct(*struct_id)
			}
		}
	}
	// }}}

	// Error reporting
	// {{{ Ambiguous name
	fn report_ambiguous_name(
		&self,
		name: &crate::lowering::QualifiedIdentifier,
		definitions: &ModuleResolution,
	) {
		let names: String = definitions
			.iter()
			.map(|i| self.print_qualified(*i))
			.collect::<Vec<_>>()
			.join(", "); // Is there no way to do this without allocating twice ;-;

		let mut report =
			ariadne::Report::build(ariadne::ReportKind::Error, name.span_of())
				.with_code("AmbiguousName")
				.with_message(format!(
					"Name \"{}\" is ambiguous and could refer to one of the following: {}",
					name, names
				))
				.with_label(
					ariadne::Label::new(name.span_of())
						.with_message("I am confused about this name")
						.with_order(-10),
				);

		for (i, (id, label)) in definitions
			.iter()
			.filter_map(|id| {
				if let Some(Identifier::Name(label)) =
					self.lowering_context.module_label(*id)
				{
					Some((id, label))
				} else {
					None
				}
			})
			.enumerate()
		{
			report = report.with_label(
				ariadne::Label::new(label.span_of())
					.with_message(format!(
						"\"{}\" is {} of the options I considered",
						self.print_qualified(*id),
						if i > 0 { "another one" } else { "one" }
					))
					.with_order(i as i32),
			)
		}

		self.push_report(report.finish());
	}
	// }}}
	// {{{ Not a type
	fn report_not_a_type(
		&self,
		name: &crate::lowering::QualifiedIdentifier,
		definition: ModuleId,
	) {
		let mut report =
			ariadne::Report::build(ariadne::ReportKind::Error, name.span_of())
				.with_code("NotAType")
				.with_message(format!(
					"Name \"{}\" does not refer to a type",
					name
				))
				.with_label(
					ariadne::Label::new(name.span_of())
						.with_message("I was expecting this to be a type...")
						.with_order(-10),
				);

		if let Some(Identifier::Name(label)) =
			self.lowering_context.module_label(definition)
		{
			report = report.with_label(
				ariadne::Label::new(label.span_of()).with_message(
					"...yet it does not look like one based on this definition",
				),
			)
		}

		self.push_report(report.finish());
	}
	// }}}
	// {{{ Definition cycle
	fn report_cycle(&self, definitions: &[ModuleId]) {
		let names: String = definitions
			.iter()
			.map(|i| self.print_qualified(*i))
			.collect::<Vec<_>>()
			.join(", "); // Is there no way to do this without allocating twice ;-;

		// NOTE: Declaration labels technically always exist, but if I
		// hypothetically ever made the parser ever more error tolerant, this code
		// could panic... Oh well, the solution would be tacking on more source info
		// related to unknown names in the lowering pass, but that's not an issue
		// I'll worry about for now :p
		let closer = self
			.lowering_context
			.module_label(*definitions.last().unwrap())
			.unwrap()
			.to_name()
			.unwrap();

		let mut report = ariadne::Report::build(
			ariadne::ReportKind::Error,
			closer.span_of(),
		)
		.with_code("CyclicDefinition")
		.with_message(format!("Cyclic definition encountered: {}", names));

		for (i, (id, label)) in definitions
			.iter()
			.filter_map(|id| {
				if let Some(Identifier::Name(label)) =
					self.lowering_context.module_label(*id)
				{
					Some((id, label))
				} else {
					None
				}
			})
			.enumerate()
		{
			report = report.with_label(
				ariadne::Label::new(label.span_of())
					.with_message(format!(
						"\"{}\" is {} of the elements I encountered in the cycle",
						self.print_qualified(*id),
						if i > 0 { "another one" } else { "one" },
					))
					.with_order(i as i32),
			)
		}

		self.push_report(report.finish());
	}
	// }}}
}
