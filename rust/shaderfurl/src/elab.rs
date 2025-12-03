#![allow(dead_code)]
use std::{
	cell::{Ref, RefCell},
	collections::{HashMap, HashSet},
	fmt::Write,
};

use crate::{
	compact_report,
	cst::{BinaryOperator, HasSpan, UnaryOperator},
	lexer::SourceSpan,
	lowering::{Identifier, LoweringContext, ModuleId, Name, StructId},
	telescope::Telescope,
};

/*
TODO:
- [ ] Elaborate aliases
- [x] Elaborate type definitions
- [ ] Elaborate structs
- [ ] Elaborate proc types
- [ ] Elaborate statements
- [ ] Elaborate expressions
*/

// {{{ The elaboration context type
#[derive(Clone, Copy, Debug)]
pub struct BinderId(usize);

#[derive(Clone, Copy, Debug)]
pub struct PropId(usize);

#[derive(Clone, Copy, Debug)]
pub struct ArrayTypeId(usize);

pub struct Binder {
	// The original name in the source code.
	// Useful for error messages.
	name: String,
	ty: Type,
}

#[derive(Default)]
pub struct ElabContext<'a> {
	pub lowering_context: LoweringContext,

	/// Indexed by [StructId]
	structs: Vec<Struct>,

	/// Indexed by [ArrayTypeId]
	arrays: RefCell<Vec<ArrayType>>,

	// Toplevel elaboration memorization
	imports: RefCell<HashMap<ModuleId, Box<[ModuleId]>>>,
	aliases: RefCell<HashMap<ModuleId, Option<Alias>>>,
	typedefs: RefCell<HashMap<ModuleId, Type>>,
	procs: RefCell<HashMap<ModuleId, Proc>>,
	external: RefCell<HashMap<ModuleId, External>>,

	// Error reporting
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

	fn register_array_type(&self, t: ArrayType) -> ArrayTypeId {
		let id = ArrayTypeId(self.arrays.borrow().len());
		self.arrays.borrow_mut().push(t);
		id
	}

	fn get_array_type(&self, index: ArrayTypeId) -> Ref<'_, ArrayType> {
		Ref::map(self.arrays.borrow(), |r| &r[index.0])
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

#[derive(Clone, Debug)]
pub struct ArrayType {
	dimensions: (usize, usize),
	inner: Type,
}

#[derive(Debug, Clone, Copy)]
pub enum Type {
	Primitive(PrimitiveType),
	Struct(StructId),
	Array(ArrayTypeId),
	Proc(ModuleId), // Procedures can only be introduced at the top level
}

impl Default for Type {
	fn default() -> Self {
		Self::Primitive(Default::default())
	}
}

#[derive(Clone, Debug)]
pub struct Alias {
	/// Keeps track of the jumps taken around the codebase in order to resolve the
	/// alias. This is only used for error reporting (in particular, it's useful
	/// for showing every alias followed while elaborating a cyclic definition)
	jumps: Box<[ModuleId]>,
	endpoint: ModuleId,
}

#[derive(Clone, Debug)]
pub struct External {
	ty: Type,
	value: crate::cst::ExternalValue,
	// TODO: I might include things like layout information here
}

#[derive(Clone, Debug)]
pub struct Struct {
	fields: Box<[PropId]>,
}

#[derive(Clone, Debug)]
pub struct Proc {
	arguments: Box<[Type]>,
	output: Type,
	implementation: ProcImplementation,
}

#[derive(Clone, Debug)]
pub enum ProcImplementation {
	Native(String),
	Implemented(Box<[BinderId]>, Block),
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

	fn elab_import(&self, module: ModuleId) -> Ref<'_, [ModuleId]> {
		if let Ok(cached) = Ref::filter_map(self.imports.borrow(), |imports| {
			imports.get(&module).map(|v| &**v)
		}) {
			return cached;
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

		if boxed.is_empty() {
			self.report_name_not_found(path);
		}

		self.imports.borrow_mut().insert(module, boxed);
		self.elab_import(module)
	}

	fn resolve_name_internally(
		&self,
		module: ModuleId,
		name: &Name,
	) -> ModuleResolution {
		let locally: ModuleResolution = match &self.lowering_context[module] {
			crate::lowering::Module::Toplevel(_) => Default::default(),
			crate::lowering::Module::Import(_, inner) => self
				.elab_import(module)
				.iter()
				.copied()
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
				write!(&mut out, "/").unwrap();
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
	// {{{ Elaborate top-level entries
	pub fn elab_toplevel(&self) {
		for module in self.lowering_context.module_ids() {
			match &self.lowering_context[module] {
				crate::lowering::Module::Import(_, _) => {
					self.elab_import(module);
				}
				crate::lowering::Module::Toplevel(
					crate::lowering::ModuleMember::Type(ty),
				) => {
					self.elab_toplevel_type(&mut Telescope::new(module), ty);
				}
				crate::lowering::Module::Toplevel(
					crate::lowering::ModuleMember::Alias(to),
				) => {
					self.elab_toplevel_alias(&mut Telescope::new(module), to);
				}
				crate::lowering::Module::Toplevel(
					crate::lowering::ModuleMember::External(_, _),
				) => {
					self.elab_external(&mut Telescope::new(module));
				}
				_ => {}
			}
		}
	}
	// }}}
	// {{{ Elaborate name aliases
	fn elab_toplevel_alias(
		&self,
		telescope: &mut Telescope<ModuleId>,
		to: &crate::lowering::QualifiedIdentifier,
	) -> Option<Ref<'_, Alias>> {
		if let Some(cycle) = telescope.find_cycle() {
			self.report_cycle(cycle);
			return None;
		} else if let Ok(cached) =
			Ref::filter_map(self.aliases.borrow(), |aliases| {
				aliases.get(telescope.tip())
			}) {
			return Ref::filter_map(cached, |v| v.as_ref()).ok();
		}

		let elaborated = self.elab_alias(telescope, to);
		self.aliases.borrow_mut().insert(*telescope.tip(), elaborated);
		self.elab_toplevel_alias(telescope, to)
	}

	/// Resolves an alias pointing to a toplevel entry. This can be considered
	/// a wrapper around [Self::resolve_path_internally] which:
	/// - errors out on ambiguous names (i.e. cases where more than one match
	///   exists)
	/// - errors out on names that are not found
	/// - follows jumps recursively until the result is a non-alias
	/// - detects alias cycles
	fn elab_alias(
		&self,
		telescope: &mut Telescope<ModuleId>,
		to: &crate::lowering::QualifiedIdentifier,
	) -> Option<Alias> {
		let ids: ModuleResolution = self
			.resolve_path_internally(*telescope.tip(), &to.0)
			.into_iter()
			.filter(|n| {
				matches!(
					&self.lowering_context[*n],
					crate::lowering::Module::Toplevel(_)
				)
			})
			.collect();

		if ids.is_empty() {
			self.report_name_not_found(to);
			None
		} else if ids.len() > 1 {
			self.report_ambiguous_name(to, &ids);
			None
		} else {
			let the_module = *ids.iter().next().unwrap();
			match &self.lowering_context[the_module] {
				crate::lowering::Module::Toplevel(
					crate::lowering::ModuleMember::Alias(to),
				) => telescope.focusing(the_module, |telescope| {
					self.elab_toplevel_alias(telescope, to).map(|alias| {
						// h
						Alias {
							jumps: std::iter::once(the_module)
								.chain(alias.jumps.iter().copied())
								.collect(),
							endpoint: alias.endpoint,
						}
					})
				}),
				_ => Some(Alias {
					jumps: vec![the_module].into_boxed_slice(),
					endpoint: the_module,
				}),
			}
		}
	}
	// }}}
	// {{{ Elaborate types
	fn elab_toplevel_type(
		&self,
		// The types we were checking while we got here
		telescope: &mut Telescope<ModuleId>,
		ty: &crate::lowering::Type,
	) -> Type {
		if let Some(cycle) = telescope.find_cycle() {
			self.report_cycle(cycle);
			Type::default()
		} else if let Some(&elaborated) =
			self.typedefs.borrow().get(telescope.tip())
		{
			elaborated
		} else {
			let elaborated = self.elab_type(telescope, ty);
			self.typedefs.borrow_mut().insert(*telescope.tip(), elaborated);
			elaborated
		}
	}

	fn elab_type(
		&self,
		telescope: &mut Telescope<ModuleId>,
		ty: &crate::lowering::Type,
	) -> Type {
		match ty {
			crate::lowering::Type::Unknown => {
				Type::Primitive(PrimitiveType::Unknown)
			}
			crate::lowering::Type::Unit => Type::Primitive(PrimitiveType::Unit),
			crate::lowering::Type::Shared(inner) => {
				self.elab_type(telescope, inner)
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

				if let Some(resolved) = self.elab_alias(telescope, name) {
					let crate::lowering::Module::Toplevel(toplevel) =
						&self.lowering_context[resolved.endpoint]
					else {
						unreachable!()
					};

					match toplevel {
						crate::lowering::ModuleMember::Type(ty) => telescope
							.focusing_many(resolved.jumps, |telescope| {
								self.elab_toplevel_type(telescope, ty)
							}),
						crate::lowering::ModuleMember::Unknown => {
							Type::default()
						}
						_ => {
							self.report_not_a_type(name, resolved.endpoint);
							Type::default()
						}
					}
				} else {
					Type::default()
				}
			}
			crate::lowering::Type::Array(dims, inner) => {
				let inner = self.elab_type(telescope, inner);

				// TODO: error out on certain sizes
				Type::Array(self.register_array_type(ArrayType {
					dimensions: *dims,
					inner,
				}))
			}
			crate::lowering::Type::Struct(struct_id) => {
				// TODO: elaborate the struct' fields
				Type::Struct(*struct_id)
			}
		}
	}
	// }}}
	// {{{ Elaborate name aliases
	fn elab_external(
		&self,
		telescope: &mut Telescope<ModuleId>,
	) -> Ref<'_, External> {
		if let Ok(cached) =
			Ref::filter_map(self.external.borrow(), |external| {
				external.get(telescope.tip())
			}) {
			return cached;
		}

		let crate::lowering::Module::Toplevel(
			crate::lowering::ModuleMember::External(value, ty),
		) = &self.lowering_context[*telescope.tip()]
		else {
			panic!("Expected module to be an external value declaration")
		};

		let elaborated =
			External { ty: self.elab_type(telescope, ty), value: *value };
		self.external.borrow_mut().insert(*telescope.tip(), elaborated);
		self.elab_external(telescope)
	}
	// }}}

	// Error reporting
	// {{{ Name not found
	fn report_name_not_found(
		&self,
		name: &crate::lowering::QualifiedIdentifier,
	) {
		compact_report!(
			(self, "NameNotFound", name),
			("Name \"{}\" not defined", name),
			("I could not resolve this name"),
		);
	}
	// }}}
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
