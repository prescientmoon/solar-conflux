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
	lowering::{self, Identifier, LoweringContext, ModuleId, Name, StructId},
};

/*
TODO:
- [x] Elaborate aliases
- [x] Elaborate type definitions
- [x] Elaborate structs
- [x] Elaborate proc types
- [ ] Elaborate statements
- [ ] Elaborate expressions
*/

// {{{ The elaboration context type
#[derive(Clone, Copy, Debug)]
pub struct BinderId(usize);

#[derive(Clone, Copy, Debug)]
pub struct PropId(usize);

#[derive(Clone, Copy, Debug)]
pub struct TypeId(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ProcId(ModuleId);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ExternalId(ModuleId);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct StatementId(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BlockId(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ExprId(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SpineId(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TypeTelescopeId(usize);

#[derive(Default)]
pub struct ElabContext<'a> {
	pub lowering_context: LoweringContext,

	// Type fragments
	/// Indexed by [TypeId]
	types: RefCell<Vec<Type>>,
	/// Indexed by [LocalId]
	locals: RefCell<Vec<Local>>,
	/// Indexed by [FlexId]
	flex_solutions: RefCell<Vec<Option<Type>>>,
	/// Indexed by [PropId]
	props: RefCell<Vec<Prop>>,
	/// Indexed by [StatementId]
	statements: RefCell<Vec<Statement>>,
	/// Indexed by [BlockId]
	blocks: RefCell<Vec<Block>>,
	/// Indexed by [ExprId]
	exprs: RefCell<Vec<Expr>>,
	/// Indexed by [SpineId]
	spines: RefCell<Vec<Spine>>,
	/// Indexed by [TypeTelescopeId]
	type_telescopes: RefCell<Vec<TypeTelescope>>,

	// Top-level elaboration memorization
	imports: RefCell<HashMap<ModuleId, Box<[ModuleId]>>>,
	aliases: RefCell<HashMap<ModuleId, Option<Alias>>>,
	typedefs: RefCell<HashMap<ModuleId, Type>>,
	procs: RefCell<HashMap<ProcId, Proc>>,
	external: RefCell<HashMap<ExternalId, External>>,
	structs: RefCell<HashMap<StructId, Struct>>,

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

	fn register_type(&self, t: Type) -> TypeId {
		let id = TypeId(self.types.borrow().len());
		self.types.borrow_mut().push(t);
		id
	}

	fn get_array_type(&self, index: TypeId) -> Type {
		self.types.borrow()[index.0]
	}

	fn regsiter_local(&self, identifier: Identifier, ty: Type) -> LocalId {
		let id = LocalId(self.locals.borrow().len());
		self.locals.borrow_mut().push(Local { ty, identifier });
		id
	}

	fn get_local(&self, id: LocalId) -> Ref<'_, Local> {
		Ref::map(self.locals.borrow(), |r| &r[id.0])
	}

	fn get_external(&self, id: ExternalId) -> Ref<'_, External> {
		Ref::map(self.external.borrow(), |e| e.get(&id).unwrap())
	}

	fn get_proc(&self, id: ProcId) -> Ref<'_, Proc> {
		Ref::map(self.procs.borrow(), |p| p.get(&id).unwrap())
	}

	fn register_flex(&self) -> Type {
		let id = FlexId(self.locals.borrow().len());
		self.flex_solutions.borrow_mut().push(None);
		Type::Flex(id)
	}

	fn register_prop(&self, name: Identifier, ty: Type) -> PropId {
		let id = PropId(self.props.borrow().len());
		self.props.borrow_mut().push(Prop { ty, name });
		id
	}

	fn get_prop(&self, id: PropId) -> Ref<'_, Prop> {
		Ref::map(self.props.borrow(), |r| &r[id.0])
	}

	fn register_statement(&self, statement: Statement) -> StatementId {
		let id = StatementId(self.statements.borrow().len());
		self.statements.borrow_mut().push(statement);
		id
	}

	fn get_statement(&self, id: StatementId) -> Statement {
		self.statements.borrow()[id.0]
	}

	fn register_block(&self, block: Block) -> BlockId {
		let id = BlockId(self.blocks.borrow().len());
		self.blocks.borrow_mut().push(block);
		id
	}

	fn get_block(&self, id: BlockId) -> Ref<'_, Block> {
		Ref::map(self.blocks.borrow(), |r| &r[id.0])
	}

	fn register_expr(&self, expr: Expr) -> ExprId {
		let id = ExprId(self.exprs.borrow().len());
		self.exprs.borrow_mut().push(expr);
		id
	}

	fn get_expr(&self, id: ExprId) -> Expr {
		self.exprs.borrow()[id.0]
	}

	fn register_spine(&self, spine: Spine) -> SpineId {
		let id = SpineId(self.spines.borrow().len());
		self.spines.borrow_mut().push(spine);
		id
	}

	fn get_spine(&self, id: SpineId) -> Ref<'_, Spine> {
		Ref::map(self.spines.borrow(), |r| &r[id.0])
	}

	fn register_type_telescope(&self, spine: TypeTelescope) -> TypeTelescopeId {
		let id = TypeTelescopeId(self.type_telescopes.borrow().len());
		self.type_telescopes.borrow_mut().push(spine);
		id
	}

	fn get_type_telescope(
		&self,
		id: TypeTelescopeId,
	) -> Ref<'_, TypeTelescope> {
		Ref::map(self.type_telescopes.borrow(), |r| &r[id.0])
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
	U32,
	U64,
	Bool,
}

#[derive(Clone, Debug)]
pub struct ArrayType {
	dimensions: (usize, usize),
	inner: Type,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FlexId(usize);

#[derive(Debug, Clone, Copy)]
pub enum Type {
	/// Unification variable
	Flex(FlexId),
	Primitive(PrimitiveType),
	Struct(StructId),
	Array((usize, usize), TypeId),
	Proc(TypeTelescopeId, TypeId),
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
pub struct Prop {
	ty: Type,
	name: Identifier,
}

#[derive(Clone, Debug)]
pub struct Proc {
	arguments: TypeTelescopeId,
	output: Type,
	implementation: ProcImplementation,
}

#[derive(Clone, Debug)]
pub enum ProcImplementation {
	Native(String),
	Implemented(Box<[LocalId]>, Statement),
}

#[derive(Clone, Copy, Debug)]
pub enum Expr {
	// Base values
	Unit,
	Bool(bool),
	Int(i64, Type),
	Float(f64, Type),
	Hole(Type),
	Local(LocalId),
	External(ExternalId),
	Indirect(ExprId),

	// Upcasts
	ScalarDiagonalUpcast(usize, ExprId),
	ScalarVectorUpcast(usize, ExprId),

	// Compound values
	Property(ExprId, PropId),
	Call(ProcId, SpineId),
	Unary(UnaryOperator, ExprId),
	Ternary(ExprId, ExprId, ExprId),
	/// We include the output type as computing it again every time would be a bit
	/// annoying (I would have to re-implement some of the operator typing logic
	/// in [ElabContext::type_of].
	Binary(ExprId, BinaryOperator, ExprId, Type),
}

#[derive(Clone, Copy, Debug)]
pub enum Statement {
	Unknown,
	Discard,
	Break,
	Continue,
	Return(Expr),
	Expression(Expr),
	Block(BlockId),
	If(Expr, StatementId, StatementId),
	For(StatementId, StatementId, StatementId, StatementId),
	Assignment(Expr, Option<BinaryOperator>, Expr),
	Declaration(LocalId, Option<Expr>),
}

#[derive(Clone, Debug, Default)]
pub struct Block(Box<[Statement]>);

#[derive(Clone, Debug, Default)]
pub struct Spine(Box<[Expr]>);

#[derive(Clone, Debug, Default)]
pub struct TypeTelescope(Box<[Type]>);
// }}}
// {{{ Name resolution
type ModuleResolution = HashSet<ModuleId>;
impl ElabContext<'_> {
	fn resolve_name_inwards(
		&self,
		module: ModuleId,
		name: &Name,
	) -> ModuleResolution {
		match &self.lowering_context[module] {
			crate::lowering::Module::Toplevel(_) => Default::default(),
			crate::lowering::Module::Import(_, inner) => {
				self.resolve_name_inwards(*inner, name)
			}
			crate::lowering::Module::Fork(items) => items
				.into_iter()
				.filter(|(mod_name, _)| mod_name.to_name() == Some(name))
				.map(|(_, inner)| *inner)
				.collect(),
		}
	}

	pub fn resolve_path_inwards(
		&self,
		module: ModuleId,
		name: &[Name],
	) -> ModuleResolution {
		match name {
			[] => HashSet::from([module]),
			[head, tail @ ..] => self
				.resolve_name_inwards(module, head)
				.into_iter()
				.flat_map(|res| self.resolve_path_inwards(res, tail))
				.collect(),
		}
	}

	fn resolve_name_outwards_at_parent(
		&self,
		module: ModuleId,
		name: &Name,
	) -> ModuleResolution {
		if let Some(last) = self.lowering_context.module_parent(module) {
			self.resolve_name_outwards(last, name)
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
				let below = self.resolve_name_inwards(*inner, head);
				let above = self.resolve_name_outwards_at_parent(module, head);

				below
					.into_iter()
					.chain(above)
					.flat_map(|res| self.resolve_path_inwards(res, tail))
					.collect()
			}
		};

		if boxed.is_empty() {
			self.report_name_not_found(path);
		}

		self.imports.borrow_mut().insert(module, boxed);
		self.elab_import(module)
	}

	fn resolve_name_outwards(
		&self,
		module: ModuleId,
		name: &Name,
	) -> ModuleResolution {
		let locally: ModuleResolution = match &self.lowering_context[module] {
			crate::lowering::Module::Toplevel(_) => Default::default(),
			crate::lowering::Module::Import(_, _) => self
				.elab_import(module)
				.iter()
				.copied()
				.flat_map(|res| self.resolve_name_inwards(res, name))
				.collect(),
			crate::lowering::Module::Fork(_) => {
				self.resolve_name_inwards(module, name)
			}
		};

		let above = self.resolve_name_outwards_at_parent(module, name);
		locally.into_iter().chain(above).collect()
	}

	pub fn resolve_path_outwards(
		&self,
		module: ModuleId,
		name: &[Name],
	) -> ModuleResolution {
		match name {
			[] => HashSet::from([module]),
			[head, tail @ ..] => self
				.resolve_name_outwards(module, head)
				.into_iter()
				.flat_map(|res| self.resolve_path_inwards(res, tail))
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
					self.elab_toplevel_type(
						&mut ModuleTelescope::new(module),
						ty,
					);
				}
				crate::lowering::Module::Toplevel(
					crate::lowering::ModuleMember::Alias(to),
				) => {
					self.elab_toplevel_alias(
						&mut ModuleTelescope::new(module),
						to,
					);
				}
				crate::lowering::Module::Toplevel(
					crate::lowering::ModuleMember::External(_, _),
				) => {
					self.elab_external(&mut ModuleTelescope::new(module));
				}
				crate::lowering::Module::Toplevel(
					crate::lowering::ModuleMember::Unknown(ty),
				) => {
					if let Some(ty) = ty {
						self.elab_type(&mut ModuleTelescope::new(module), ty);
					}
				}
				crate::lowering::Module::Toplevel(
					crate::lowering::ModuleMember::Proc(_),
				) => {
					self.elab_toplevel_proc(&mut ModuleTelescope::new(module));
				}
				crate::lowering::Module::Fork(_) => {}
			}
		}
	}
	// }}}
	// {{{ Elaborate name aliases
	fn elab_toplevel_alias(
		&self,
		telescope: &mut ModuleTelescope,
		to: &crate::lowering::QualifiedIdentifier,
	) -> Option<Ref<'_, Alias>> {
		if let Some(cycle) = telescope.find_cycle() {
			self.report_cycle(cycle);
			return None;
		} else if let Ok(cached) =
			Ref::filter_map(self.aliases.borrow(), |aliases| {
				aliases.get(&telescope.tip())
			}) {
			return Ref::filter_map(cached, |v| v.as_ref()).ok();
		}

		let elaborated = self.elab_alias(telescope, to);
		self.aliases.borrow_mut().insert(telescope.tip(), elaborated);
		self.elab_toplevel_alias(telescope, to)
	}

	/// Resolves an alias pointing to a toplevel entry. This can be considered
	/// a wrapper around [Self::resolve_path_outwards] which:
	/// - errors out on ambiguous names (i.e. cases where more than one match
	///   exists)
	/// - errors out on names that are not found
	/// - follows jumps recursively until the result is a non-alias
	/// - detects alias cycles
	fn elab_alias(
		&self,
		telescope: &mut ModuleTelescope,
		to: &crate::lowering::QualifiedIdentifier,
	) -> Option<Alias> {
		let ids: ModuleResolution = self
			.resolve_path_outwards(telescope.tip(), &to.0)
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
					self.elab_toplevel_alias(telescope, to).map(|alias| Alias {
						jumps: std::iter::once(the_module)
							.chain(alias.jumps.iter().copied())
							.collect(),
						endpoint: alias.endpoint,
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
	fn elab_struct(
		&self,
		// The types we were checking while we got here
		telescope: &mut ModuleTelescope,
		s: StructId,
	) {
		if !self.structs.borrow().contains_key(&s) {
			let structure = &self.lowering_context[s];
			let mut fields = Vec::new();

			for (name, ty) in &structure.fields {
				let elaborated = self.elab_type(telescope, ty);
				let id = self.register_prop(name.clone(), elaborated);
				fields.push(id)
			}

			// O(N²), but doesn't matter
			for (i, (name, _)) in structure.fields.iter().enumerate() {
				if let Some(name) = name.to_name() {
					let occurrences: Box<[_]> = structure
						.fields
						.iter()
						.enumerate()
						.filter_map(|(j, (current, _))| {
							Some((j, current.to_name()?))
						})
						.filter(|(_, current)| *current == name)
						// We drop everything if any element comes before the current name,
						// since that means we've already reported on this name.
						.take_while(|(j, _)| *j >= i)
						.map(|(_, c)| c)
						.cloned()
						.collect();

					if occurrences.len() > 1 {
						self.report_duplicate_field(&occurrences[..]);
					}
				}
			}

			self.structs
				.borrow_mut()
				.insert(s, Struct { fields: fields.into_boxed_slice() });
		}
	}
	// }}}
	// {{{ Elaborate types
	fn elab_toplevel_type(
		&self,
		// The types we were checking while we got here
		telescope: &mut ModuleTelescope,
		ty: &crate::lowering::Type,
	) -> Type {
		if let Some(cycle) = telescope.find_cycle() {
			self.report_cycle(cycle);
			Type::default()
		} else if let Some(&elaborated) =
			self.typedefs.borrow().get(&telescope.tip())
		{
			elaborated
		} else {
			let elaborated = self.elab_type(telescope, ty);
			self.typedefs.borrow_mut().insert(telescope.tip(), elaborated);
			elaborated
		}
	}

	fn elab_type(
		&self,
		telescope: &mut ModuleTelescope,
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
						"u32" => return Type::Primitive(PrimitiveType::U32),
						"u64" => return Type::Primitive(PrimitiveType::U64),
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
						crate::lowering::ModuleMember::Unknown(None) => {
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
				Type::Array(*dims, self.register_type(inner))
			}
			crate::lowering::Type::Struct(struct_id) => {
				self.elab_struct(telescope, *struct_id);
				Type::Struct(*struct_id)
			}
		}
	}
	// }}}
	// {{{ Elaborate external values
	fn elab_external(
		&self,
		telescope: &mut ModuleTelescope,
	) -> Ref<'_, External> {
		let external_id = ExternalId(telescope.tip());
		if let Ok(cached) =
			Ref::filter_map(self.external.borrow(), |external| {
				external.get(&external_id)
			}) {
			return cached;
		}

		let crate::lowering::Module::Toplevel(
			crate::lowering::ModuleMember::External(value, ty),
		) = &self.lowering_context[telescope.tip()]
		else {
			panic!("Expected module to be an external value declaration")
		};

		let elaborated =
			External { ty: self.elab_type(telescope, ty), value: *value };
		self.external.borrow_mut().insert(external_id, elaborated);
		self.elab_external(telescope)
	}
	// }}}
	// {{{ Elaborate procedures
	fn elab_toplevel_proc(
		&self,
		telescope: &mut ModuleTelescope,
	) -> Ref<'_, Proc> {
		let proc_id = ProcId(telescope.tip());
		if let Ok(cached) =
			Ref::filter_map(self.procs.borrow(), |procs| procs.get(&proc_id))
		{
			return cached;
		}

		let crate::lowering::Module::Toplevel(
			crate::lowering::ModuleMember::Proc(proc),
		) = &self.lowering_context[telescope.tip()]
		else {
			panic!("Expected module to be a procedure")
		};

		let arg_types: Box<[Type]> = proc
			.args
			.iter()
			.map(|(_, arg)| self.elab_type(telescope, arg))
			.collect();

		let implementation = match &proc.body {
			lowering::ProcBody::Native(name) => {
				ProcImplementation::Native(name.clone())
			}
			lowering::ProcBody::Implemented(block) => {
				let locals: Box<[LocalId]> = proc
					.args
					.iter()
					.zip(arg_types.iter().copied())
					.map(|((identifier, _), ty)| {
						self.regsiter_local(identifier.clone(), ty)
					})
					.collect();

				let mut env = LocalEnv::new(telescope.tip());
				for id in &locals {
					self.add_to_env(&mut env, *id);
				}

				ProcImplementation::Implemented(
					locals,
					Statement::Block(
						self.register_block(self.elab_block(&mut env, block)),
					),
				)
			}
		};

		let elaborated = Proc {
			arguments: self.register_type_telescope(TypeTelescope(arg_types)),
			output: self.elab_type(telescope, &proc.ret),
			implementation,
		};

		self.procs.borrow_mut().insert(proc_id, elaborated);
		self.elab_toplevel_proc(telescope)
	}

	fn elab_block(
		&self,
		env: &mut LocalEnv,
		block: &crate::lowering::Block,
	) -> Block {
		self.block(env, |env| {
			for statement in &block.0 {
				self.elab_statement(env, statement);
			}
		})
		.1
	}

	fn elab_statement(
		&self,
		env: &mut LocalEnv,
		statement: &crate::lowering::Statement,
	) {
		match &statement.kind {
			lowering::StatementKind::Unknown => {}
			lowering::StatementKind::Discard => {
				self.emit_statement(env, Statement::Unknown);
				self.emit_span(env, statement.span, true);
			}
			lowering::StatementKind::Break => {
				if env.in_loop {
					self.emit_statement(env, Statement::Break);
				} else {
					compact_report!(
						(self, "InvalidBreak", statement),
						("'break' cannot be used outside loops"),
						("I was expecting this statement to live inside a loop"),
					);
				}

				self.emit_span(env, statement.span, env.in_loop);
			}
			lowering::StatementKind::Continue => {
				if env.in_loop {
					self.emit_statement(env, Statement::Continue);
				} else {
					compact_report!(
						(self, "InvalidContinue", statement),
						("'continue' cannot be used outside loops"),
						("I was expecting this statement to live inside a loop"),
					);
				}

				self.emit_span(env, statement.span, env.in_loop);
			}
			lowering::StatementKind::Return(expr) => {
				let expr = self.elab_expr(env, expr);
				self.emit_statement(env, Statement::Return(expr));
				self.emit_span(env, statement.span, true);
			}
			lowering::StatementKind::Expression(expr) => {
				env.might_backtrack(|env| {
					let expr = self.elab_expr(env, expr);
					// TODO: do a more thorough check.
					// For example, we don't want to allow arithmetic expressions
					// where one of the operands has side effects.
					//
					// In fact, I think we should only really allow calls here...
					if self.expr_has_side_effects(expr) {
						(expr, true)
					} else {
						if let Some(span) = statement.span {
							self.report_no_effect(span);
						}

						(expr, false)
					}
				});

				// NOTE: we could technically do some more analysis here like looking
				// for discards inside function calls, but I am too lazy to do that, and
				// always-diverging calls don't really come up in practice.
				self.emit_span(env, statement.span, false);
			}
			lowering::StatementKind::If(_items) => todo!(),
			lowering::StatementKind::For(_, _block) => todo!(),
			lowering::StatementKind::Assignment(
				_expr,
				_binary_operator,
				_expr1,
			) => {
				self.emit_span(env, statement.span, false);
				todo!()
			}
			lowering::StatementKind::Declaration(
				_qualified_identifier,
				_,
				_expr,
			) => {
				self.emit_span(env, statement.span, false);
				todo!()
			}
		}
	}

	fn elab_expr(
		&self,
		env: &mut LocalEnv,
		expr: &crate::lowering::Expr,
	) -> Expr {
		match expr {
			lowering::Expr::Unknown => Expr::Hole(self.register_flex()),
			lowering::Expr::Unit => Expr::Unit,
			lowering::Expr::Bool(b) => Expr::Bool(*b),
			lowering::Expr::Int(i) => {
				let ty = self.register_flex();
				env.type_in(
					ty,
					vec![
						Type::Primitive(PrimitiveType::U32),
						Type::Primitive(PrimitiveType::U64),
						Type::Primitive(PrimitiveType::I32),
						Type::Primitive(PrimitiveType::I64),
						Type::Primitive(PrimitiveType::F32),
						Type::Primitive(PrimitiveType::F64),
					],
				);
				Expr::Int(*i, ty)
			}
			lowering::Expr::Float(f) => {
				let ty = self.register_flex();
				env.type_in(
					ty,
					vec![
						Type::Primitive(PrimitiveType::F32),
						Type::Primitive(PrimitiveType::F64),
					],
				);
				Expr::Float(*f, ty)
			}
			lowering::Expr::Variable(qualified_identifier) => {
				let path = &qualified_identifier.0[..];
				if let [name] = path
					&& let Some(local) = env.variables.get(name)
				{
					Expr::Local(*local)
				} else {
					let results = self.resolve_path_inwards(env.at, path);
					todo!()
				}
			}
			lowering::Expr::Property(expr, identifier) => {
				let expr = self.elab_expr(env, expr);
				let res = self.register_flex();
				let hole = Expr::Hole(res);
				match identifier {
					Identifier::Unknown => hole,
					Identifier::Name(name) => {
						let out = self.register_expr(hole);
						env.constraints.push(Constraint::HasProp {
							of: expr,
							projection: name.clone(),
							into: out,
						});

						Expr::Indirect(out)
					}
				}
			}
			lowering::Expr::Call(expr, exprs) => {
				let mut spine = Vec::new();
				let mut args = Vec::new();
				for expr in exprs {
					let expr = self.elab_expr(env, expr);
					spine.push(expr);
					args.push(self.type_of(expr));
				}

				let return_type = self.register_flex();
				let proc_type = Type::Proc(
					self.register_type_telescope(TypeTelescope(
						args.into_boxed_slice(),
					)),
					self.register_type(return_type),
				);

				env.type_in(proc_type, todo!());
			}
			lowering::Expr::Unary(unary_operator, expr) => todo!(),
			lowering::Expr::Binary(_) => todo!(),
			lowering::Expr::Ternary(_) => todo!(),
		}
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
	// {{{ No effect
	fn report_no_effect(&self, span: SourceSpan) {
		self.push_report(
			ariadne::Report::build(ariadne::ReportKind::Warning, span)
				.with_code("NoEffect")
				.with_message("Standalone expression has no side effects")
				.with_label(ariadne::Label::new(span).with_message(
					"I expected this expression to perform side effects",
				))
				.finish(),
		);
	}
	// }}}
	// {{{ Field declared multiple times
	fn report_duplicate_field(&self, definitions: &[Name]) {
		assert!(!definitions.is_empty());

		let mut report = ariadne::Report::build(
			ariadne::ReportKind::Error,
			definitions.last().span_of(),
		)
		.with_code("DuplicateField")
		.with_message(format!(
			"Field {} declared multiple times",
			definitions.last().unwrap()
		));

		for (i, name) in definitions.iter().enumerate() {
			report = report.with_label(
				ariadne::Label::new(name.span_of())
					.with_message(format!(
						"This is {} definition of {}",
						if i > 0 { "another" } else { "the first" },
						definitions.last().unwrap()
					))
					.with_order(i as i32),
			)
		}

		self.push_report(report.finish());
	}
	// }}}
}

// {{{ Locals
#[derive(Clone, Copy, Debug)]
pub struct LocalId(usize);

struct Local {
	ty: Type,
	identifier: Identifier,
}

enum Constraint {
	Unification(Type, Type),
	OneOf(Type, Box<[Type]>),
	HasProp {
		/// The value being projected. We keep the expression around because we are
		/// going to insert the projection into the AST once the constraint gets
		/// solved.
		of: Expr,
		projection: Name,
		/// When solved, the result will get inserted here
		into: ExprId,
	},
}

struct LocalEnv {
	/// The ambient module we resolve non-local names to
	at: ModuleId,

	variables: HashMap<Name, LocalId>,
	constraints: Vec<Constraint>,
	statements: Vec<Statement>,

	/// Signifies whether constructs like `break` or `continue` can be used.
	in_loop: bool,

	/// Contains the location where execution diverged, if such a location has
	/// been encountered.
	diverged_at: Option<SourceSpan>,

	/// Used for error handling. Contains the span of code that lives after the
	/// block diverged.
	diverging_span: Option<SourceSpan>,
}

impl LocalEnv {
	fn new(at: ModuleId) -> Self {
		Self {
			at,
			variables: Default::default(),
			constraints: Default::default(),
			statements: Default::default(),
			in_loop: Default::default(),
			diverged_at: Default::default(),
			diverging_span: Default::default(),
		}
	}

	fn might_backtrack<O>(
		&mut self,
		computation: impl FnOnce(&mut Self) -> (O, bool),
	) -> O {
		let statements = self.statements.len();
		let diverged_at = self.diverged_at;
		let diverging_span = self.diverging_span;
		let (result, should_keep) = computation(self);
		if !should_keep {
			self.statements.truncate(statements);
			self.diverged_at = diverged_at;
			self.diverging_span = diverging_span;
		}

		result
	}

	fn type_in(&mut self, ty: Type, one_of: Vec<Type>) {
		self.constraints.push(Constraint::OneOf(ty, one_of.into_boxed_slice()));
	}
}

impl ElabContext<'_> {
	fn add_to_env(&self, env: &mut LocalEnv, id: LocalId) {
		let local = self.get_local(id);
		if let Identifier::Name(name) = &local.identifier {
			env.variables.insert(name.clone(), id);
		}
	}

	fn emit_statement(&self, env: &mut LocalEnv, statement: Statement) {
		if env.diverged_at.is_none() {
			env.statements.push(statement);
		}
	}

	fn emit_span(
		&self,
		env: &mut LocalEnv,
		span: Option<SourceSpan>,
		diverged: bool,
	) {
		if env.diverged_at.is_some() {
			env.diverging_span = (env.diverging_span, span).try_span_of();
		} else if diverged && env.diverged_at.is_none() {
			env.diverged_at = span;
		}
	}

	fn block<O>(
		&self,
		env: &mut LocalEnv,
		computation: impl FnOnce(&mut LocalEnv) -> O,
	) -> (O, Block) {
		let statements = std::mem::take(&mut env.statements);
		let diverged_at = std::mem::take(&mut env.diverged_at);
		let diverging_span = std::mem::take(&mut env.diverging_span);
		let result = computation(env);

		if let Some(at) = env.diverged_at
			&& let Some(span) = env.diverging_span
			&& diverged_at.is_none()
		{
			self.push_report(
				ariadne::Report::build(ariadne::ReportKind::Warning, span)
					.with_code("UnreachableCode")
					.with_message("Code encountered after diverging statement")
					.with_label(
						ariadne::Label::new(at)
							.with_message("This is where execution diverges"),
					)
					.with_label(ariadne::Label::new(span).with_message(
						"These statements will never get reached",
					))
					.finish(),
			);
		}

		env.diverged_at = diverged_at;
		env.diverging_span = diverging_span;

		let block = Block(
			std::mem::replace(&mut env.statements, statements)
				.into_boxed_slice(),
		);

		(result, block)
	}
}
// }}}
// {{{ Side effect tracking
impl ElabContext<'_> {
	fn expr_has_side_effects(&self, expr: Expr) -> bool {
		match expr {
			Expr::Unit => false,
			Expr::Bool(_) => false,
			Expr::Int(_, _) => false,
			Expr::Float(_, _) => false,
			Expr::Hole(_) => false,
			Expr::Local(_) => false,
			Expr::External(_) => false,
			Expr::Indirect(id) => self.expr_has_side_effects(self.get_expr(id)),
			Expr::Property(id, _) => {
				self.expr_has_side_effects(self.get_expr(id))
			}
			Expr::Call(proc_id, spine) => {
				for expr in &self.get_spine(spine).0 {
					if self.expr_has_side_effects(*expr) {
						return true;
					}
				}

				self.proc_has_side_effects(proc_id)
			}
			Expr::Unary(_, id) => self.expr_has_side_effects(self.get_expr(id)),
			Expr::Binary(lhs, _, rhs, _) => {
				self.expr_has_side_effects(self.get_expr(lhs))
					|| self.expr_has_side_effects(self.get_expr(rhs))
			}
			Expr::Ternary(cond, if_true, if_false) => {
				self.expr_has_side_effects(self.get_expr(cond))
					|| self.expr_has_side_effects(self.get_expr(if_true))
					|| self.expr_has_side_effects(self.get_expr(if_false))
			}
			Expr::ScalarVectorUpcast(_, id) => {
				self.expr_has_side_effects(self.get_expr(id))
			}
			Expr::ScalarDiagonalUpcast(_, id) => {
				self.expr_has_side_effects(self.get_expr(id))
			}
		}
	}

	fn proc_has_side_effects(&self, id: ProcId) -> bool {
		let proc = self.get_proc(id);
		if let ProcImplementation::Implemented(_, statement) =
			&proc.implementation
		{
			self.statement_has_side_effects(*statement)
		} else {
			false
		}
	}

	fn block_has_side_effects(&self, block: &Block) -> bool {
		block
			.0
			.iter()
			.any(|statement| self.statement_has_side_effects(*statement))
	}

	// NOTE: this marks things like break or discard statements as "pure", since
	// they cannot leak outside the current scope
	fn statement_has_side_effects(&self, statement: Statement) -> bool {
		match statement {
			Statement::Unknown => false,
			Statement::Discard => true,
			Statement::Break => false,
			Statement::Continue => false,
			Statement::Return(expr) => self.expr_has_side_effects(expr),
			Statement::Expression(expr) => self.expr_has_side_effects(expr),
			Statement::Block(id) => {
				let block = self.get_block(id);

				block.0.iter().any(|statement| {
					self.statement_has_side_effects(*statement)
				})
			}
			Statement::If(cond, if_true, if_false) => {
				self.expr_has_side_effects(cond)
					|| self
						.statement_has_side_effects(self.get_statement(if_true))
					|| self.statement_has_side_effects(
						self.get_statement(if_false),
					)
			}
			Statement::For(start, cond, end, inner) => {
				self.statement_has_side_effects(self.get_statement(start))
					|| self.statement_has_side_effects(self.get_statement(cond))
					|| self.statement_has_side_effects(self.get_statement(end))
					|| self
						.statement_has_side_effects(self.get_statement(inner))
			}
			Statement::Assignment(left, _, right) => {
				self.write_to_expr_has_side_effects(left)
					|| self.expr_has_side_effects(right)
			}
			Statement::Declaration(_, expr) => {
				expr.is_some_and(|expr| self.expr_has_side_effects(expr))
			}
		}
	}

	/// Computes whether writing to the result of a given expression constitutes
	/// a side effect.
	#[allow(clippy::only_used_in_recursion)]
	fn write_to_expr_has_side_effects(&self, expr: Expr) -> bool {
		match expr {
			Expr::External(_) => true,
			Expr::Property(id, _) => {
				self.write_to_expr_has_side_effects(self.get_expr(id))
			}
			_ => false,
		}
	}
}
// }}}
// {{{ Type-checking
impl ElabContext<'_> {
	/// Computes the type of an elaborated expression. This operation does no
	/// unification of advanced checking, and assumes everything checks out
	/// already.
	fn type_of(&self, expr: Expr) -> Type {
		match expr {
			Expr::Unit => Type::Primitive(PrimitiveType::Unit),
			Expr::Bool(_) => Type::Primitive(PrimitiveType::Bool),
			Expr::Int(_, ty) => ty,
			Expr::Float(_, ty) => ty,
			Expr::Hole(ty) => ty,
			Expr::Local(id) => self.get_local(id).ty,
			Expr::External(id) => self.get_external(id).ty,
			Expr::Indirect(id) => self.type_of(self.get_expr(id)),
			Expr::Property(_, prop_id) => self.get_prop(prop_id).ty,
			Expr::Call(id, _) => self.get_proc(id).output,
			// NOTE: unary operators are type-preserving
			Expr::Unary(_, id) => self.type_of(self.get_expr(id)),
			Expr::Binary(_, _, _, ty) => ty,
			Expr::Ternary(_, if_true, _) => {
				self.type_of(self.get_expr(if_true))
			}
			Expr::ScalarVectorUpcast(dim, expr) => {
				Type::Array(self.register_array_type(ArrayType {
					dimensions: (dim, 1),
					inner: self.type_of(self.get_expr(expr)),
				}))
			}
			Expr::ScalarDiagonalUpcast(dim, expr) => {
				Type::Array(self.register_array_type(ArrayType {
					dimensions: (dim, dim),
					inner: self.type_of(self.get_expr(expr)),
				}))
			}
		}
	}
}
// }}}

// {{{ Module telescopes
#[derive(Debug, Clone)]
pub struct ModuleTelescope {
	elements: Vec<ModuleId>,
}

impl ModuleTelescope {
	pub fn new(id: ModuleId) -> Self {
		Self { elements: vec![id] }
	}

	pub fn tip(&self) -> ModuleId {
		*self.elements.last().unwrap()
	}

	pub fn focusing<O>(
		&mut self,
		element: ModuleId,
		compute: impl FnOnce(&mut Self) -> O,
	) -> O {
		self.elements.push(element);
		let output = compute(self);
		self.elements.pop();
		output
	}

	pub fn focusing_many<O>(
		&mut self,
		elements: impl IntoIterator<Item = ModuleId>,
		compute: impl FnOnce(&mut Self) -> O,
	) -> O {
		self.elements.extend(elements);
		let output = compute(self);
		self.elements.pop();
		output
	}

	/// Computes whether the telescope contains a closed cycle. If such a cycle
	/// exists, we could never escape it, thus it is guaranteed to be at the end
	/// of the telescope.
	pub fn find_cycle(&self) -> Option<&[ModuleId]> {
		let tip = self.tip();
		let rest = &self.elements[..self.elements.len() - 1];
		for (i, element) in rest.iter().enumerate().rev() {
			if *element == tip {
				return Some(&rest[i..]);
			}
		}

		None
	}
}
// }}}
