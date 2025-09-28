use crate::types::*;
use anyhow::Context;
use anyhow::anyhow;
use anyhow::bail;
use glsl::visitor::Host;

// {{{ Resolve includes
struct IncludeVisitor<'a> {
	state: &'a mut State,
	id: GlslFileId,
	err: Option<anyhow::Error>,
}

impl IncludeVisitor<'_> {
	fn try_resolve_include(
		&mut self,
		include: &glsl::syntax::PreprocessorInclude,
	) -> anyhow::Result<()> {
		let mut overall = match &include.path {
			glsl::syntax::Path::Absolute(p) => self.state.base_path.join(p),
			glsl::syntax::Path::Relative(p) => self.state[self.id].path.parent().unwrap().join(p),
		};

		overall.set_extension("glsl");

		let overall = overall
			.canonicalize()
			.with_context(|| format!("While canonicalizing path {:?}", overall))?;
		let referenced = self
			.state
			.files
			.iter()
			.find_map(|f| if f.path == overall { Some(f.id) } else { None })
			.ok_or_else(|| anyhow!("Cannot find file matching path {:?}", overall))?;
		self.state[self.id].includes.push(referenced);
		self.state[referenced].included_by.push(self.id);
		Ok(())
	}
}

impl glsl::visitor::Visitor for IncludeVisitor<'_> {
	fn visit_preprocessor_include(
		&mut self,
		include: &glsl::syntax::PreprocessorInclude,
	) -> glsl::visitor::Visit {
		if self.err.is_some() {
			return glsl::visitor::Visit::Parent;
		}

		let res = self.try_resolve_include(include);
		if let Err(err) = res {
			self.err = Some(err);
			return glsl::visitor::Visit::Parent;
		}

		glsl::visitor::Visit::Children
	}
}

impl State {
	pub fn resolve_includes(&mut self) -> anyhow::Result<()> {
		for f in self.files.clone() {
			let mut visitor = IncludeVisitor {
				state: self,
				id: f.id,
				err: None,
			};

			f.syntax.visit(&mut visitor);
			if let Some(err) = visitor.err {
				bail!(err);
			};
		}

		// Propagate includes until no more changes are seen.
		// This is an inefficient implementation, but I do not care.
		let mut keep_going = true;
		while keep_going {
			keep_going = false;
			let cloned = self.clone();
			for f in &mut self.files {
				for include in &f.includes.clone() {
					let other = &cloned[*include];
					for other_dep in &other.includes {
						if !f.includes.contains(other_dep) {
							f.includes.push(*other_dep);
							keep_going = true;
						}
					}
				}
			}
		}

		Ok(())
	}
}
// }}}
// {{{ Check whether a file declares some function
impl State {
	pub fn declares_function_shallow(
		&self,
		id: GlslFileId,
		name: &str,
	) -> Option<GlslUsedFunctionId> {
		for f in &self[id].used_functions {
			if self[f.decl_id].def.prototype.name.0 == name {
				return Some(f.id);
			}
		}

		None
	}

	pub fn declares_function(&self, id: GlslFileId, name: &str) -> Option<GlslUsedFunctionId> {
		self.declares_function_shallow(id, name).or(self[id]
			.includes
			.iter()
			.find_map(|f| self.declares_function_shallow(*f, name)))
	}

	pub fn detect_stages(&mut self) {
		let cloned = self.clone();
		for f in &mut self.files {
			f.vert_main = cloned.declares_function(f.id, "vert");
			f.frag_main = cloned.declares_function(f.id, "frag");
		}
	}
}
// }}}

// {{{ Propagate references
#[derive(Default)]
struct IdentifierVisitor {
	identifiers: Vec<glsl::syntax::Identifier>,
}

impl glsl::visitor::Visitor for IdentifierVisitor {
	fn visit_identifier(&mut self, identifier: &glsl::syntax::Identifier) -> glsl::visitor::Visit {
		self.identifiers.push(identifier.clone());
		glsl::visitor::Visit::Parent
	}
}

enum Identifier {
	Function(GlslFunctionId),
	Uniform(GlslUniformId),
	Attrib(GlslAttribId),
	Varying(GlslVaryingId),
}

impl State {
	pub fn find_references(&mut self) {
		let all_functions: Vec<_> = self
			.files
			.iter()
			.flat_map(|file| file.declared_functions.iter().map(|f| f.id))
			.collect();

		for id in all_functions {
			let mut visitor = IdentifierVisitor::default();
			self[id].def.statement.visit(&mut visitor);

			for identifier in &visitor.identifiers {
				if let Some(uniform) = self[id.0]
					.used_uniforms
					.iter()
					.find(|u| self[u.id].decl.name.as_ref().unwrap() == identifier)
					.map(|u| u.id)
				{
					if !self[id].references_uniforms.contains(&uniform) {
						self[id].references_uniforms.push(uniform);
					}
				} else if let Some(attrib) = self[id.0]
					.used_attribs
					.iter()
					.find(|u| self[u.id].decl.name.as_ref().unwrap() == identifier)
					.map(|u| u.id)
				{
					if !self[id].references_attribs.contains(&attrib) {
						self[id].references_attribs.push(attrib);
					}
				} else if let Some(varying) = self[id.0]
					.used_varyings
					.iter()
					.find(|u| self[u.id].decl.name.as_ref().unwrap() == identifier)
					.map(|u| u.id)
				{
					if !self[id].references_varyings.contains(&varying) {
						self[id].references_varyings.push(varying);
					}
				} else if let Some(func) = self[id.0]
					.used_functions
					.iter()
					.find(|u| self[u.decl_id].def.prototype.name == *identifier)
					.map(|u| u.decl_id)
				{
					if !self[id].references_functions.contains(&func) {
						self[id].references_functions.push(func);
					}
				} else {
					// Variable not found, continue. Likely a local.
					continue;
				}
			}
		}

		// Propagate references until no more changes are seen.
		// This is an inefficient implementation, but I do not care.
		let mut keep_going = true;
		while keep_going {
			keep_going = false;
			let cloned = self.clone();
			for file in &cloned.files {
				for func in &file.declared_functions {
					for &other_func in &func.references_functions {
						for &other_dep in &cloned[other_func].references_functions {
							if !func.references_functions.contains(&other_dep) {
								self[func.id].references_functions.push(other_dep);
								keep_going = true;
							}
						}

						for &other_dep in &cloned[other_func].references_varyings {
							if !func.references_varyings.contains(&other_dep) {
								self[func.id].references_varyings.push(other_dep);
								keep_going = true;
							}
						}

						for &other_dep in &cloned[other_func].references_attribs {
							if !func.references_attribs.contains(&other_dep) {
								self[func.id].references_attribs.push(other_dep);
								keep_going = true;
							}
						}

						for &other_dep in &cloned[other_func].references_uniforms {
							if !func.references_uniforms.contains(&other_dep) {
								self[func.id].references_uniforms.push(other_dep);
								keep_going = true;
							}
						}
					}
				}
			}
		}
	}
}
// }}}
