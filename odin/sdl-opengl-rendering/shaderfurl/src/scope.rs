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
	pub fn declares_function_shallow(&self, id: GlslFileId, name: &str) -> bool {
		for v in &self[id].syntax {
			if let glsl::syntax::ExternalDeclaration::FunctionDefinition(fn_def) = v
				&& fn_def.prototype.name.0 == name
			{
				return true;
			}
		}

		false
	}

	pub fn declares_function(&self, id: GlslFileId, name: &str) -> bool {
		self.declares_function_shallow(id, name)
			|| self[id]
				.includes
				.iter()
				.any(|f| self.declares_function_shallow(*f, name))
	}

	pub fn detect_stages(&mut self) {
		let cloned = self.clone();
		for f in &mut self.files {
			f.has_vert = cloned.declares_function(f.id, "vert");
			f.has_frag = cloned.declares_function(f.id, "frag");
		}
	}
}
// }}}
