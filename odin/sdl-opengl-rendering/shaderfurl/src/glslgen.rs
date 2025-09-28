use std::fmt::Write;

use anyhow::anyhow;

use crate::types::*;

// {{{ Codegen
pub enum ShaderStage {
	Vert,
	Frag,
}

impl State {
	pub fn gen_glsl_shader(
		&self,
		id: GlslFileId,
		stage: ShaderStage,
		out: &mut String,
	) -> anyhow::Result<()> {
		let file = &self[id];

		writeln!(out, "#version 430")?;

		// {{{ Uniforms
		if !file.used_uniforms.is_empty() {
			writeln!(out)?;
			writeln!(out, "// Uniforms")?;
		}

		for used in &file.used_uniforms {
			let uniform = &self[used.id];
			let mut decl = uniform.decl.clone();

			// Add the position qualifier
			decl.ty.qualifier.as_mut().unwrap().qualifiers.0.insert(
				0,
				glsl::syntax::TypeQualifierSpec::Layout(mk_pos_qualifier(used.location)),
			);

			let decl =
				glsl::syntax::Declaration::InitDeclaratorList(glsl::syntax::InitDeclaratorList {
					head: decl,
					tail: Vec::new(),
				});

			glsl::transpiler::glsl::show_declaration(out, &decl);
		}
		// }}}
		// {{{ Attributes
		if !file.used_attribs.is_empty() {
			writeln!(out)?;
			writeln!(out, "// Attributes")?;
		}

		for used in &file.used_attribs {
			let attrib = &self[used.id];
			let mut decl = attrib.decl.clone();

			// Add the position qualifier
			decl.ty.qualifier.as_mut().unwrap().qualifiers.0.insert(
				0,
				glsl::syntax::TypeQualifierSpec::Layout(mk_pos_qualifier(used.location)),
			);

			let decl =
				glsl::syntax::Declaration::InitDeclaratorList(glsl::syntax::InitDeclaratorList {
					head: decl,
					tail: Vec::new(),
				});

			glsl::transpiler::glsl::show_declaration(out, &decl);
		}

		// }}}
		// {{{ Varyings
		if !file.used_varyings.is_empty() {
			writeln!(out)?;
			writeln!(out, "// Varyings")?;
		}

		for used in &file.used_varyings {
			let varying = &self[used.id];
			let mut decl = varying.decl.clone();

			// Add the position qualifier
			let pos_qualifier =
				glsl::syntax::TypeQualifierSpec::Layout(mk_pos_qualifier(used.location));

			let varying_qualifier = glsl::syntax::TypeQualifierSpec::Storage(match stage {
				ShaderStage::Vert => glsl::syntax::StorageQualifier::Out,
				ShaderStage::Frag => glsl::syntax::StorageQualifier::In,
			});

			decl.ty.qualifier.as_mut().unwrap().qualifiers.0 =
				vec![pos_qualifier, varying_qualifier];

			let decl =
				glsl::syntax::Declaration::InitDeclaratorList(glsl::syntax::InitDeclaratorList {
					head: decl,
					tail: Vec::new(),
				});

			glsl::transpiler::glsl::show_declaration(out, &decl);
		}
		// }}}

		// Functions
		writeln!(out)?;
		writeln!(out, "// Main")?;
		match stage {
			ShaderStage::Vert => self.gen_function(
				"main",
				file.vert_main.ok_or_else(|| {
					anyhow!("Cannot generate vertex shader without a `vert` function")
				})?,
				out,
			),
			ShaderStage::Frag => self.gen_function(
				"main",
				file.frag_main.ok_or_else(|| {
					anyhow!("Cannot generate fragment shader without a `frag` function")
				})?,
				out,
			),
		}

		Ok(())
	}

	fn gen_function(&self, name: &str, id: GlslUsedFunctionId, out: &mut String) {
		let f = &self[id];
		let f_def = &self[f.decl_id];
		let mut def = f_def.def.clone();
		def.prototype.name.0 = name.to_string();
		glsl::transpiler::glsl::show_function_definition(out, &def);
	}
}
// }}}

// {{{ Helpers
fn mk_pos_qualifier(location: usize) -> glsl::syntax::LayoutQualifier {
	glsl::syntax::LayoutQualifier {
		ids: glsl::syntax::NonEmpty::from_non_empty_iter(std::iter::once(
			glsl::syntax::LayoutQualifierSpec::Identifier(
				"location".into(),
				Some(Box::new(glsl::syntax::Expr::IntConst(location as i32))),
			),
		))
		.unwrap(),
	}
}
// }}}
