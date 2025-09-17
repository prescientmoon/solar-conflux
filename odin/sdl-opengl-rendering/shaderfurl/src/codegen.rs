use std::fmt::Write;

use anyhow::bail;
use convert_case::Casing;

use crate::syntax::*;
use crate::types::*;

// Codegen
// {{{ Codegen
impl State {
	pub fn codegen(&self, out: &mut String) -> anyhow::Result<()> {
		writeln!(out, "package {}", self.odin_package)?;
		writeln!(out)?;
		writeln!(out, "import \"core:log\"")?;
		writeln!(out, "import \"core:math/linalg/glsl\"")?;
		writeln!(out, "import \"vendor:OpenGL\"")?;
		self.ubo_codegen(out)?;

		for f in &self.files {
			self.codegen_program(f, out)?;
		}

		Ok(())
	}

	pub fn codegen_program(&self, f: &GlslFile, out: &mut String) -> anyhow::Result<()> {
		let filename = f.short_path.file_stem().unwrap().to_str().unwrap();
		writeln!(out)?;
		writeln!(out, "// ========== File: {filename}")?;
		self.codegen_types(f, out)?;

		if !f.has_vert || !f.has_frag {
			return Ok(());
		}

		self.codegen_attribs(f, out)?;
		self.codegen_uniforms(f, out)?;

		Ok(())
	}

	pub fn codegen_type(
		&self,
		id: GlslFileId,
		ty: &glsl::syntax::TypeSpecifier,
		out: &mut String,
	) -> anyhow::Result<()> {
		if let Some(arr) = &ty.array_specifier {
			for dim in &arr.dimensions {
				let i = expr_as_int(arr_dimension_as_sized(dim)?)?;
				write!(out, "[{i}]")?;
			}
		}

		self.codegen_type_non_array(id, &ty.ty, out)?;
		Ok(())
	}

	pub fn codegen_type_non_array(
		&self,
		id: GlslFileId,
		ty: &glsl::syntax::TypeSpecifierNonArray,
		out: &mut String,
	) -> anyhow::Result<()> {
		use glsl::syntax::TypeSpecifierNonArray::*;
		match ty {
			Void => write!(out, "[0]byte")?,
			Bool => write!(out, "bool")?,
			Int => write!(out, "i32")?,
			UInt => write!(out, "u32")?,
			Float => write!(out, "f32")?,
			Double => write!(out, "f64")?,
			Vec2 => write!(out, "glsl.vec2")?,
			Vec3 => write!(out, "glsl.vec3")?,
			Vec4 => write!(out, "glsl.vec4")?,
			DVec2 => write!(out, "glsl.dvec2")?,
			DVec3 => write!(out, "glsl.dvec3")?,
			DVec4 => write!(out, "glsl.dvec4")?,
			IVec2 => write!(out, "glsl.ivec2")?,
			IVec3 => write!(out, "glsl.ivec3")?,
			IVec4 => write!(out, "glsl.ivec4")?,
			UVec2 => write!(out, "glsl.uvec2")?,
			UVec3 => write!(out, "glsl.uvec3")?,
			UVec4 => write!(out, "glsl.uvec4")?,
			Mat2 => write!(out, "glsl.mat2")?,
			Mat23 => write!(out, "glsl.mat2x3")?,
			Mat24 => write!(out, "glsl.mat2x4")?,
			Mat3 => write!(out, "glsl.mat3")?,
			Mat32 => write!(out, "glsl.mat3x2")?,
			Mat34 => write!(out, "glsl.mat3x4")?,
			Mat4 => write!(out, "glsl.mat4")?,
			Mat42 => write!(out, "glsl.mat4x2")?,
			Mat43 => write!(out, "glsl.mat4x3")?,
			DMat2 => write!(out, "glsl.dmat2")?,
			DMat23 => write!(out, "glsl.dmat2x3")?,
			DMat24 => write!(out, "glsl.dmat2x4")?,
			DMat3 => write!(out, "glsl.dmat3")?,
			DMat32 => write!(out, "glsl.dmat3x2")?,
			DMat34 => write!(out, "glsl.dmat3x4")?,
			DMat4 => write!(out, "glsl.dmat4")?,
			DMat42 => write!(out, "glsl.dmat4x2")?,
			DMat43 => write!(out, "glsl.dmat4x3")?,
			Struct(s) => {
				writeln!(out, "struct {{")?;
				for field in &s.fields {
					for ident in &field.identifiers {
						writeln!(
							out,
							"{} : ",
							ident.ident.0.to_case(convert_case::Case::Snake)
						)?;

						let mut ty = field.ty.clone();
						ty.array_specifier =
							concat_array_specifiers(&ident.array_spec, &ty.array_specifier);

						self.codegen_type(id, &ty, out)?;
						writeln!(out, ",")?;
					}
				}
				writeln!(out, "}}")?;
			}
			_ => bail!("Type {ty:?} not supported."),
		}
		Ok(())
	}

	pub fn codegen_block_to_struct(
		&self,
		id: GlslFileId,
		name: &str,
		block: &glsl::syntax::Block,
		out: &mut String,
	) -> anyhow::Result<()> {
		writeln!(out)?;
		writeln!(out, "{name} :: struct {{")?;
		for field in &block.fields {
			if let Some(q) = &field.qualifier {
				bail!("Qualifier {q:?} not allowed for interface block field.");
			}

			for ident in &field.identifiers {
				write!(
					out,
					"  {} : ",
					ident.ident.0.to_case(convert_case::Case::Snake)
				)?;

				let mut ty = field.ty.clone();
				ty.array_specifier =
					concat_array_specifiers(&ident.array_spec, &ty.array_specifier);

				self.codegen_type(id, &ty, out)?;
				writeln!(out, ",")?;
			}
		}
		writeln!(out, "}}")?;
		Ok(())
	}
}
// }}}
// {{{ Codegen helpers
fn maybe_repeated(
	out: &mut String,
	odin_index_type: &str,
	count: usize,
	f: impl FnOnce(&mut String) -> anyhow::Result<()>,
) -> anyhow::Result<()> {
	if count > 1 {
		writeln!(out, "for i in {odin_index_type}(0)..<{count} {{")?;
		f(out)?;
		writeln!(out, "}}")?;
	} else if count == 1 {
		f(out)?;
	}

	Ok(())
}
// }}}
// {{{ UBO codegen
impl State {
	pub fn ubo_codegen(&self, out: &mut String) -> anyhow::Result<()> {
		if self.ubos.is_empty() {
			return Ok(());
		}

		writeln!(out)?;
		writeln!(out, "// ========== UBOs")?;
		writeln!(out, "Ubo_Id :: enum {{")?;
		for ubo in &self.ubos {
			let name = ubo.decl.name.0.to_case(convert_case::Case::Ada);
			writeln!(out, "  {name},")?;
		}
		writeln!(out, "}}")?;
		writeln!(out)?;
		writeln!(out, "@(rodata)")?;
		writeln!(out, "UBO_BINDINGS : [Ubo_Id]u32 = {{")?;
		for (ix, ubo) in self.ubos.iter().enumerate() {
			let name = ubo.decl.name.0.to_case(convert_case::Case::Ada);
			writeln!(out, "  .{name} = {ix},")?;
		}
		writeln!(out, "}}")?;
		for ubo in &self.ubos {
			let name = format!("U_{}", ubo.decl.name.0).to_case(convert_case::Case::Ada);
			self.codegen_block_to_struct(ubo.at.file, &name, &ubo.decl, out)?;
		}
		Ok(())
	}
}
// }}}
// {{{ Uniform codegen
impl State {
	pub fn codegen_uniforms(&self, f: &GlslFile, out: &mut String) -> anyhow::Result<()> {
		fn uname(uniform: &GlslUniform) -> String {
			uniform
				.decl
				.name
				.as_ref()
				.unwrap()
				.0
				.to_case(convert_case::Case::Ada)
		}

		if f.uniforms.is_empty() {
			return Ok(());
		}

		let filename = f.short_path.file_stem().unwrap().to_str().unwrap();
		let uniform_id_name =
			format!("{filename}_UNIFORM_ID").to_case(convert_case::Case::Constant);
		let loc_table_name =
			format!("{filename}_UNIFORM_LOCATION_TABLE").to_case(convert_case::Case::Ada);

		writeln!(out, "{uniform_id_name} :: enum {{")?;
		for uniform in &f.uniforms {
			writeln!(out, "  {},", uname(uniform))?;
		}
		writeln!(out, "}}")?;
		writeln!(out)?;
		writeln!(out, "@(rodata)")?;
		writeln!(out, "{} : [{uniform_id_name}]u32 = {{", loc_table_name,)?;
		for uniform in &f.uniforms {
			writeln!(out, "  .{} = {},", uname(uniform), uniform.location)?;
		}
		writeln!(out, "}}")?;

		for uniform in &f.uniforms {
			writeln!(out)?;
			let name =
				format!("{filename}_set_{}", uname(uniform)).to_case(convert_case::Case::Snake);

			write!(out, "{name} :: proc(value : ^")?;
			self.codegen_type(f.id, &uniform.decl.ty.ty, out)?;
			writeln!(out, ") {{")?;
			writeln!(out, "loc : i32 = {}", uniform.location)?;
			self.codegen_set_uniform(f.id, &uniform.decl.ty.ty, out)?;
			writeln!(out, "}}")?;
		}

		writeln!(out)?;
		Ok(())
	}

	pub fn codegen_set_uniform(
		&self,
		id: GlslFileId,
		ty: &glsl::syntax::TypeSpecifier,
		out: &mut String,
	) -> anyhow::Result<()> {
		let count = get_type_count(ty)?;
		maybe_repeated(out, "i32", count, |out| {
			if count > 1 {
				writeln!(out, "value := &value[i]")?;
				let stride = self.uniform_count_non_array(id, &ty.ty)?;
				writeln!(out, "loc := loc + {stride} * i")?;
			}
			self.codegen_set_uniform_non_array(id, &ty.ty, out)?;
			Ok(())
		})?;

		Ok(())
	}

	pub fn codegen_set_uniform_non_array(
		&self,
		id: GlslFileId,
		ty: &glsl::syntax::TypeSpecifierNonArray,
		out: &mut String,
	) -> anyhow::Result<()> {
		use glsl::syntax::TypeSpecifierNonArray::*;
		match ty {
			Void => {}
			Bool => writeln!(out, "OpenGL.Uniform1i(loc, u32(value^))")?,
			Int => writeln!(out, "OpenGL.Uniform1i(loc, value^)")?,
			UInt => writeln!(out, "OpenGL.Uniform1ui(loc, value^)")?,
			Float => writeln!(out, "OpenGL.Uniform1f(loc, value^)")?,
			Double => writeln!(out, "OpenGL.Uniform1d(loc, value^)")?,
			Vec2 => Self::codegen_set_uniform_vec(out, "f", 2)?,
			Vec3 => Self::codegen_set_uniform_vec(out, "f", 3)?,
			Vec4 => Self::codegen_set_uniform_vec(out, "f", 4)?,
			DVec2 => Self::codegen_set_uniform_vec(out, "d", 2)?,
			DVec3 => Self::codegen_set_uniform_vec(out, "d", 3)?,
			DVec4 => Self::codegen_set_uniform_vec(out, "d", 4)?,
			IVec2 => Self::codegen_set_uniform_vec(out, "i", 2)?,
			IVec3 => Self::codegen_set_uniform_vec(out, "i", 3)?,
			IVec4 => Self::codegen_set_uniform_vec(out, "i", 4)?,
			UVec2 => Self::codegen_set_uniform_vec(out, "ui", 2)?,
			UVec3 => Self::codegen_set_uniform_vec(out, "ui", 3)?,
			UVec4 => Self::codegen_set_uniform_vec(out, "ui", 4)?,
			Mat2 => Self::codegen_set_uniform_mat(out, "f", 2, 2)?,
			Mat23 => Self::codegen_set_uniform_mat(out, "f", 2, 3)?,
			Mat24 => Self::codegen_set_uniform_mat(out, "f", 2, 4)?,
			Mat3 => Self::codegen_set_uniform_mat(out, "f", 3, 3)?,
			Mat32 => Self::codegen_set_uniform_mat(out, "f", 3, 2)?,
			Mat34 => Self::codegen_set_uniform_mat(out, "f", 3, 4)?,
			Mat4 => Self::codegen_set_uniform_mat(out, "f", 4, 4)?,
			Mat42 => Self::codegen_set_uniform_mat(out, "f", 4, 2)?,
			Mat43 => Self::codegen_set_uniform_mat(out, "f", 4, 3)?,
			DMat2 => Self::codegen_set_uniform_mat(out, "d", 2, 2)?,
			DMat23 => Self::codegen_set_uniform_mat(out, "d", 2, 3)?,
			DMat24 => Self::codegen_set_uniform_mat(out, "d", 2, 4)?,
			DMat3 => Self::codegen_set_uniform_mat(out, "d", 3, 3)?,
			DMat32 => Self::codegen_set_uniform_mat(out, "d", 3, 2)?,
			DMat34 => Self::codegen_set_uniform_mat(out, "d", 3, 4)?,
			DMat4 => Self::codegen_set_uniform_mat(out, "d", 4, 4)?,
			DMat42 => Self::codegen_set_uniform_mat(out, "d", 4, 2)?,
			DMat43 => Self::codegen_set_uniform_mat(out, "d", 4, 3)?,
			Struct(s) => {
				let mut loc = 0;
				for field in &s.fields {
					writeln!(out, "{{")?;
					for ident in &field.identifiers {
						if loc != 0 {
							writeln!(out, "loc := loc + {loc}",)?;
						}
						writeln!(
							out,
							"value := &value.{}",
							ident.ident.0.to_case(convert_case::Case::Snake)
						)?;

						let mut ty = field.ty.clone();
						ty.array_specifier =
							concat_array_specifiers(&ident.array_spec, &ty.array_specifier);

						self.codegen_set_uniform(id, &ty, out)?;
						loc += self.uniform_count(id, &ty)?;
					}
					writeln!(out, "}}")?;
				}
			}
			_ => bail!("Type {ty:?} not supported as uniform."),
		}

		Ok(())
	}

	pub fn codegen_set_uniform_vec(out: &mut String, ty: &str, cols: usize) -> anyhow::Result<()> {
		writeln!(out, "OpenGL.Uniform{cols}{ty}v(loc, 1, auto_cast value)")?;

		Ok(())
	}

	pub fn codegen_set_uniform_mat(
		out: &mut String,
		ty: &str,
		cols: usize,
		rows: usize,
	) -> anyhow::Result<()> {
		if rows == cols {
			writeln!(
				out,
				"OpenGL.UniformMatrix{cols}{ty}v(loc, 1, false, auto_cast value)"
			)?;
		} else {
			writeln!(
				out,
				"OpenGL.UniformMatrix{cols}x{rows}{ty}v(loc, 1, false, auto_cast value)"
			)?;
		}

		Ok(())
	}
}
// }}}
// {{{ Attrib codegen
fn aname(attrib: &GlslAttrib) -> String {
	attrib
		.decl
		.name
		.as_ref()
		.unwrap()
		.0
		.to_case(convert_case::Case::Ada)
}

impl State {
	pub fn codegen_types(&self, f: &GlslFile, out: &mut String) -> anyhow::Result<()> {
		let filename = f.short_path.file_stem().unwrap().to_str().unwrap();
		for attrib in &f.attribs {
			if attrib.at.file == f.id {
				writeln!(
					out,
					"{} :: ",
					format!("{filename}_{}_Type", aname(attrib)).to_case(convert_case::Case::Ada)
				)?;
				self.codegen_type(f.id, &attrib.decl.ty.ty, out)?;
				writeln!(out)?;
			}
		}
		writeln!(out)?;

		Ok(())
	}

	pub fn codegen_attribs(&self, f: &GlslFile, out: &mut String) -> anyhow::Result<()> {
		if f.attribs.is_empty() {
			return Ok(());
		}

		let filename = f.short_path.file_stem().unwrap().to_str().unwrap();
		let attrib_id_name = format!("{filename}_ATTRIB_ID").to_case(convert_case::Case::Constant);
		let loc_table_name =
			format!("{filename}_ATTRIB_LOCATION_TABLE").to_case(convert_case::Case::Ada);

		writeln!(out, "{attrib_id_name} :: enum {{")?;
		for attrib in &f.attribs {
			writeln!(out, "  {},", aname(attrib))?;
		}
		writeln!(out, "}}")?;
		writeln!(out)?;
		writeln!(out, "@(rodata)")?;
		writeln!(out, "{} : [{attrib_id_name}]u32 = {{", loc_table_name,)?;
		for attrib in &f.attribs {
			writeln!(out, "  .{} = {},", aname(attrib), attrib.location)?;
		}
		writeln!(out, "}}")?;
		writeln!(out)?;
		writeln!(
			out,
			"{} :: proc(buffers: [{attrib_id_name}]u32, indices: u32 = 0, instanced : bit_set[{attrib_id_name}] = {{}}) -> (program: u32, vao: u32) {{",
			format!("{filename}_create_program").to_case(convert_case::Case::Snake)
		)?;
		writeln!(out, "v_shader: string // TODO")?;
		writeln!(out, "f_shader: string // TODO")?;
		writeln!(
			out,
			"program = OpenGL.load_shaders_source(v_shader, f_shader) or_else log.panic(\"Failed to create program `{filename}`\")"
		)?;
		writeln!(out)?;

		writeln!(out, "// Create VAO")?;
		writeln!(out, "OpenGL.GenVertexArrays(1, &vao)")?;
		writeln!(out, "OpenGL.BindVertexArray(vao)")?;
		writeln!(out, "defer OpenGL.BindVertexArray(0)")?;
		for attrib in &f.attribs {
			writeln!(out)?;
			writeln!(out, "// Set up .{}", aname(attrib))?;
			writeln!(
				out,
				"OpenGL.BindBuffer(buffers[.{}], OpenGL.ARRAY_BUFFER)",
				aname(attrib)
			)?;
		}
		writeln!(out)?;
		writeln!(out, "return program, vao")?;
		writeln!(out, "}}")?;

		writeln!(out)?;
		Ok(())
	}

	pub fn codegen_set_attrib(
		&self,
		id: GlslFileId,
		ty: &glsl::syntax::TypeSpecifier,
		_stride: usize,
		_instanced: bool,
		out: &mut String,
	) -> anyhow::Result<()> {
		let count = get_type_count(ty)?;
		maybe_repeated(out, "i32", count, |out| {
			if count > 1 {
				writeln!(out, "value := &value[i]")?;
				let stride = self.uniform_count_non_array(id, &ty.ty)?;
				writeln!(out, "loc := loc + {stride} * i")?;
			}
			self.codegen_set_uniform_non_array(id, &ty.ty, out)?;
			Ok(())
		})?;

		Ok(())
	}
}
// }}}
