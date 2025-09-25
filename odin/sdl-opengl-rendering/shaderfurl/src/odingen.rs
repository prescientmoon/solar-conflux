use std::fmt::Display;
use std::fmt::Write;
use std::rc::Rc;

use anyhow::bail;
use convert_case::Casing;

use crate::syntax::*;
use crate::types::*;

use glsl::syntax::TypeSpecifierNonArray;

// {{{ Name helpers
fn const_case(s: &str) -> String {
	s.to_case(convert_case::Case::Constant)
}

fn ada_case(s: &str) -> String {
	s.to_case(convert_case::Case::Ada)
}

fn snake_case(s: &str) -> String {
	s.to_case(convert_case::Case::Snake)
}

impl GlslAttribDecl {
	fn name(&self) -> String {
		ada_case(&self.decl.name.as_ref().unwrap().0)
	}
}

impl GlslUniformDecl {
	fn name(&self) -> String {
		ada_case(&self.decl.name.as_ref().unwrap().0)
	}

	fn type_name(&self, state: &State) -> String {
		ada_case(&format!(
			"{}_U_{}",
			state[self.at.file].filename(),
			self.name()
		))
	}
}
// }}}
// {{{ Codegen
impl State {
	pub fn gen_module(&self, out: &mut String) -> anyhow::Result<()> {
		writeln!(out, "package {}", self.odin_package)?;
		writeln!(out)?;
		writeln!(out, "import \"core:log\"")?;
		writeln!(out, "import \"core:math/linalg/glsl\"")?;
		writeln!(out, "import \"vendor:OpenGL\"")?;
		self.ubo_codegen(out)?;

		for f in &self.files {
			self.gen_program(f, out)?;
		}

		Ok(())
	}

	fn gen_program(&self, f: &GlslFile, out: &mut String) -> anyhow::Result<()> {
		let filename = f.short_path.file_stem().unwrap().to_str().unwrap();
		writeln!(out)?;
		writeln!(out, "// ========== File: {filename}")?;
		self.gen_types(f, out)?;

		if !f.has_vert || !f.has_frag {
			return Ok(());
		}

		self.gen_uniforms(f, out)?;
		self.gen_attribs(f, out)?;

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
		ty: &TypeSpecifierNonArray,
		out: &mut String,
	) -> anyhow::Result<()> {
		use TypeSpecifierNonArray::*;
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
						writeln!(out, "{} : ", snake_case(&ident.ident.0))?;

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
				write!(out, "  {} : ", snake_case(&ident.ident.0))?;

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
			let name = ada_case(&ubo.decl.name.0);
			writeln!(out, "  {name},")?;
		}
		writeln!(out, "}}")?;
		writeln!(out)?;
		writeln!(out, "@(rodata)")?;
		writeln!(out, "UBO_BINDINGS : [Ubo_Id]u32 = {{")?;
		for (ix, ubo) in self.ubos.iter().enumerate() {
			let name = ada_case(&ubo.decl.name.0);
			writeln!(out, "  .{name} = {ix},")?;
		}
		writeln!(out, "}}")?;
		for ubo in &self.ubos {
			let name = ada_case(&format!("U_{}", ubo.decl.name.0));
			// TODO: use the correct layout here!
			self.codegen_block_to_struct(ubo.at.file, &name, &ubo.decl, out)?;
		}
		Ok(())
	}
}
// }}}

// Layout based generation (awful code)
// {{{ Layout variable state
#[derive(Clone)]
struct LayoutVarState {
	constant: usize,
	var: Option<Rc<String>>,
}

impl LayoutVarState {
	fn from_constant(constant: usize) -> Self {
		Self {
			constant,
			var: None,
		}
	}

	fn set_var(&mut self, v: &str) {
		if self.var.as_ref().is_none_or(|i| **i != v) {
			self.var = Some(Rc::new(v.to_string()));
		}
	}
}

impl Display for LayoutVarState {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if let Some(var) = &self.var {
			if self.constant > 0 {
				write!(f, "{} + {}", var, self.constant)?;
			} else {
				write!(f, "{}", var)?;
			}
		} else {
			write!(f, "{}", self.constant)?;
		}

		Ok(())
	}
}
// }}}
// {{{ Accessor state
#[derive(Clone)]
struct AccessorState {
	var: String,
	/// When this is false, the variable already refers to a pointer / a value that
	/// cannot be taken the pointer of.
	should_ref: bool,
}

impl AccessorState {
	fn from_ref(s: &str) -> Self {
		Self {
			var: s.to_string(),
			should_ref: false,
		}
	}
}

impl Display for AccessorState {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.should_ref {
			write!(f, "&{}", self.var)?;
		} else {
			write!(f, "{}", self.var)?;
		}

		Ok(())
	}
}

struct UnreferencedAccessor(AccessorState);
impl Display for UnreferencedAccessor {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.0.var)?;
		Ok(())
	}
}
// }}}
// {{{ Layout folding
impl State {
	/// Fold on some generic input (uniform/attribute), optionally keeping track
	/// of accessors into some value, offsets, and locations.
	#[allow(clippy::too_many_arguments)]
	// This is horribly implemented, OMG.
	fn gen_fold_input(
		&self,
		id: GlslFileId,
		ty: &glsl::syntax::TypeSpecifier,
		// If None, do not track offsets.
		location: LayoutVarState,
		offset: Option<LayoutVarState>,
		var: Option<AccessorState>,
		out: &mut String,
		generator: &impl Fn(
			LayoutVarState,         // loc
			Option<LayoutVarState>, // offset
			usize,                  // size
			Option<AccessorState>,  // var
			&TypeSpecifierNonArray, // ty
			&mut String,            // out
		) -> anyhow::Result<()>,
	) -> anyhow::Result<()> {
		let count = get_type_count(ty)?;

		if count > 1 {
			if let Some(var) = &var {
				writeln!(out, "for &value, i in {var} {{")?;
			} else {
				writeln!(out, "for i in i32(0)..<{count} {{")?;
			}

			let stride = self.input_count_non_array(id, &ty.ty)?;
			writeln!(out, "loc := {location} + {stride} * i")?;

			let mut location = location;
			location.constant = 0;
			location.set_var("loc");

			let mut offset = offset;
			if let Some(offset) = &mut offset {
				let stride = self.type_sizeof_non_array(id, &ty.ty)?;
				writeln!(out, "offset := {offset} + {stride} * i")?;
				offset.constant = 0;
				offset.set_var("offset");
			}

			let mut cloned = ty.clone();
			cloned.array_specifier = None;
			self.gen_fold_input(
				id,
				&cloned,
				location,
				offset,
				var.as_ref().map(|_| AccessorState::from_ref("value")),
				out,
				generator,
			)?;
			writeln!(out, "}}")?;
			return Ok(());
		}

		// TODO: handle named types here
		match &ty.ty {
			TypeSpecifierNonArray::Struct(s) => {
				let mut local_loc = 0;
				let mut local_offset = 0;
				for field in &s.fields {
					for ident in &field.identifiers {
						let mut ty = field.ty.clone();
						ty.array_specifier =
							concat_array_specifiers(&ident.array_spec, &ty.array_specifier);

						self.gen_fold_input(
							id,
							&ty,
							LayoutVarState {
								constant: location.constant + local_loc,
								var: location.var.clone(),
							},
							offset.clone().map(|state| LayoutVarState {
								constant: state.constant + local_offset,
								var: state.var,
							}),
							var.as_ref().map(|var| AccessorState {
								should_ref: true,
								var: format!("{}.{}", var.var, snake_case(&ident.ident.0)),
							}),
							out,
							generator,
						)?;

						local_loc += self.input_count(id, &ty)?;
						local_offset += self.type_sizeof(id, &ty)?;
					}
				}
			}
			other => generator(
				location,
				offset,
				self.type_sizeof_non_array(id, &ty.ty)?,
				var,
				other,
				out,
			)?,
		}

		Ok(())
	}
}
// }}}

// External interface
// {{{ Uniform codegen
impl State {
	fn gen_uniforms(&self, f: &GlslFile, out: &mut String) -> anyhow::Result<()> {
		if f.used_uniforms.is_empty() {
			return Ok(());
		}

		let uniform_id_name = const_case(&format!("{}_UNIFORM_ID", f.filename()));
		let loc_table_name = ada_case(&format!("{}_UNIFORM_LOCATION_TABLE", f.filename()));

		// Generate an enum of all the uniforms used
		writeln!(out, "{uniform_id_name} :: enum {{")?;
		for uniform in &f.used_uniforms {
			writeln!(out, "  {},", self[uniform.id].name())?;
		}
		writeln!(out, "}}")?;
		writeln!(out)?;

		// Generate location table for the uniform layout
		writeln!(out, "@(rodata)")?;
		writeln!(out, "{} : [{uniform_id_name}]u32 = {{", loc_table_name)?;
		for uniform in &f.used_uniforms {
			writeln!(
				out,
				"  .{} = {},",
				self[uniform.id].name(),
				uniform.location
			)?;
		}
		writeln!(out, "}}")?;
		writeln!(out)?;

		// Generate setter functions
		for uniform in &f.used_uniforms {
			let u_decl = &self[uniform.id];
			let name = snake_case(&format!("{}_set_{}", f.filename(), u_decl.name()));

			writeln!(
				out,
				"{name} :: proc(value : ^{}) {{",
				u_decl.type_name(self)
			)?;

			self.gen_fold_input(
				f.id,
				&u_decl.decl.ty.ty,
				LayoutVarState::from_constant(uniform.location),
				None,
				Some(AccessorState::from_ref("value")),
				out,
				&|loc, _, _, var, ty, out| gen_set_uniform_primitive(loc, ty, &var.unwrap(), out),
			)?;
			writeln!(out, "}}")?;
			writeln!(out)?;
		}

		Ok(())
	}
}
// }}}
// {{{ Uniform setting helpers
fn gen_set_uniform_primitive(
	loc: LayoutVarState,
	ty: &TypeSpecifierNonArray,
	var: &AccessorState,
	out: &mut String,
) -> anyhow::Result<()> {
	use TypeSpecifierNonArray::*;
	match ty {
		Void => {}
		Bool => writeln!(out, "OpenGL.Uniform1i({loc}, u32({}))", var.var)?,
		Int => writeln!(out, "OpenGL.Uniform1i({loc}, {})", var.var)?,
		UInt => writeln!(out, "OpenGL.Uniform1ui({loc}, {})", var.var)?,
		Float => writeln!(out, "OpenGL.Uniform1f({loc}, {})", var.var)?,
		Double => writeln!(out, "OpenGL.Uniform1d({loc}, {})", var.var)?,
		Vec2 => gen_set_uniform_vec(out, loc, var, "f", 2)?,
		Vec3 => gen_set_uniform_vec(out, loc, var, "f", 3)?,
		Vec4 => gen_set_uniform_vec(out, loc, var, "f", 4)?,
		DVec2 => gen_set_uniform_vec(out, loc, var, "d", 2)?,
		DVec3 => gen_set_uniform_vec(out, loc, var, "d", 3)?,
		DVec4 => gen_set_uniform_vec(out, loc, var, "d", 4)?,
		IVec2 => gen_set_uniform_vec(out, loc, var, "i", 2)?,
		IVec3 => gen_set_uniform_vec(out, loc, var, "i", 3)?,
		IVec4 => gen_set_uniform_vec(out, loc, var, "i", 4)?,
		UVec2 => gen_set_uniform_vec(out, loc, var, "ui", 2)?,
		UVec3 => gen_set_uniform_vec(out, loc, var, "ui", 3)?,
		UVec4 => gen_set_uniform_vec(out, loc, var, "ui", 4)?,
		Mat2 => gen_set_uniform_sq_mat(out, loc, var, "f", 2)?,
		Mat23 => gen_set_uniform_mat(out, loc, var, "f", 2, 3)?,
		Mat24 => gen_set_uniform_mat(out, loc, var, "f", 2, 4)?,
		Mat3 => gen_set_uniform_sq_mat(out, loc, var, "f", 3)?,
		Mat32 => gen_set_uniform_mat(out, loc, var, "f", 3, 2)?,
		Mat34 => gen_set_uniform_mat(out, loc, var, "f", 3, 4)?,
		Mat4 => gen_set_uniform_sq_mat(out, loc, var, "f", 4)?,
		Mat42 => gen_set_uniform_mat(out, loc, var, "f", 4, 2)?,
		Mat43 => gen_set_uniform_mat(out, loc, var, "f", 4, 3)?,
		DMat2 => gen_set_uniform_sq_mat(out, loc, var, "d", 2)?,
		DMat23 => gen_set_uniform_mat(out, loc, var, "d", 2, 3)?,
		DMat24 => gen_set_uniform_mat(out, loc, var, "d", 2, 4)?,
		DMat3 => gen_set_uniform_sq_mat(out, loc, var, "d", 3)?,
		DMat32 => gen_set_uniform_mat(out, loc, var, "d", 3, 2)?,
		DMat34 => gen_set_uniform_mat(out, loc, var, "d", 3, 4)?,
		DMat4 => gen_set_uniform_sq_mat(out, loc, var, "d", 4)?,
		DMat42 => gen_set_uniform_mat(out, loc, var, "d", 4, 2)?,
		DMat43 => gen_set_uniform_mat(out, loc, var, "d", 4, 3)?,
		_ => bail!("Type {ty:?} not supported as uniform."),
	}

	Ok(())
}

fn gen_set_uniform_vec(
	out: &mut String,
	loc: LayoutVarState,
	var: &AccessorState,
	ty: &str,
	cols: usize,
) -> anyhow::Result<()> {
	writeln!(out, "OpenGL.Uniform{cols}{ty}v({loc}, 1, auto_cast {var})")?;

	Ok(())
}

fn gen_set_uniform_mat(
	out: &mut String,
	loc: LayoutVarState,
	var: &AccessorState,
	ty: &str,
	cols: usize,
	rows: usize,
) -> anyhow::Result<()> {
	assert_ne!(cols, rows);
	writeln!(
		out,
		"OpenGL.UniformMatrix{cols}x{rows}{ty}v({loc}, 1, false, auto_cast {var})"
	)?;

	Ok(())
}

fn gen_set_uniform_sq_mat(
	out: &mut String,
	loc: LayoutVarState,
	var: &AccessorState,
	ty: &str,
	dim: usize,
) -> anyhow::Result<()> {
	writeln!(
		out,
		"OpenGL.UniformMatrix{dim}{ty}v({loc}, 1, false, auto_cast {var})"
	)?;

	Ok(())
}
// }}}
// {{{ Attrib codegen
impl State {
	pub fn gen_attribs(&self, f: &GlslFile, out: &mut String) -> anyhow::Result<()> {
		if f.used_attribs.is_empty() {
			return Ok(());
		}

		let attrib_id_name = const_case(&format!("{}_ATTRIB_ID", f.filename()));
		let loc_table_name = const_case(&format!("{}_ATTRIB_LOCATION_TABLE", f.filename()));

		// Generate an enum of all the attributes used
		writeln!(out, "{attrib_id_name} :: enum {{")?;
		for attrib in &f.used_attribs {
			writeln!(out, "  {},", self[attrib.id].name())?;
		}
		writeln!(out, "}}")?;
		writeln!(out)?;

		// Generate location table for the attribute layout
		writeln!(out, "@(rodata)")?;
		writeln!(out, "{} : [{attrib_id_name}]u32 = {{", loc_table_name)?;
		for attrib in &f.used_attribs {
			writeln!(out, "  .{} = {},", self[attrib.id].name(), attrib.location)?;
		}
		writeln!(out, "}}")?;
		writeln!(out)?;

		// Generate program initializer
		writeln!(
			out,
		  " {} :: proc(buffers: [{attrib_id_name}]u32, indices: u32 = 0, instanced : bit_set[{attrib_id_name}] = {{}}) -> (program: u32, vao: u32) {{
        v_shader: string // TODO
        f_shader: string // TODO

        program_ok: bool
        program, program_ok = OpenGL.load_shaders_source(v_shader, f_shader) 
        if !program_ok do log.panic(\"Failed to create program `{}`\")
      ",
			snake_case(&format!("{}_create_program", f.filename())),
      f.filename(),
		)?;

		writeln!(out, "// Create VAO")?;
		writeln!(out, "OpenGL.GenVertexArrays(1, &vao)")?;
		writeln!(out, "OpenGL.BindVertexArray(vao)")?;
		writeln!(out, "defer OpenGL.BindVertexArray(0)")?;
		writeln!(out)?;

		for attrib in &f.used_attribs {
			let a_decl = &self[attrib.id];
			writeln!(out, "// Set up .{}", a_decl.name())?;
			writeln!(
				out,
				"OpenGL.BindBuffer(buffers[.{}], OpenGL.ARRAY_BUFFER)",
				a_decl.name()
			)?;

			let is_instanced = format!("{} in instanced", a_decl.name());
			let stride = self.type_sizeof(f.id, &a_decl.decl.ty.ty)?;
			self.gen_fold_input(
				f.id,
				&a_decl.decl.ty.ty,
				LayoutVarState::from_constant(attrib.location),
				Some(LayoutVarState::from_constant(0)),
				None,
				out,
				&|loc, offset, size, _, ty, out| {
					gen_set_attrib_primitive(
						ty,
						size,
						stride,
						loc,
						offset.unwrap(),
						&is_instanced,
						out,
					)
				},
			)?;

			writeln!(out)?;
		}

		writeln!(out, "return program, vao")?;
		writeln!(out, "}}")?;

		writeln!(out)?;
		Ok(())
	}
}
// }}}
// {{{ Attrib setting helpers
fn gen_set_attrib_primitive(
	ty: &TypeSpecifierNonArray,
	size: usize,
	stride: usize,
	location: LayoutVarState,
	offset: LayoutVarState,
	instanced: &str, // expression that evaluates to true when the attr is instanced
	out: &mut String,
) -> anyhow::Result<()> {
	use TypeSpecifierNonArray::*;

	// I love creating closures for no reason other than convenience, yay!! \s
	//
	// (this should be a macro)
	let go = |ty_name, rows, out| {
		gen_set_attrib_primitive_impl(
			stride,
			location.clone(),
			offset.clone(),
			instanced,
			rows,
			ty_name,
			out,
		)
	};

	let go_mat = |ty_name, rows, cols: usize, out: &mut String| -> anyhow::Result<()> {
		writeln!(out, "for i in i32(0)..<{cols} {{")?;
		writeln!(out, "loc := {location} + i")?;
		let mut location = location.clone();
		location.constant = 0;
		location.set_var("loc");

		let mut offset = offset.clone();
		let elem_size = size / cols;
		writeln!(out, "offset := {offset} + {elem_size} * i")?;
		offset.constant = 0;
		offset.set_var("offset");

		gen_set_attrib_primitive_impl(
			stride,
			location,
			offset.clone(),
			instanced,
			rows,
			ty_name,
			out,
		)?;
		writeln!(out, "}}")?;
		Ok(())
	};

	// We don't support booleans right here, and I won't bother.
	// (I'd have to add padding on the Odin side, and I'm too lazy).
	match ty {
		Void => {}
		Int => go("INT", 1, out)?,
		IVec2 => go("INT", 2, out)?,
		IVec3 => go("INT", 3, out)?,
		IVec4 => go("INT", 4, out)?,
		UInt => go("UNSIGNED_INT", 1, out)?,
		UVec2 => go("UNSIGNED_INT", 2, out)?,
		UVec3 => go("UNSIGNED_INT", 3, out)?,
		UVec4 => go("UNSIGNED_INT", 4, out)?,
		Float => go("FLOAT", 1, out)?,
		Vec2 => go("FLOAT", 2, out)?,
		Vec3 => go("FLOAT", 3, out)?,
		Vec4 => go("FLOAT", 4, out)?,
		Mat2 => go_mat("FLOAT", 2, 2, out)?,
		Mat23 => go_mat("FLOAT", 3, 2, out)?,
		Mat24 => go_mat("FLOAT", 4, 2, out)?,
		Mat3 => go_mat("FLOAT", 3, 3, out)?,
		Mat32 => go_mat("FLOAT", 2, 3, out)?,
		Mat34 => go_mat("FLOAT", 4, 3, out)?,
		Mat4 => go_mat("FLOAT", 4, 4, out)?,
		Mat42 => go_mat("FLOAT", 2, 4, out)?,
		Mat43 => go_mat("FLOAT", 3, 4, out)?,
		Double => go("DOUBLE", 1, out)?,
		DVec2 => go("DOUBLE", 2, out)?,
		DVec3 => go("DOUBLE", 3, out)?,
		DVec4 => go("DOUBLE", 4, out)?,
		DMat2 => go_mat("DOUBLE", 2, 2, out)?,
		DMat23 => go_mat("DOUBLE", 3, 2, out)?,
		DMat24 => go_mat("DOUBLE", 4, 2, out)?,
		DMat3 => go_mat("DOUBLE", 3, 3, out)?,
		DMat32 => go_mat("DOUBLE", 2, 3, out)?,
		DMat34 => go_mat("DOUBLE", 4, 3, out)?,
		DMat4 => go_mat("DOUBLE", 4, 4, out)?,
		DMat42 => go_mat("DOUBLE", 2, 4, out)?,
		DMat43 => go_mat("DOUBLE", 3, 4, out)?,
		_ => bail!("Type {ty:?} not supported as an attribute."),
	}

	Ok(())
}

fn gen_set_attrib_primitive_impl(
	stride: usize,
	location: LayoutVarState,
	offset: LayoutVarState,
	instanced: &str,
	rows: usize,
	base_type: &str,
	out: &mut String,
) -> anyhow::Result<()> {
	writeln!(out, "OpenGL.EnableVertexAttribArray({location})")?;

	writeln!(
		out,
		"OpenGL.VertexAttribPointer(
        {location},
        {rows},
        OpenGL.{base_type},
        false,
        {stride},
        uintptr({offset}),
      )
      if {instanced} do OpenGL.VertexAttribDivisor({location}, 1)"
	)?;

	Ok(())
}

// }}}
// {{{ All the types in a module
impl GlslFile {
	fn filename(&self) -> &str {
		self.short_path.file_stem().unwrap().to_str().unwrap()
	}
}

impl State {
	fn gen_types(&self, f: &GlslFile, out: &mut String) -> anyhow::Result<()> {
		for uniform in &f.declared_uniforms {
			write!(out, "{} :: ", uniform.type_name(self))?;
			self.codegen_type(f.id, &uniform.decl.ty.ty, out)?;
			writeln!(out)?;
		}

		for attrib in &f.declared_attribs {
			write!(
				out,
				"{} :: ",
				ada_case(&format!("{}_A_{}", f.filename(), attrib.name()))
			)?;
			self.codegen_type(f.id, &attrib.decl.ty.ty, out)?;
			writeln!(out)?;
		}

		Ok(())
	}
}
// }}}
