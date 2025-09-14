#![allow(dead_code)]
use std::env;
use std::fmt::Write;
use std::fs;
use std::path::Path;
use std::path::PathBuf;

use anyhow::Context;
use anyhow::anyhow;
use anyhow::bail;
use convert_case::Casing;
use glsl::visitor::Host;

// Input handling
// {{{ Find glsl files
/// Recursively collects all `.glsl` files in the given directory.
fn find_glsl_files(dir: &Path, results: &mut Vec<PathBuf>) -> anyhow::Result<()> {
	if dir.is_dir() {
		for entry in fs::read_dir(dir)? {
			let path = entry?.path();
			if path.is_dir() {
				find_glsl_files(&path, results)?;
			} else if let Some(ext) = path.extension()
				&& ext == "glsl"
			{
				results.push(path);
			}
		}
	}

	Ok(())
}
// }}}
// {{{ Parse files
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct GlslFileId(usize);

#[derive(Clone, Debug)]
struct GlslFile {
	id: GlslFileId,
	short_path: PathBuf,
	path: PathBuf,
	syntax: glsl::syntax::TranslationUnit,
	includes: Vec<GlslFileId>,

	// Stages
	has_vert: bool,
	has_frag: bool,

	// Layout
	attribs: Vec<GlslAttrib>,
	uniforms: Vec<GlslUniform>,
}

#[derive(Clone, Copy, Debug)]
struct DeclId {
	file: GlslFileId,
	ix: usize,
}

#[derive(Clone, Debug)]
struct GlslAttrib {
	at: DeclId,
	decl: glsl::syntax::SingleDeclaration,
	location: usize,
}

#[derive(Clone, Debug)]
struct GlslUniform {
	at: DeclId,
	decl: glsl::syntax::SingleDeclaration,
	location: usize,
}

fn parse_glsl_files(files: &[PathBuf]) -> anyhow::Result<Vec<GlslFile>> {
	let mut results: Vec<GlslFile> = Vec::new();
	for og_path in files {
		let path = og_path.canonicalize().unwrap();
		let source = fs::read_to_string(&path)?;
		let syntax: glsl::syntax::TranslationUnit = glsl::parser::Parse::parse(source)
			.with_context(|| format!("While parsing {:?}", &path))?;

		results.push(GlslFile {
			id: GlslFileId(results.len()),
			short_path: og_path.clone(),
			path,
			syntax,
			includes: Vec::new(),
			has_vert: false,
			has_frag: false,
			attribs: Vec::new(),
			uniforms: Vec::new(),
		});
	}

	Ok(results)
}
// }}}
// {{{ Main entrypoint
#[derive(Clone)]
struct State {
	odin_package: String,
	odin_namespace_sep: String,
	base_path: PathBuf,
	files: Vec<GlslFile>,
	ubos: Vec<GlslUbo>,
}

#[derive(Clone, Debug)]
struct GlslUbo {
	at: DeclId,
	decl: glsl::syntax::Block,
}

fn main() -> anyhow::Result<()> {
	let args: Vec<String> = env::args().collect();
	if args.len() < 2 {
		eprintln!("Usage: {} <directory>", args[0]);
		std::process::exit(1);
	}

	let base_path = Path::new(&args[1]);
	let mut files = Vec::new();
	find_glsl_files(base_path, &mut files)?;
	let parsed = parse_glsl_files(&files)?;

	let mut state = State {
		files: parsed,
		base_path: base_path.to_path_buf(),
		ubos: Vec::new(),
		odin_package: "shaders".to_string(),
		odin_namespace_sep: "›".to_string(),
	};

	state.resolve_includes()?;
	state.detect_stages();
	state.find_ubos()?;
	state.find_uniforms()?;
	state.alloc_uniforms()?;

	let mut out = String::new();
	state.codegen(&mut out)?;
	println!("{out}");
	// println!("{:?}", state.files[1].syntax);

	Ok(())
}
// }}}

// GLSL file handling
// {{{ State helpers
impl State {
	fn get(&self, id: GlslFileId) -> &GlslFile {
		&self.files[id.0]
	}

	fn get_mut(&mut self, id: GlslFileId) -> &mut GlslFile {
		&mut self.files[id.0]
	}

	fn get_decl(&self, decl: DeclId) -> &glsl::syntax::ExternalDeclaration {
		&self.get(decl.file).syntax.0.0[decl.ix]
	}
}
// }}}
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
			glsl::syntax::Path::Relative(p) => {
				self.state.get_mut(self.id).path.parent().unwrap().join(p)
			}
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
		self.state.get_mut(self.id).includes.push(referenced);
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
	fn resolve_includes(&mut self) -> anyhow::Result<()> {
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
					let other = cloned.get(*include);
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
	fn declares_function_shallow(&self, id: GlslFileId, name: &str) -> bool {
		let f = self.get(id);
		for v in &f.syntax {
			if let glsl::syntax::ExternalDeclaration::FunctionDefinition(fn_def) = v
				&& fn_def.prototype.name.0 == name
			{
				return true;
			}
		}

		false
	}

	fn declares_function(&self, id: GlslFileId, name: &str) -> bool {
		self.declares_function_shallow(id, name)
			|| self
				.get(id)
				.includes
				.iter()
				.any(|f| self.declares_function_shallow(*f, name))
	}

	fn detect_stages(&mut self) {
		let cloned = self.clone();
		for f in &mut self.files {
			f.has_vert = cloned.declares_function(f.id, "vert");
			f.has_frag = cloned.declares_function(f.id, "frag");
		}
	}
}
// }}}

// Layout stuff
// {{{ UBO detection
impl State {
	fn find_ubos(&mut self) -> anyhow::Result<()> {
		let cloned = self.clone();
		for f in cloned.files {
			for (ix, decl) in f.syntax.into_iter().enumerate() {
				if let glsl::syntax::ExternalDeclaration::Declaration(
					glsl::syntax::Declaration::Block(block),
				) = decl
				{
					let storage = ensure_only_storage(&block.qualifier).with_context(|| {
						format!(
							"While trying to detect UBOs in {:?}, at the declaration with index {ix}.",
							f.short_path,
						)
					})?;
					if storage == glsl::syntax::StorageQualifier::Uniform {
						self.ubos.push(GlslUbo {
							at: DeclId { file: f.id, ix },
							decl: block.clone(),
						});
					}
				}
			}
		}

		Ok(())
	}
}
// }}}
// {{{ Uniform & attribute detection
impl State {
	fn find_uniforms(&mut self) -> anyhow::Result<()> {
		let cloned = self.clone();
		for f in &cloned.files {
			self.find_uniforms_for_shallow(f.id, f.id)?;
			for i in &f.includes {
				self.find_uniforms_for_shallow(f.id, *i)?;
			}
		}

		Ok(())
	}

	fn find_uniforms_for_shallow(
		&mut self,
		look_for: GlslFileId,
		look_in: GlslFileId,
	) -> anyhow::Result<()> {
		let cloned = self.clone();
		let f = cloned.get(look_in);
		for (ix, decl) in (&f.syntax).into_iter().enumerate() {
			if let glsl::syntax::ExternalDeclaration::Declaration(
				glsl::syntax::Declaration::InitDeclaratorList(block),
			) = decl
			{
				if block.head.ty.qualifier.is_none() {
					continue;
				}

				ensure_only_one_name(block)?;

				let storage = ensure_only_storage(block.head.ty.qualifier.as_ref().unwrap())
					.with_context(|| {
						format!(
							"While trying to detect uniforms in {:?}, at the declaration with index {ix}.",
							f.short_path,
						)
					})?;

				if storage == glsl::syntax::StorageQualifier::Uniform {
					let mut decl = block.head.clone();
					decl.ty.ty.array_specifier =
						concat_array_specifiers(&decl.array_specifier, &decl.ty.ty.array_specifier);
					decl.array_specifier = None;
					self.get_mut(look_for).uniforms.push(GlslUniform {
						at: DeclId { file: f.id, ix },
						decl,
						location: 0,
					});
				} else if storage == glsl::syntax::StorageQualifier::In {
					let mut decl = block.head.clone();
					decl.ty.ty.array_specifier =
						concat_array_specifiers(&decl.array_specifier, &decl.ty.ty.array_specifier);
					decl.array_specifier = None;
					self.get_mut(look_for).attribs.push(GlslAttrib {
						at: DeclId { file: f.id, ix },
						decl,
						location: 0,
					});
				}
			}
		}

		Ok(())
	}
}
// }}}
// {{{ Uniform & attribute allocation
impl State {
	fn uniform_count(
		&self,
		id: GlslFileId,
		ty: &glsl::syntax::TypeSpecifier,
	) -> anyhow::Result<usize> {
		Ok(get_type_count(ty)? * self.uniform_count_non_array(id, &ty.ty)?)
	}

	fn uniform_count_non_array(
		&self,
		id: GlslFileId,
		ty: &glsl::syntax::TypeSpecifierNonArray,
	) -> anyhow::Result<usize> {
		use glsl::syntax::TypeSpecifierNonArray::*;
		Ok(match ty {
			Void => 0,
			Bool | Int | UInt | Float | Double => 1,
			Vec2 | Vec3 | Vec4 => 1,
			DVec2 | DVec3 | DVec4 => 1,
			BVec2 | BVec3 | BVec4 => 1,
			UVec2 | UVec3 | UVec4 => 1,
			Mat2 | Mat23 | Mat24 => 2,
			DMat2 | DMat23 | DMat24 => 2,
			Mat3 | Mat32 | Mat34 => 3,
			DMat3 | DMat32 | DMat34 => 3,
			Mat4 | Mat42 | Mat43 => 4,
			DMat4 | DMat42 | DMat43 => 4,
			Struct(s) => {
				let mut total = 0;
				for field in &s.fields {
					let count = self.uniform_count(id, &field.ty)?;
					for ident in &field.identifiers {
						total += get_array_spec_count(&ident.array_spec)? * count;
					}
				}
				total
			}
			_ => bail!("Type {ty:?} not supported."),
		})
	}

	fn alloc_uniforms(&mut self) -> anyhow::Result<()> {
		let cloned = self.clone();
		for f in &cloned.files {
			let mut loc = 0;
			for (ix, uniform) in f.uniforms.iter().enumerate() {
				self.get_mut(f.id).uniforms[ix].location = loc;
				loc += cloned.uniform_count(f.id, &uniform.decl.ty.ty)?;
			}

			loc = 0;
			for (ix, attrib) in f.attribs.iter().enumerate() {
				self.get_mut(f.id).attribs[ix].location = loc;
				loc += cloned.uniform_count(f.id, &attrib.decl.ty.ty)?;
			}
		}

		Ok(())
	}
}
// }}}

// Codegen
// {{{ Codegen
impl State {
	fn codegen(&self, out: &mut String) -> anyhow::Result<()> {
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

	fn codegen_program(&self, f: &GlslFile, out: &mut String) -> anyhow::Result<()> {
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

	fn codegen_type(
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

	fn codegen_type_non_array(
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

	fn codegen_block_to_struct(
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
	fn ubo_codegen(&self, out: &mut String) -> anyhow::Result<()> {
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
	fn codegen_uniforms(&self, f: &GlslFile, out: &mut String) -> anyhow::Result<()> {
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

	fn codegen_set_uniform(
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

	fn codegen_set_uniform_non_array(
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

	fn codegen_set_uniform_vec(out: &mut String, ty: &str, cols: usize) -> anyhow::Result<()> {
		writeln!(out, "OpenGL.Uniform{cols}{ty}v(loc, 1, auto_cast value)")?;

		Ok(())
	}

	fn codegen_set_uniform_mat(
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
	fn codegen_types(&self, f: &GlslFile, out: &mut String) -> anyhow::Result<()> {
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

	fn codegen_attribs(&self, f: &GlslFile, out: &mut String) -> anyhow::Result<()> {
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

	fn codegen_set_attrib(
		&self,
		id: GlslFileId,
		ty: &glsl::syntax::TypeSpecifier,
		stride: usize,
		instanced: bool,
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

// GLSL syntax helpers
// {{{ Ensure only storage is qualified
fn ensure_only_storage(
	qualifier: &glsl::syntax::TypeQualifier,
) -> anyhow::Result<glsl::syntax::StorageQualifier> {
	let mut storage_qualifier: Option<glsl::syntax::StorageQualifier> = None;
	for q in &qualifier.qualifiers {
		match q {
			glsl::syntax::TypeQualifierSpec::Storage(storage) => {
				if storage_qualifier.is_some() {
					bail!("Qualifier {:?} declares storage twice", qualifier);
				}

				storage_qualifier = Some(storage.clone());
			}
			_ => {
				bail!(
					"Qualifier {:?} not allowed (only storage qualifiers are).",
					q
				);
			}
		}
	}

	storage_qualifier.ok_or_else(|| anyhow!("Couldn't find storage qualifier for {qualifier:?}."))
}
// }}}
// {{{ Forced casts
fn expr_as_int(expr: &glsl::syntax::Expr) -> anyhow::Result<i32> {
	if let glsl::syntax::Expr::IntConst(i) = expr {
		Ok(*i)
	} else {
		bail!("Non integer const expressions are not yet supported.");
	}
}

fn arr_dimension_as_sized(
	dim: &glsl::syntax::ArraySpecifierDimension,
) -> anyhow::Result<&glsl::syntax::Expr> {
	match dim {
		glsl::syntax::ArraySpecifierDimension::Unsized => {
			bail!("Unsized array dimensions are not yet supported.");
		}
		glsl::syntax::ArraySpecifierDimension::ExplicitlySized(expr) => Ok(expr.as_ref()),
	}
}
// }}}
// {{{ Ensure only one identifier per initialization
fn ensure_only_one_name(decl: &glsl::syntax::InitDeclaratorList) -> anyhow::Result<()> {
	if !decl.tail.is_empty() {
		bail!(
			"Toplevel declarations with more than one identifier are not supported (found additional identifiers after {:?} for {:?}).",
			decl.head.name,
			decl
		)
	}

	Ok(())
}
// }}}
// {{{ Concat array specifiers
fn concat_array_specifiers(
	a: &Option<glsl::syntax::ArraySpecifier>,
	b: &Option<glsl::syntax::ArraySpecifier>,
) -> Option<glsl::syntax::ArraySpecifier> {
	match (a, b) {
		(Some(a), Some(b)) => Some(glsl::syntax::ArraySpecifier {
			dimensions: glsl::syntax::NonEmpty::from_non_empty_iter(
				a.dimensions.clone().into_iter().chain(b.dimensions.clone()),
			)
			.unwrap(),
		}),
		(Some(a), None) => Some(a.clone()),
		(None, Some(a)) => Some(a.clone()),
		(None, None) => None,
	}
}
// }}}
// {{{ Type count
fn get_type_count(ty: &glsl::syntax::TypeSpecifier) -> anyhow::Result<usize> {
	get_array_spec_count(&ty.array_specifier)
}

fn get_array_spec_count(arr: &Option<glsl::syntax::ArraySpecifier>) -> anyhow::Result<usize> {
	let mut result = 1;

	if let Some(arr) = &arr {
		for d in &arr.dimensions {
			result *= expr_as_int(arr_dimension_as_sized(d)?)? as usize;
		}
	}

	Ok(result)
}
// }}}
