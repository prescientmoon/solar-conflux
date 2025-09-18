use std::ops::{Index, IndexMut};

use glsl::syntax::ExternalDeclaration;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GlslFileId(pub usize);

#[derive(Clone, Debug)]
pub struct GlslFile {
	pub id: GlslFileId,
	pub short_path: std::path::PathBuf,
	pub path: std::path::PathBuf,
	pub syntax: glsl::syntax::TranslationUnit,
	pub includes: Vec<GlslFileId>,
	pub included_by: Vec<GlslFileId>,

	// Stages
	pub has_vert: bool,
	pub has_frag: bool,

	// Declarations
	pub declared_uniforms: Vec<GlslUniformDecl>,
	pub used_uniforms: Vec<GlslUsedUniform>,
	pub declared_attribs: Vec<GlslAttribDecl>,
	pub used_attribs: Vec<GlslUsedAttrib>,
	pub declared_varyings: Vec<GlslVaryingDecl>,
	pub used_varyings: Vec<GlslUsedVarying>,
	pub declared_functions: Vec<GlslFunctionDecl>,
	pub used_functions: Vec<GlslUsedFunction>,
}

#[derive(Clone, Copy, Debug)]
pub struct GlslDeclId {
	pub file: GlslFileId,
	ix: usize,
}

impl GlslDeclId {
	pub fn new(file: GlslFileId, ix: usize) -> Self {
		Self { file, ix }
	}
}

// {{{ Uniforms
#[derive(Clone, Debug)]
pub struct GlslUsedUniform {
	pub location: usize,
	pub id: GlslUniformId,
}

#[derive(Clone, Debug)]
pub struct GlslUniformDecl {
	pub id: GlslUniformId,
	pub at: GlslDeclId,
	pub decl: glsl::syntax::SingleDeclaration,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct GlslUniformId(GlslFileId, usize);

impl GlslUniformId {
	pub fn new(glsl_file_id: GlslFileId, ix: usize) -> Self {
		Self(glsl_file_id, ix)
	}
}
// }}}
// {{{ Attributes
#[derive(Clone, Debug)]
pub struct GlslUsedAttrib {
	pub location: usize,
	pub id: GlslAttribId,
}

#[derive(Clone, Debug)]
pub struct GlslAttribDecl {
	pub id: GlslAttribId,
	pub at: GlslDeclId,
	pub decl: glsl::syntax::SingleDeclaration,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct GlslAttribId(GlslFileId, usize);

impl GlslAttribId {
	pub fn new(glsl_file_id: GlslFileId, ix: usize) -> Self {
		Self(glsl_file_id, ix)
	}
}
// }}}
// {{{ Varying-s
#[derive(Clone, Debug)]
pub struct GlslUsedVarying {
	pub location: usize,
	pub id: GlslVaryingId,
	pub frag_referenced: bool,
	pub vert_referenced: bool,
}

#[derive(Clone, Debug)]
pub struct GlslVaryingDecl {
	pub id: GlslVaryingId,
	pub at: GlslDeclId,
	pub decl: glsl::syntax::SingleDeclaration,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct GlslVaryingId(GlslFileId, usize);

impl GlslVaryingId {
	pub fn new(glsl_file_id: GlslFileId, ix: usize) -> Self {
		Self(glsl_file_id, ix)
	}
}
// }}}
// {{{ Functions
#[derive(Clone, Debug)]
pub struct GlslUsedFunction {
	pub id: GlslFunctionId,
}

#[derive(Clone, Debug)]
pub struct GlslFunctionDecl {
	pub id: GlslFunctionId,
	pub at: GlslDeclId,
	pub def: glsl::syntax::FunctionDefinition,
	pub references_functions: Vec<GlslFunctionId>,
	pub references_varyings: Vec<GlslVaryingId>,
	pub references_uniforms: Vec<GlslUniformId>,
	pub references_attribs: Vec<GlslAttribId>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct GlslFunctionId(pub GlslFileId, usize);

impl GlslFunctionId {
	pub fn new(glsl_file_id: GlslFileId, ix: usize) -> Self {
		Self(glsl_file_id, ix)
	}
}
// }}}

#[derive(Clone)]
pub struct State {
	pub odin_package: String,
	pub odin_namespace_sep: String,
	pub base_path: std::path::PathBuf,
	pub files: Vec<GlslFile>,
	pub ubos: Vec<GlslUbo>,
}

#[derive(Clone, Debug)]
pub struct GlslUbo {
	pub at: GlslDeclId,
	pub decl: glsl::syntax::Block,
}

// {{{ State indexing
impl Index<GlslFileId> for State {
	type Output = GlslFile;
	fn index(&self, index: GlslFileId) -> &Self::Output {
		&self.files[index.0]
	}
}

impl IndexMut<GlslFileId> for State {
	fn index_mut(&mut self, index: GlslFileId) -> &mut Self::Output {
		&mut self.files[index.0]
	}
}

impl Index<GlslUniformId> for State {
	type Output = GlslUniformDecl;
	fn index(&self, index: GlslUniformId) -> &Self::Output {
		&self[index.0].declared_uniforms[index.1]
	}
}

impl Index<GlslAttribId> for State {
	type Output = GlslAttribDecl;
	fn index(&self, index: GlslAttribId) -> &Self::Output {
		&self[index.0].declared_attribs[index.1]
	}
}

impl Index<GlslVaryingId> for State {
	type Output = GlslVaryingDecl;
	fn index(&self, index: GlslVaryingId) -> &Self::Output {
		&self[index.0].declared_varyings[index.1]
	}
}

impl Index<GlslFunctionId> for State {
	type Output = GlslFunctionDecl;
	fn index(&self, index: GlslFunctionId) -> &Self::Output {
		&self[index.0].declared_functions[index.1]
	}
}

impl IndexMut<GlslFunctionId> for State {
	fn index_mut(&mut self, index: GlslFunctionId) -> &mut Self::Output {
		&mut self[index.0].declared_functions[index.1]
	}
}

impl Index<GlslDeclId> for State {
	type Output = ExternalDeclaration;
	fn index(&self, index: GlslDeclId) -> &Self::Output {
		&self[index.file].syntax.0.0[index.ix]
	}
}
// }}}
// {{{ Iteration
impl State {
	pub fn external_declarations_for(
		&self,
		id: GlslFileId,
	) -> impl Iterator<Item = &ExternalDeclaration> {
		let f = &self[id];
		(&f.syntax)
			.into_iter()
			.chain(f.includes.iter().flat_map(|i| {
				let included = &self[*i];
				(&included.syntax).into_iter()
			}))
	}
}
// }}}
