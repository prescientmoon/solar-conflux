use std::{
	ops::{Index, IndexMut},
	path::PathBuf,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GlslFileId(pub usize);

#[derive(Clone, Debug)]
pub struct GlslFile {
	pub id: GlslFileId,
	pub short_path: PathBuf,
	pub path: PathBuf,
	pub syntax: glsl::syntax::TranslationUnit,
	pub includes: Vec<GlslFileId>,

	// Stages
	pub has_vert: bool,
	pub has_frag: bool,

	// Layout
	pub attribs: Vec<GlslAttrib>,
	pub uniforms: Vec<GlslUniform>,
}

#[derive(Clone, Copy, Debug)]
pub struct DeclId {
	pub file: GlslFileId,
	pub ix: usize,
}

#[derive(Clone, Debug)]
pub struct GlslAttrib {
	pub at: DeclId,
	pub decl: glsl::syntax::SingleDeclaration,
	pub location: usize,
}

#[derive(Clone, Debug)]
pub struct GlslUniform {
	pub at: DeclId,
	pub decl: glsl::syntax::SingleDeclaration,
	pub location: usize,
}
#[derive(Clone)]
pub struct State {
	pub odin_package: String,
	pub odin_namespace_sep: String,
	pub base_path: PathBuf,
	pub files: Vec<GlslFile>,
	pub ubos: Vec<GlslUbo>,
}

#[derive(Clone, Debug)]
pub struct GlslUbo {
	pub at: DeclId,
	pub decl: glsl::syntax::Block,
}

// {{{ State helpers
impl State {
	pub fn get_decl(&self, decl: DeclId) -> &glsl::syntax::ExternalDeclaration {
		&self[decl.file].syntax.0.0[decl.ix]
	}
}

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
// }}}
