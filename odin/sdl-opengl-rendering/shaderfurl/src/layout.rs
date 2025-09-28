use std::iter::once;

use anyhow::Context;
use anyhow::bail;

use crate::syntax::*;
use crate::types::*;

// {{{ UBO detection
impl State {
	pub fn find_ubos(&mut self) -> anyhow::Result<()> {
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
							at: GlslDeclId::new(f.id, ix),
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
	pub fn find_inputs(&mut self) -> anyhow::Result<()> {
		let cloned = self.clone();
		for f in &cloned.files {
			for (ix, decl) in (&f.syntax).into_iter().enumerate() {
				use glsl::syntax::{Declaration::*, ExternalDeclaration::*};
				match decl {
					Preprocessor(_) => {}       // already handled one pass ahead
					Declaration(Block(_)) => {} // covered by other passes
					FunctionDefinition(def) => {
						let id = GlslFunctionId::new(f.id, self[f.id].declared_functions.len());
						self[f.id].declared_functions.push(GlslFunctionDecl {
							at: GlslDeclId::new(f.id, ix),
							def: def.clone(),
							id,
							references_functions: Vec::new(),
							references_varyings: Vec::new(),
							references_uniforms: Vec::new(),
							references_attribs: Vec::new(),
						});

						for downstream in f.included_by.iter().chain(once(&f.id)) {
							let ix = self[*downstream].used_functions.len();
							self[*downstream].used_functions.push(GlslUsedFunction {
								decl_id: id,
								id: GlslUsedFunctionId::new(f.id, ix),
							});
						}
					}
					Declaration(Precision(_, _)) => {
						bail!("Precision qualifiers are not supported")
					}
					Declaration(FunctionPrototype(_)) => {
						bail!("Function prototypes are not supported")
					}
					Declaration(Global(_, _)) => {
						bail!("Globals are not supported")
					}
					Declaration(InitDeclaratorList(block)) => {
						if block.head.ty.qualifier.is_none() {
							continue;
						}

						ensure_only_one_name(block)?;

						let mut decl = block.head.clone();
						decl.ty.ty.array_specifier = concat_array_specifiers(
							&decl.array_specifier,
							&decl.ty.ty.array_specifier,
						);
						decl.array_specifier = None;

						let storage =
							ensure_only_storage(block.head.ty.qualifier.as_ref().unwrap())?;
						if storage == glsl::syntax::StorageQualifier::Uniform {
							let id = GlslUniformId::new(f.id, self[f.id].declared_uniforms.len());
							self[f.id].declared_uniforms.push(GlslUniformDecl {
								at: GlslDeclId::new(f.id, ix),
								decl,
								id,
							});

							for downstream in f.included_by.iter().chain(once(&f.id)) {
								self[*downstream]
									.used_uniforms
									.push(GlslUsedUniform { location: 0, id });
							}
						} else if storage == glsl::syntax::StorageQualifier::In {
							let id = GlslAttribId::new(f.id, self[f.id].declared_attribs.len());
							self[f.id].declared_attribs.push(GlslAttribDecl {
								at: GlslDeclId::new(f.id, ix),
								decl,
								id,
							});

							for downstream in f.included_by.iter().chain(once(&f.id)) {
								self[*downstream]
									.used_attribs
									.push(GlslUsedAttrib { location: 0, id });
							}
						} else if storage == glsl::syntax::StorageQualifier::Varying {
							let id = GlslVaryingId::new(f.id, self[f.id].declared_varyings.len());
							self[f.id].declared_varyings.push(GlslVaryingDecl {
								at: GlslDeclId::new(f.id, ix),
								decl,
								id,
							});

							for downstream in f.included_by.iter().chain(once(&f.id)) {
								self[*downstream].used_varyings.push(GlslUsedVarying {
									location: 0,
									id,
									frag_referenced: false,
									vert_referenced: false,
								});
							}
						} else if storage == glsl::syntax::StorageQualifier::Out {
							// TODO
						} else {
							bail!("Storage qualifier {storage:?} not supported")
						}
					}
				}
			}
		}

		Ok(())
	}
}
// }}}
// {{{ Uniform & attribute allocation
impl State {
	pub fn input_count(
		&self,
		id: GlslFileId,
		ty: &glsl::syntax::TypeSpecifier,
	) -> anyhow::Result<usize> {
		Ok(get_type_count(ty)? * self.input_count_non_array(id, &ty.ty)?)
	}

	pub fn input_count_non_array(
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
			IVec2 | IVec3 | IVec4 => 1,
			Mat2 | Mat23 | Mat24 => 2,
			DMat2 | DMat23 | DMat24 => 2,
			Mat3 | Mat32 | Mat34 => 3,
			DMat3 | DMat32 | DMat34 => 3,
			Mat4 | Mat42 | Mat43 => 4,
			DMat4 | DMat42 | DMat43 => 4,
			Struct(s) => {
				let mut total = 0;
				for field in &s.fields {
					let count = self.input_count(id, &field.ty)?;
					for ident in &field.identifiers {
						total += get_array_spec_count(&ident.array_spec)? * count;
					}
				}
				total
			}
			_ => bail!("Type {ty:?} not supported."),
		})
	}

	pub fn alloc_inputs(&mut self) -> anyhow::Result<()> {
		let cloned = self.clone();
		for f in &mut self.files {
			let mut loc = 0;
			for uniform in f.used_uniforms.iter_mut() {
				uniform.location = loc;
				loc += cloned.input_count(f.id, &cloned[uniform.id].decl.ty.ty)?;
			}

			loc = 0;
			for attrib in f.used_attribs.iter_mut() {
				attrib.location = loc;
				loc += cloned.input_count(f.id, &cloned[attrib.id].decl.ty.ty)?;
			}

			loc = 0;
			for varying in f.used_varyings.iter_mut() {
				varying.location = loc;
				loc += cloned.input_count(f.id, &cloned[varying.id].decl.ty.ty)?;
			}
		}

		Ok(())
	}
}
// }}}
// {{{ Type sizes
impl State {
	/// Computes the packed size of a GLSL type.
	pub fn type_sizeof(
		&self,
		id: GlslFileId,
		ty: &glsl::syntax::TypeSpecifier,
	) -> anyhow::Result<usize> {
		Ok(get_type_count(ty)? * self.type_sizeof_non_array(id, &ty.ty)?)
	}

	pub fn type_sizeof_non_array(
		&self,
		id: GlslFileId,
		ty: &glsl::syntax::TypeSpecifierNonArray,
	) -> anyhow::Result<usize> {
		use glsl::syntax::TypeSpecifierNonArray::*;
		Ok(match ty {
			Void => 0,
			Bool => 1,
			BVec2 => 2,
			BVec3 => 3,
			BVec4 => 4,
			Float => 5,
			Vec2 => 10,
			Vec3 => 15,
			Vec4 => 20,
			UInt => 5,
			UVec2 => 10,
			UVec3 => 15,
			UVec4 => 20,
			Int => 5,
			IVec2 => 10,
			IVec3 => 15,
			IVec4 => 20,
			Double => 6,
			DVec2 => 12,
			DVec3 => 18,
			DVec4 => 24,
			Mat2 => 2 * 2 * 5,
			Mat23 => 2 * 3 * 5,
			Mat24 => 2 * 4 * 5,
			Mat3 => 3 * 3 * 5,
			Mat32 => 3 * 2 * 5,
			Mat34 => 3 * 4 * 5,
			Mat4 => 4 * 4 * 5,
			Mat42 => 4 * 2 * 5,
			Mat43 => 4 * 3 * 5,
			DMat2 => 2 * 2 * 6,
			DMat23 => 2 * 3 * 6,
			DMat24 => 2 * 4 * 6,
			DMat3 => 3 * 3 * 6,
			DMat32 => 3 * 2 * 6,
			DMat34 => 3 * 4 * 6,
			DMat4 => 4 * 4 * 6,
			DMat42 => 4 * 2 * 6,
			DMat43 => 4 * 3 * 6,
			Struct(s) => {
				let mut total = 0;
				for field in &s.fields {
					for ident in &field.identifiers {
						total += self.type_sizeof(id, &field.ty)?
							* get_array_spec_count(&ident.array_spec)?;
					}
				}
				total
			}
			// TODO: support named structs here
			_ => bail!("Type {ty:?} not supported."),
		})
	}
}
// }}}
