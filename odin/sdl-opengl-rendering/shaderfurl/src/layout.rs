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
	pub fn find_uniforms(&mut self) -> anyhow::Result<()> {
		let cloned = self.clone();
		for f in &cloned.files {
			self.find_uniforms_for_shallow(f.id, f.id)?;
			for i in &f.includes {
				self.find_uniforms_for_shallow(f.id, *i)?;
			}
		}

		Ok(())
	}

	pub fn find_uniforms_for_shallow(
		&mut self,
		look_for: GlslFileId,
		look_in: GlslFileId,
	) -> anyhow::Result<()> {
		let cloned = self.clone();
		let f = &cloned[look_in];
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
					self[look_for].uniforms.push(GlslUniform {
						at: DeclId { file: f.id, ix },
						decl,
						location: 0,
					});
				} else if storage == glsl::syntax::StorageQualifier::In {
					let mut decl = block.head.clone();
					decl.ty.ty.array_specifier =
						concat_array_specifiers(&decl.array_specifier, &decl.ty.ty.array_specifier);
					decl.array_specifier = None;
					self[look_for].attribs.push(GlslAttrib {
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
	pub fn uniform_count(
		&self,
		id: GlslFileId,
		ty: &glsl::syntax::TypeSpecifier,
	) -> anyhow::Result<usize> {
		Ok(get_type_count(ty)? * self.uniform_count_non_array(id, &ty.ty)?)
	}

	pub fn uniform_count_non_array(
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

	pub fn alloc_uniforms(&mut self) -> anyhow::Result<()> {
		let cloned = self.clone();
		for f in &cloned.files {
			let mut loc = 0;
			for (ix, uniform) in f.uniforms.iter().enumerate() {
				self[f.id].uniforms[ix].location = loc;
				loc += cloned.uniform_count(f.id, &uniform.decl.ty.ty)?;
			}

			loc = 0;
			for (ix, attrib) in f.attribs.iter().enumerate() {
				self[f.id].attribs[ix].location = loc;
				loc += cloned.uniform_count(f.id, &attrib.decl.ty.ty)?;
			}
		}

		Ok(())
	}
}
// }}}
