#![allow(dead_code)]
use anyhow::anyhow;
use anyhow::bail;

// GLSL syntax helpers
// {{{ Ensure only storage is qualified
pub fn ensure_only_storage(
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
pub fn expr_as_int(expr: &glsl::syntax::Expr) -> anyhow::Result<i32> {
	if let glsl::syntax::Expr::IntConst(i) = expr {
		Ok(*i)
	} else {
		bail!("Non integer const expressions are not yet supported.");
	}
}

pub fn arr_dimension_as_sized(
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
pub fn ensure_only_one_name(decl: &glsl::syntax::InitDeclaratorList) -> anyhow::Result<()> {
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
pub fn concat_array_specifiers(
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
pub fn get_type_count(ty: &glsl::syntax::TypeSpecifier) -> anyhow::Result<usize> {
	get_array_spec_count(&ty.array_specifier)
}

pub fn get_array_spec_count(arr: &Option<glsl::syntax::ArraySpecifier>) -> anyhow::Result<usize> {
	let mut result = 1;

	if let Some(arr) = &arr {
		for d in &arr.dimensions {
			result *= expr_as_int(arr_dimension_as_sized(d)?)? as usize;
		}
	}

	Ok(result)
}
// }}}
