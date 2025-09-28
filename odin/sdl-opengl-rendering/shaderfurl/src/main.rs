#![allow(dead_code)]
use std::env;
use std::fs;
use std::path::Path;
use std::path::PathBuf;

use anyhow::Context;

mod glslgen;
mod layout;
mod odingen;
mod scope;
mod syntax;
mod types;

use types::*;

use crate::glslgen::ShaderStage;

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
			included_by: Vec::new(),
			vert_main: None,
			frag_main: None,
			used_attribs: Vec::new(),
			declared_attribs: Vec::new(),
			declared_uniforms: Vec::new(),
			used_uniforms: Vec::new(),
			declared_varyings: Vec::new(),
			used_varyings: Vec::new(),
			declared_functions: Vec::new(),
			used_functions: Vec::new(),
		});
	}

	Ok(results)
}
// }}}
// {{{ Main entrypoint
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
	state.find_ubos()?;
	state.find_inputs()?;
	state.detect_stages();
	state.find_references();
	state.alloc_inputs()?;

	let mut out = String::new();
	state.gen_module(&mut out)?;
	println!("---------- Odin");
	println!("{out}");
	// println!("{:?}", state.files[1].syntax);
	for f in &state.files {
		if f.vert_main.is_none() || f.frag_main.is_none() {
			continue;
		}

		let mut out = String::new();
		state.gen_glsl_shader(f.id, ShaderStage::Vert, &mut out)?;
		println!("---------- Vert: {:?}", &f.short_path);
		println!("{}", out);

		let mut out = String::new();
		state.gen_glsl_shader(f.id, ShaderStage::Frag, &mut out)?;
		println!("---------- Frag: {:?}", &f.short_path);
		println!("{}", out);
	}

	Ok(())
}
// }}}
