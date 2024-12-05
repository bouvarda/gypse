//!
//! Entry point to use compiler as a library
//!
use ast::{Import, Program};
use std::collections::HashSet;
use std::fs::File;
use std::io::{Error, Write};
use std::path::Path;
use types::{SOURCE_FILE_EXTENSION, STDLIB_ROOT};

use crate::codegen::CodeGen;
use crate::executable::Executable;
use crate::utils::print_ast;
use log::info;

mod ast;
mod bytecode;
mod codegen;
mod executable;
mod inference;
mod naive_allocator;
mod parser;
mod scope;
mod transformer;
mod types;
mod utils;

pub fn load_program_tree(
    import: &Import,
    source_path: &Path,
    stdlib_path: &Path,
    visited: &mut HashSet<Import>,
) -> Program {
    info!("Load {}\n", import.parts.join("."));

    let first_part = import.parts.first().unwrap();
    let mut relative_path = if first_part == STDLIB_ROOT {
        stdlib_path.join(&import.parts[1..].join("/"))
    } else {
        source_path.join(&import.parts[0..].join("/"))
    };
    relative_path.set_extension(&SOURCE_FILE_EXTENSION[1..]);
    let mut program: Program = parser::parse_file(relative_path.as_path());
    for sub_import in &program.imports {
        if !visited.contains(sub_import) {
            visited.insert(sub_import.clone());
            let sub_program = load_program_tree(sub_import, source_path, stdlib_path, visited);
            for class in sub_program.classes {
                program.classes.push(class);
            }
        }
    }
    program
}

pub fn compile(
    main_source_path: &Path,
    stdlib_path: &Path,
    binary_path: &Path,
    debug_mode: bool,
) -> Result<(), Error> {
    let main_import = Import::new(main_source_path.file_name().unwrap().to_str().unwrap());
    let program = load_program_tree(
        &main_import,
        main_source_path.parent().unwrap(),
        stdlib_path,
        &mut HashSet::new(),
    );
    if debug_mode {
        print_ast(&program);
    }

    // run compiler on expanded AST
    let mut codegen = CodeGen::new(&program);
    let (bytecode, entry_label) = codegen.gen_program();

    // output text assembly
    let asm = bytecode.print_text_assembly(entry_label.as_str());
    let mut asm_path = binary_path.to_path_buf();
    asm_path.set_extension("asm");
    let mut output: File = File::create(asm_path)?;
    output.write(asm.as_bytes())?;

    // output executable assembly
    let mut executable = Executable::new(&bytecode);
    executable.gen_executable(&entry_label, binary_path, true)?;
    Ok(())
}
