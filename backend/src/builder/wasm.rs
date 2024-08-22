use crate::{compile, leb128::to_signed_leb128};

pub fn build(program: compile::Program) -> Result<Vec<u8>, String> {
  let mut wasm = vec![
    0x00, 0x61, 0x73, 0x6d, // magic   = \0 + 'asm'
    0x01, 0x00, 0x00, 0x00, // version = 1
  ];
  wasm.append(&mut section(1, type_section_contents(&program.functions)?));
  wasm.append(&mut section(
    3,
    function_section_contents(&program.functions),
  ));
  wasm.append(&mut section(
    7,
    export_section_contents(&program.functions)?,
  ));
  wasm.append(&mut section(10, code_section_contents(&program.functions)));
  Ok(wasm)
}

fn type_section_contents(functions: &Vec<compile::Function>) -> Result<Vec<u8>, String> {
  let mut contents = to_signed_leb128(functions.len() as i64);
  for function in functions {
    // Function Type begin
    contents.push(0x60);
    // num params (number of arguments)
    contents.append(&mut to_signed_leb128(function.args.len() as i64));
    for arg_type in &function.args {
      contents.push(to_wasm_numtype(arg_type)?)
    }
    // num results (number of arguments)
    if function.return_type == compile::Type::Void {
      contents.push(0);
    } else {
      contents.push(1);
      contents.push(to_wasm_numtype(&function.return_type)?);
    }
  }
  Ok(contents)
}

fn function_section_contents(functions: &Vec<compile::Function>) -> Vec<u8> {
  let mut contents = to_signed_leb128(functions.len() as i64);
  for i in 0..functions.len() as i64 {
    // function[i] signature index
    contents.append(&mut to_signed_leb128(i));
  }
  contents
}

fn export_section_contents(functions: &Vec<compile::Function>) -> Result<Vec<u8>, String> {
  let main_function = functions
    .iter()
    .find(|f| f.name == "main")
    .ok_or("Function named 'main' is not found".to_string())?;
  let mut contents = vec![1 /* num exports (export only one function) */];
  // function name
  let export_name = "main";
  contents.append(&mut to_signed_leb128(export_name.len() as i64));
  contents.extend_from_slice(export_name.as_bytes());
  contents.push(0); // exported item is function
  contents.append(&mut to_signed_leb128(main_function.idx as i64));
  Ok(contents)
}

fn code_section_contents(functions: &Vec<compile::Function>) -> Vec<u8> {
  // TODO
  vec![]
}

fn section(section_id: u8, mut contents: Vec<u8>) -> Vec<u8> {
  let mut section = vec![section_id];
  section.append(&mut to_signed_leb128(contents.len() as i64));
  section.append(&mut contents);
  section
}
enum Numtype {
  I32 = 0x7f,
  I64 = 0x7e,
}
fn to_wasm_numtype(compile_type: &compile::Type) -> Result<u8, String> {
  match compile_type {
    compile::Type::I32 => Ok(Numtype::I32 as u8),
    compile::Type::I64 => Ok(Numtype::I64 as u8),
    _ => Err(format!("Invalid type : {:?}", compile_type)),
  }
}

enum Inst {
  End = 0x0b,
}
