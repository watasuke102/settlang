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
  wasm.append(&mut section(10, code_section_contents(&program.functions)?));
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

fn code_section_contents(functions: &Vec<compile::Function>) -> Result<Vec<u8>, String> {
  let mut contents = to_signed_leb128(functions.len() as i64);
  for function in functions {
    let mut locals = to_signed_leb128(function.variables.len() as i64);
    let mut expr = Vec::new();

    for var in &function.variables {
      locals.push(1); // local decl count (?)
      locals.push(to_wasm_numtype(&var.vartype)?);
      // initialize variable
      expr.append(&mut assemble_expr(&var.initial_value.expr_stack)?);
      expr.push(Inst::LocalSet as u8);
      expr.append(&mut to_signed_leb128(var.idx as i64));
    }
    for statement in &function.code {
      use compile::Statement::*;
      match statement {
        ExprStatement(e) => {
          expr.append(&mut assemble_expr(&e.expr_stack)?);
          if e.result_type != compile::Type::Void {
            expr.push(Inst::Drop as u8);
          }
        }
        Return(e) => {
          expr.append(&mut assemble_expr(&e.expr_stack)?);
          expr.push(Inst::Return as u8);
        }
      }
    }

    expr.push(Inst::End as u8);
    contents.append(&mut to_signed_leb128((locals.len() + expr.len()) as i64)); // func body size
    contents.append(&mut locals);
    contents.append(&mut expr);
  }
  Ok(contents)
}
fn assemble_expr(commands: &Vec<compile::ExprCommand>) -> Result<Vec<u8>, String> {
  let mut res = Vec::new();
  let mut current_type = Numtype::I32;
  for command in commands {
    use compile::ExprCommand::*;
    match command {
      Add => res.push(match current_type {
        Numtype::I32 => Inst::AddI32,
        Numtype::I64 => Inst::AddI64,
      } as u8),
      Sub => res.push(match current_type {
        Numtype::I32 => Inst::SubI32,
        Numtype::I64 => Inst::SubI64,
      } as u8),
      Mul => res.push(match current_type {
        Numtype::I32 => Inst::MulI32,
        Numtype::I64 => Inst::MulI64,
      } as u8),
      Div => res.push(match current_type {
        Numtype::I32 => Inst::DivSignedI32,
        Numtype::I64 => Inst::DivSignedI64,
      } as u8),
      PushImm(imm) => {
        res.push(Inst::ConstI32 as u8);
        res.append(&mut to_signed_leb128(*imm as i64))
      }
      PushVar(idx) | GetInitialValueFromArg(idx) => {
        res.push(Inst::LocalGet as u8);
        res.append(&mut to_signed_leb128(*idx as i64));
      }
      FnCall(idx) => {
        res.push(Inst::Call as u8);
        res.append(&mut to_signed_leb128(*idx as i64));
      }
      Cast(compile::Type::I32, compile::Type::I64) => {
        res.push(Inst::ExtendSignedI32ToI64 as u8);
        current_type = Numtype::I64;
      }
      Cast(compile::Type::I64, compile::Type::I32) => {
        res.push(Inst::WrapI64ToI32 as u8);
        current_type = Numtype::I32;
      }
      command => return Err(format!("Unknown command : {:?}", command)),
    }
  }
  Ok(res)
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
  End                  = 0x0b,
  Return               = 0x0f,
  Call                 = 0x10,
  Drop                 = 0x1a,
  LocalGet             = 0x20,
  LocalSet             = 0x21,
  ConstI32             = 0x41,
  AddI32               = 0x6a,
  SubI32               = 0x6b,
  MulI32               = 0x6c,
  DivSignedI32         = 0x6d,
  AddI64               = 0x7c,
  SubI64               = 0x7d,
  MulI64               = 0x7e,
  DivSignedI64         = 0x7f,
  WrapI64ToI32         = 0xa7,
  ExtendSignedI32ToI64 = 0xac,
}
