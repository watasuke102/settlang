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
    for var in &function.variables {
      locals.push(1); // local decl count (?)
      locals.push(to_wasm_numtype(&var.vartype)?);
    }
    let mut expr = Vec::new();
    expr.append(&mut assemble_statements(&function.code)?);
    expr.push(Inst::End as u8);
    contents.append(&mut to_signed_leb128((locals.len() + expr.len()) as i64)); // func body size
    contents.append(&mut locals);
    contents.append(&mut expr);
  }
  Ok(contents)
}
fn assemble_statements(statements: &Vec<compile::Statement>) -> Result<Vec<u8>, String> {
  let mut expr = Vec::new();
  for statement in statements.iter() {
    use compile::Statement::*;
    match statement {
      VarInitialize(idx, initial_value) => {
        expr.append(&mut assemble_expr(&initial_value.expr_stack)?);
        expr.push(Inst::LocalSet as u8);
        expr.append(&mut to_signed_leb128(*idx as i64));
      }
      ExprStatement(e, should_drop) => {
        expr.append(&mut assemble_expr(&e.expr_stack)?);
        if e.result_type != compile::Type::Void && *should_drop {
          expr.push(Inst::Drop as u8);
        }
      }
      Return(e) => {
        expr.append(&mut assemble_expr(&e.expr_stack)?);
        expr.push(Inst::Return as u8);
      }
      SetterCall(mutate_info) => {
        // push arguments
        expr.append(&mut assemble_expr(&mutate_info.arg_stack)?);
        // call function as setter; top of the stack will become a new value of variable
        expr.push(Inst::Call as u8);
        expr.append(&mut to_signed_leb128(mutate_info.setter as i64));
        // mutate local (variable)
        expr.push(Inst::LocalSet as u8);
        expr.append(&mut to_signed_leb128(mutate_info.var as i64));
      }
      ForLoop(for_loop) => {
        expr.append(&mut assemble_expr(&for_loop.begin.expr_stack)?);
        expr.push(Inst::LocalSet as u8);
        expr.append(&mut to_signed_leb128(for_loop.cnt_var_idx as i64));

        expr.push(Inst::Loop as u8);
        expr.push(0x40); // void; 'for' block does not (is not expected to) return value

        expr.append(&mut assemble_statements(&for_loop.code)?);

        // i = 'i+1'
        expr.push(Inst::LocalGet as u8);
        expr.append(&mut to_signed_leb128(for_loop.cnt_var_idx as i64));
        if for_loop.end.result_type == compile::Type::I32 {
          expr.push(Inst::ConstI32 as u8);
          expr.append(&mut to_signed_leb128(1));
          expr.push(Inst::AddI32 as u8);
        } else if for_loop.end.result_type == compile::Type::I64 {
          expr.push(Inst::ConstI64 as u8);
          expr.append(&mut to_signed_leb128(1));
          expr.push(Inst::AddI64 as u8);
        } else {
          unreachable!();
        }
        // 'i =' i+1
        expr.push(Inst::LocalTee as u8);
        expr.append(&mut to_signed_leb128(for_loop.cnt_var_idx as i64));
        // if i == end (i is remained on the stack)
        expr.append(&mut assemble_expr(&for_loop.end.expr_stack)?);
        if for_loop.end.result_type == compile::Type::I32 {
          expr.push(Inst::NonEqI32 as u8);
        } else if for_loop.end.result_type == compile::Type::I64 {
          expr.push(Inst::NonEqI64 as u8);
        } else {
          unreachable!();
        }
        expr.push(Inst::BranchIf as u8);
        expr.push(0); // break depth == 1
        expr.push(Inst::End as u8);
      }
    }
  }
  Ok(expr)
}
fn assemble_expr(commands: &Vec<compile::ExprCommand>) -> Result<Vec<u8>, String> {
  let mut res = Vec::new();
  let mut current_type = Numtype::I64;
  macro_rules! map {
    ($i32_inst:expr, $i64_inst:expr) => {
      match current_type {
          Numtype::I32 => $i32_inst,
          Numtype::I64 => $i64_inst,
        } as u8
      }
    }
  for command in commands {
    use compile::ExprCommand::*;
    use Inst::*;
    match command {
      Add => res.push(map!(AddI32, AddI64)),
      Sub => res.push(map!(SubI32, SubI64)),
      Mul => res.push(map!(MulI32, MulI64)),
      Div => res.push(map!(DivSignedI32, DivSignedI64)),
      Mod => res.push(map!(RemSignedI32, RemSignedI64)),
      Less => res.push(map!(LessSignedI32, LessSignedI64)),
      LessEq => res.push(map!(LessEqSignedI32, LessEqSignedI64)),
      Greater => res.push(map!(GreaterSignedI32, GreaterSignedI64)),
      GreaterEq => res.push(map!(GreaterEqSignedI32, GreaterEqSignedI64)),
      Eq => res.push(map!(EqI32, EqI64)),
      NonEq => res.push(map!(NonEqI32, NonEqI64)),
      BitOr => res.push(map!(OrI32, OrI64)),
      BitAnd => res.push(map!(AndI32, AndI64)),
      LogicOr | LogicAnd => unreachable!(),
      IfExpr(if_expr) => {
        res.append(&mut assemble_expr(&if_expr.cond.expr_stack)?);
        res.push(If as u8);
        res.push(if if_expr.result_type == compile::Type::Void {
          0x40
        } else {
          to_wasm_numtype(&if_expr.result_type)?
        });
        res.append(&mut assemble_statements(&if_expr.then)?);
        if let Some(ref otherwise) = if_expr.otherwise {
          res.push(Else as u8);
          res.append(&mut assemble_statements(otherwise)?);
        }
        res.push(End as u8);
      }
      ImmI32(imm) => {
        res.push(ConstI32 as u8);
        res.append(&mut to_signed_leb128(*imm as i64));
        current_type = Numtype::I32;
      }
      ImmI64(imm) => {
        res.push(ConstI64 as u8);
        res.append(&mut to_signed_leb128(*imm as i64));
        current_type = Numtype::I64;
      }
      PushVar(idx) => {
        res.push(LocalGet as u8);
        res.append(&mut to_signed_leb128(*idx as i64));
      }
      FnCall(idx) => {
        res.push(Call as u8);
        res.append(&mut to_signed_leb128(*idx as i64));
      }
      CastI32ToI64 => {
        res.push(ExtendSignedI32ToI64 as u8);
        current_type = Numtype::I64;
      }
      CastI64ToI32 => {
        res.push(WrapI64ToI32 as u8);
        current_type = Numtype::I32;
      }
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
    _ => Err(format!("Invalid type ({:?})", compile_type)),
  }
}

enum Inst {
  Loop                 = 0x03,
  If                   = 0x04,
  Else                 = 0x05,
  End                  = 0x0b,
  BranchIf             = 0x0d,
  Return               = 0x0f,
  Call                 = 0x10,
  Drop                 = 0x1a,
  LocalGet             = 0x20,
  LocalSet             = 0x21,
  LocalTee             = 0x22,
  ConstI32             = 0x41,
  ConstI64             = 0x42,

  EqI32                = 0x46,
  NonEqI32             = 0x47,
  LessSignedI32        = 0x48,
  GreaterSignedI32     = 0x4a,
  LessEqSignedI32      = 0x4c,
  GreaterEqSignedI32   = 0x4e,
  EqI64                = 0x51,
  NonEqI64             = 0x52,
  LessSignedI64        = 0x53,
  GreaterSignedI64     = 0x55,
  LessEqSignedI64      = 0x57,
  GreaterEqSignedI64   = 0x59,

  AddI32               = 0x6a,
  SubI32               = 0x6b,
  MulI32               = 0x6c,
  DivSignedI32         = 0x6d,
  RemSignedI32         = 0x6f,
  AndI32               = 0x71,
  OrI32                = 0x72,
  AddI64               = 0x7c,
  SubI64               = 0x7d,
  MulI64               = 0x7e,
  DivSignedI64         = 0x7f,
  RemSignedI64         = 0x81,
  AndI64               = 0x83,
  OrI64                = 0x84,

  WrapI64ToI32         = 0xa7,
  ExtendSignedI32ToI64 = 0xac,
}
