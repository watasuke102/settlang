#![feature(stmt_expr_attributes)]
use compile::Program;
use source_code::SourceCode;
use tokenizer::StatementKind;

pub mod builder;
mod compile;
mod error;
mod leb128;
mod parser;
pub mod source_code;
mod tokenizer;

fn _print_statement(statement: &StatementKind, indent: usize) {
  let indent_space = "  ".repeat(indent);
  match statement {
    StatementKind::FnDecl(func) => {
      println!(
        "{}[declare] fn {}({}){} {{",
        indent_space,
        func.name,
        func
          .args
          .iter()
          .map(|e| format!("{}: {}", e.name, e.vartype.type_ident))
          .collect::<Vec<String>>()
          .join(", "),
        if let Some(ref return_type) = func.return_type {
          format!(" -> {}", return_type.type_ident)
        } else {
          "".to_string()
        },
      );
      func
        .code
        .iter()
        .for_each(|s| _print_statement(&s.kind, indent + 1));
      println!("{}}}", indent_space);
    }
    StatementKind::VarDecl(var) => println!(
      "{}[variable] {}: type={:?}, initial_value={:?}",
      indent_space, var.name, var.vartype, var.initial_value
    ),
    StatementKind::ExprStatement(expr) => {
      println!(
        "{}[expr] {:?} (eval: {:?})",
        indent_space,
        expr,
        expr.element._eval()
      )
    }
    StatementKind::Return(retval) => println!(
      "{}[return] retval: {:?} (eval: {:?})",
      indent_space,
      retval,
      retval.clone().and_then(|e| e.element._eval().ok())
    ),
  }
}

pub fn compile(code: &mut SourceCode) -> Result<Program, String> {
  // parse
  let statements = tokenizer::expect_code(code).or_else(|err| {
    let error_pos = code.lines_and_cols();
    Err(format!(
      "[error] {} -> {:?}\n{}\nFailed to parse",
      error_pos,
      err,
      code.pointed_string(&error_pos)
    ))
  })?;
  // compile
  let program = Program::from_statements(statements);
  if let Ok(program) = program {
    return Ok(program);
  }
  let errors = program.unwrap_err();
  let mut error_message: Vec<String> = errors
    .iter()
    .map(|err| {
      use error::CompileError::*;
      match err {
        UndefinedVariable(name, pos) => format!(
          "[error] {} -> variable `{}` is undefined\n{}",
          pos,
          name,
          code.pointed_string(pos)
        ),
        UndefinedFunction(name, pos) => format!(
          "[error] {} -> function `{}` is undefined\n{}",
          pos,
          name,
          code.pointed_string(pos)
        ),
        ReturnNotFound(name, return_type) => format!(
          "[error] function `{}` has return type ({:?}) but does not have return statement",
          name, return_type,
        ),
        DuplicatedDecl(name, pos) => format!(
          "[error] {} -> `{}` is already defined\n{}",
          pos,
          name,
          code.pointed_string(pos)
        ),
        WrongArgumentLen(name, expect, actual, pos) => format!(
          "[error] {} -> function `{}` takes {} arg(s) but {} arg(s) was passed\n{}",
          pos,
          name,
          expect,
          actual,
          code.pointed_string(pos),
        ),
        InvalidType(name, pos) => format!(
          "[error] {} -> `{}` is invalid type name\n{}",
          pos,
          name,
          code.pointed_string(pos),
        ),
        InvalidCast(from, to, begin, end) => format!(
          "[error] {} -> cannot cast from {:?} to {:?}\n{}",
          begin,
          from,
          to,
          code.ranged_string(begin, end)
        ),
        MismatchReturnExprType(expect, actual, begin, end) => format!(
        "[error] {} -> mismatched type; function return type is {:?} but return value is {:?}\n{}",
        begin,
        expect,
        actual,
        code.ranged_string(begin, end)
      ),
        GlobalStatementWithMain(lines) => {
          let lines = lines
            .into_iter()
            .map(|line| line.to_string())
            .collect::<Vec<String>>()
            .join(", ");
          [
            // insert width that is the same width of "[error]" before "lines: {}" to align output
            format!("[error] Statement(s) found in the outside of main()"),
            format!("        lines: {}", lines),
          ]
          .join("\n")
        }
      }
    })
    .collect();
  error_message.push(format!(
    "Failed to compile due to {} error(s)",
    errors.len()
  ));

  Err(error_message.join("\n"))
}
