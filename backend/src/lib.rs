#![feature(stmt_expr_attributes)]
use compile::Program;
use source_code::SourceCode;

pub mod builder;
mod compile;
mod error;
mod leb128;
mod parser;
pub mod source_code;
mod tokenizer;

pub fn compile(code: &mut SourceCode) -> Result<Program, String> {
  // parse
  let statements = tokenizer::expect_code(code).or_else(|err| Err(err.to_string(code)))?;
  // compile
  let program = Program::from_statements(statements);
  if let Ok(program) = program {
    return Ok(program);
  }

  let errors = program.unwrap_err();
  let mut error_message: Vec<String> = errors.iter().map(|err| err.to_string(code)).collect();
  error_message.push(format!(
    "\nFailed to compile due to {} error(s)",
    errors.len()
  ));

  Err(error_message.join("\n"))
}
