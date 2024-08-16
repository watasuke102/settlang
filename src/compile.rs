use std::collections::HashMap;

use crate::{error::CompileError, tokenizer};

type CompileResult<T> = Result<T, Vec<CompileError>>;

#[derive(Debug)]
pub struct Program<'p> {
  functions: HashMap<String, Function<'p>>,
}

#[derive(Debug)]
enum Type {
  I32,
  I64,
}

#[derive(Debug, Default)]
struct Function<'p> {
  name:        String,
  args:        HashMap<String, Argument>,
  return_type: Option<Type>,
  code:        Vec<Statement>,
  functions:   HashMap<String, Function<'p>>,
  variables:   HashMap<String, Variable>,
  parent:      Option<&'p Function<'p>>,
}

#[derive(Debug)]
struct Argument {
  idx:     usize,
  name:    String,
  vartype: Type,
}

#[derive(Debug)]
struct Variable {
  idx:           usize,
  name:          String,
  vartype:       Type,
  initial_value: Expression,
}

#[derive(Debug)]
enum Statement {
  Return(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
  Constant(i32),
}

impl Program<'_> {
  pub fn from_statements(statements: Vec<tokenizer::Statement>) -> CompileResult<Self> {
    let mut errors = Vec::new();
    let program = {
      let main = Function::new("main".to_string(), vec![], None, statements, None)?;
      if main.functions.get("main").is_none() {
        Program {
          functions: HashMap::from([("main".to_string(), main)]),
        }
      } else {
        if main.variables.len() != 0 {
          errors.push(CompileError::GlobalVariableWithMain);
        }
        // TODO: code.len shoud be 0?
        Program {
          functions: main.functions,
        }
      }
    };
    if errors.len() == 0 {
      Ok(program)
    } else {
      Err(errors)
    }
  }
}

impl<'p> Function<'p> {
  pub fn new(
    name: String,
    _args: Vec<tokenizer::Argument>,
    _return_type: Option<String>,
    code: Vec<tokenizer::Statement>,
    _parent: Option<&'p Function<'p>>,
  ) -> CompileResult<Self> {
    let mut errors = Vec::new();
    let mut func = Function {
      name,
      ..Default::default()
    };
    for statement in &code {
      match statement {
        tokenizer::Statement::Return(expr) => {
          let tokenizer::Expression::Constant(retval) = expr else {
            errors.push(CompileError::NotImplemented);
            continue;
          };
          func
            .code
            .push(Statement::Return(Expression::Constant(*retval)));
        }
        _ => errors.push(CompileError::NotImplemented),
      }
    }
    Ok(func)
  }
}
