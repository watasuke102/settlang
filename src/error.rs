use crate::{compile, source_code};

#[derive(Debug, PartialEq)]
pub enum ParseError {
  NoMatch,
  PartialMatch(String),
  EmptyInput,
}

#[derive(Debug, PartialEq)]
pub enum TokenizeError {
  NoMatch,
  ExpectedExpression,
  Expected(&'static str),
  ExpectedType,
  InvalidNumber,
  UnclosedDelimiter,
}

#[derive(Debug, PartialEq)]
pub enum CompileError {
  UndefinedVariable(String, source_code::Position),
  UndefinedFunction(String, source_code::Position),
  DuplicatedDecl(String, source_code::Position),
  // function name, expect, actual
  WrongArgumentLen(String, usize, usize, source_code::Position),
  InvalidType(String, source_code::Position),
  // from, to, begin, end
  InvalidCast(
    compile::Type,
    compile::Type,
    source_code::Position,
    source_code::Position,
  ),
  // expect, actual, begin, end
  MismatchReturnExprType(
    compile::Type,
    compile::Type,
    source_code::Position,
    source_code::Position,
  ),
  GlobalStatementWithMain(Vec<usize>),
}
