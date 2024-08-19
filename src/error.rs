use crate::source_code;

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
  InvalidType(String, source_code::Position),
  GlobalStatementWithMain(Vec<usize>),
}
