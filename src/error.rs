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
