#[derive(Debug, PartialEq)]
pub enum ParseError {
  NoMatch,
  EmptyInput,
}

#[derive(Debug, PartialEq)]
pub enum TokenizeError {
  NoMatch,
  ExpectedExpression,
  ExpectedKeyword,
  ExpectedType,
  InvalidNumber,
  UnclosedDelimiter,
}
