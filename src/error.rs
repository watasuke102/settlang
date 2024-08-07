#[derive(Debug, PartialEq)]
pub enum ParseError {
  NoMatch,
  EmptyInput,
}

#[derive(Debug, PartialEq)]
pub enum TokenizeError {
  ExpectedStatement,
  ExpectedExpression,
  ExpectedKeyword,
  InvalidNumber,
  InvalidIdentifier,
  UnclosedDelimiter,
}
