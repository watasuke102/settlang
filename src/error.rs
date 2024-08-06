#[derive(Debug)]
pub enum ParseError {
  UnexpectedStr,
  UnexpectedChar,
  ExpectedStatement,
  ExpectedExpression,
}
