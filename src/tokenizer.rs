use crate::{error, parser::*};

#[derive(Debug, PartialEq)]
pub enum Token {
  Identifier(String),
  Expression(i32), // TODO: it should be token tree
}

/// TODO: this accepts only number
fn expect_expression(mut input: &str) -> Result<(Token, &str), error::ParseError> {
  let res = num_1(input)?;
  let mut constant = res.0;
  input = res.1;
  loop {
    let res = num_0(input);
    if res.0.is_none() {
      return Ok((Token::Expression(constant), input));
    }
    constant = constant * 10 + res.0.unwrap();
    input = res.1;
  }
}

/// consume Identifier and return (it, consumed input)
fn expect_identifier(mut input: &str) -> Result<(Token, &str), error::ParseError> {
  let mut identity = String::new();
  let result = alpha_1(input)?;
  identity.push(result.0);
  input = result.1;
  loop {
    let Ok(result) = alpha_num_1(input) else {
      break;
    };
    input = result.1;
    identity.push(result.0);
  }
  Ok((Token::Identifier(identity), input))
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn expect_expression_returns_expression_if_input_begins_with_number() {
    let input = "123 abc";
    let res = expect_expression(input);
    assert!(res.is_ok());
    let res = res.unwrap();
    let Token::Expression(constant) = res.0 else {
      panic!(
        "expect_expression({}) succeeded but does not return Token::Expression (returned: {:?})",
        input, res.0
      );
    };
    assert_eq!(constant, 123);
    assert_eq!(res.1, " abc");
  }
  #[test]
  fn expect_expression_fails_if_input_begins_with_alphabet() {
    let input = "abc 123";
    let res = expect_expression(input);
    assert!(res.is_err());
  }
  #[test]
  fn expect_identifier_returns_str_before_whitespace_and_consumed_input() {
    let input = "name01  name02";
    let res = expect_identifier(input);
    assert!(res.is_ok());
    let res = res.unwrap();
    assert_eq!(res.0, Token::Identifier("name01".to_string()));
    assert_eq!(res.1, "  name02");
  }
  #[test]
  fn expect_identifier_fails_when_input_begins_with_number() {
    let input = "0name";
    let res = expect_identifier(input);
    assert!(res.is_err());
  }
}
