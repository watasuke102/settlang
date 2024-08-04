use crate::{error, parser::*};

#[derive(Debug, PartialEq)]
pub enum Token {
  Identifier(String),
  Expression(i32), // TODO: it should be token tree
}

#[derive(Debug)]
pub struct Function {
  name:   Token, // Identifier
  // TODO: arguments
  retval: Token, // Expression
}

pub fn tokenize(input: &str) -> Result<(Function, &str), error::ParseError> {
  expect_fn_declare(input)
}

fn expect_fn_declare(mut input: &str) -> Result<(Function, &str), error::ParseError> {
  input = expect_str(space_0(input), "fn")?;
  let res = expect_identifier(space_1(input)?)?;
  let Token::Identifier(_) = res.0 else {
    return Err(error::ParseError::UnexpectedToken);
  };
  let ident = res.0;
  input = res.1;
  input = expect_char(space_0(input), '(')?;
  // TODO: arguments
  input = expect_char(space_0(input), ')')?;
  input = expect_char(space_0(input), '{')?;
  input = expect_str(space_0(input), "return")?;

  let res = expect_expression(space_1(input)?)?;
  let Token::Expression(_) = res.0 else {
    return Err(error::ParseError::UnexpectedToken);
  };
  let retval = res.0;
  input = res.1;

  input = expect_char(space_0(input), '}')?;
  Ok((
    Function {
      name: ident,
      retval,
    },
    input,
  ))
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
  fn test_expect_fn_declare() {
    for (input, is_expected_success, fname, retval) in [
      ("fn main(){return 0}", true, "main", 0),
      ("   fn      func ( ) { return 128 }", true, "func", 128),
      ("fnmain() { return 0 }", false, "", 0),
      ("fn main() { return0 }", false, "", 0),
      ("fn main() { return }", false, " ", 0),
      ("fn main) { return }", false, "  ", 0),
    ] {
      let Ok(res) = expect_fn_declare(input) else {
        if is_expected_success {
          panic!("input `{}` is expected to succeed but it fails", input);
        } else {
          continue;
        }
      };
      let Token::Identifier(parsed_fname) = res.0.name else {
        panic!("UnexpectedToken ({:?})", res.0.name);
      };
      let Token::Expression(parsed_retval) = res.0.retval else {
        panic!("UnexpectedToken ({:?})", res.0.retval);
      };
      assert_eq!(parsed_fname, fname.to_string());
      assert_eq!(parsed_retval, retval);
    }
  }
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
