use crate::{error, parser::*};

#[derive(Debug, PartialEq)]
pub enum Expression {
  Constant(i32),
}

#[derive(Debug)]
pub enum Statement {
  DeclStatement(Declaration),
}

#[derive(Debug)]
pub enum Declaration {
  FnDecl(Function),
}

pub fn expect_code(mut input: &str) -> ParseResult<Vec<Statement>> {
  let mut statements = Vec::new();
  while let Ok(res) = expect_statement(input) {
    statements.push(res.0);
    input = res.1;
  }
  Ok((statements, input))
}

fn expect_statement(input: &str) -> ParseResult<Statement> {
  let res = expect_declaration(input)?;
  Ok((Statement::DeclStatement(res.0), res.1))
}
fn expect_declaration(input: &str) -> ParseResult<Declaration> {
  let res = expect_fn_declaration(input)?;
  Ok((Declaration::FnDecl(res.0), res.1))
}

#[derive(Debug)]
pub struct Function {
  name:   String,
  // TODO: arguments
  retval: Expression, // Expression
}
fn expect_fn_declaration(mut input: &str) -> Result<(Function, &str), error::ParseError> {
  input = expect_str(mulspace_0(input), "fn")?.1;
  let name;
  (name, input) = expect_identifier(mulspace_1(input)?)?;
  input = expect_char(mulspace_0(input), '(')?.1;
  // TODO: arguments
  input = expect_char(mulspace_0(input), ')')?.1;
  input = expect_char(mulspace_0(input), '{')?.1;
  input = expect_str(mulspace_0(input), "return")?.1;

  let retval;
  (retval, input) = expect_expression(mulspace_1(input)?)?;

  input = expect_char(mulspace_0(input), '}')?.1;
  Ok((Function { name, retval }, input))
}

/// TODO: this accepts only number
fn expect_expression(mut input: &str) -> Result<(Expression, &str), error::ParseError> {
  let res = num_1(input)?;
  let mut constant = res.0;
  input = res.1;
  loop {
    let res = num_0(input);
    if res.0.is_none() {
      return Ok((Expression::Constant(constant), input));
    }
    constant = constant * 10 + res.0.unwrap();
    input = res.1;
  }
}

/// consume Identifier and return (it, consumed input)
fn expect_identifier(mut input: &str) -> Result<(String, &str), error::ParseError> {
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
  Ok((identity, input))
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
      let Ok(res) = expect_fn_declaration(input) else {
        if is_expected_success {
          panic!("input `{}` is expected to succeed but it fails", input);
        } else {
          continue;
        }
      };
      assert_eq!(res.0.name, fname.to_string());
      assert_eq!(res.0.retval, Expression::Constant(retval));
    }
  }
  #[test]
  fn expect_expression_returns_constant_if_input_begins_with_number() {
    let input = "123 abc";
    let res = expect_expression(input);
    assert!(res.is_ok());
    let res = res.unwrap();
    let Expression::Constant(constant) = res.0 else {
      panic!(
        "expect_expression({}) succeeded but does not return Expression::Constant (returned: {:?})",
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
    assert_eq!(res.0, "name01".to_string());
    assert_eq!(res.1, "  name02");
  }
  #[test]
  fn expect_identifier_fails_when_input_begins_with_number() {
    let input = "0name";
    let res = expect_identifier(input);
    assert!(res.is_err());
  }
}
