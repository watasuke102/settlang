use std::vec;

use crate::{error::TokenizeError, parser::*};
type TokenizeResult<'s, T> = Result<(T, /*input:*/ &'s str), TokenizeError>;

#[derive(Debug)]
pub enum Statement {
  DeclStatement(Declaration),
  Return(Expression),
}

#[derive(Debug)]
pub enum Declaration {
  FnDecl(Function),
}

pub fn expect_code(mut input: &str) -> TokenizeResult<Vec<Statement>> {
  let mut statements = Vec::new();
  while let Ok(res) = expect_statement(input) {
    statements.push(res.0);
    input = res.1;
  }
  Ok((statements, input))
}

fn expect_statement(input: &str) -> TokenizeResult<Statement> {
  let res = expect_return(input);
  if res.is_ok() {
    let res = res.unwrap();
    return Ok((Statement::Return(res.0), res.1));
  }

  let res = expect_declaration(input);
  if res.is_ok() {
    let res = res.unwrap();
    return Ok((Statement::DeclStatement(res.0), res.1));
  }
  Err(TokenizeError::ExpectedStatement)
}
fn expect_declaration(input: &str) -> TokenizeResult<Declaration> {
  let res = expect_fn_declaration(input)?;
  Ok((Declaration::FnDecl(res.0), res.1))
}

fn expect_return(input: &str) -> TokenizeResult<Expression> {
  let Ok(input) = seq(vec![str("return".to_string()), mul(space())])(mulspace_0()(input).unwrap())
  else {
    return Err(TokenizeError::ExpectedKeyword);
  };
  expect_expression(input)
}

#[derive(Debug)]
pub struct Function {
  pub name: String,
  // TODO: arguments
  pub code: Vec<Statement>,
}
fn expect_fn_declaration(mut input: &str) -> Result<(Function, &str), TokenizeError> {
  input = seq(vec![str("fn".to_string()), mul(space())])(mulspace_0()(input).unwrap())
    .or(Err(TokenizeError::ExpectedKeyword))?;
  let name;
  (name, input) = expect_identifier(input)?;
  // TODO: arguments
  input = seq(vec![
    mulspace_0(),
    char('('),
    mulspace_0(),
    char(')'),
    mulspace_0(),
    char('{'),
  ])(mulspace_0()(input).unwrap())
  .or(Err(TokenizeError::ExpectedKeyword))?;

  let code;
  (code, input) = expect_code(input)?;

  input = char('}')(mulspace_0()(input).unwrap()).or(Err(TokenizeError::UnclosedDelimiter))?;
  Ok((Function { name, code }, input))
}

#[derive(Debug, PartialEq)]
pub enum Expression {
  Constant(i32),
  Add(Box<Expression>, Box<Expression>),
  Sub(Box<Expression>, Box<Expression>),
  Mul(Box<Expression>, Box<Expression>),
  Div(Box<Expression>, Box<Expression>),
}
impl Expression {
  pub fn eval(&self) -> i32 {
    use Expression::*;
    match self {
      Constant(a) => *a,
      Add(lhs, rhs) => lhs.eval() + rhs.eval(),
      Sub(lhs, rhs) => lhs.eval() - rhs.eval(),
      Mul(lhs, rhs) => lhs.eval() * rhs.eval(),
      Div(lhs, rhs) => lhs.eval() / rhs.eval(),
    }
  }
}
// <expr> = <expr-secondary> ('+' <expr-secondary> | '-' <expr-secondary>)*
fn expect_expression(mut input: &str) -> TokenizeResult<Expression> {
  let mut expr;
  (expr, input) = expect_expr_secondary(mulspace_0()(input).unwrap())?;
  loop {
    if let Ok(res) = char('+')(mulspace_0()(input).unwrap()) {
      input = res;
      let second_expr;
      (second_expr, input) = expect_expr_secondary(mulspace_0()(input).unwrap())?;
      expr = Expression::Add(Box::new(expr), Box::new(second_expr));
    } else if let Ok(res) = char('-')(mulspace_0()(input).unwrap()) {
      input = res;
      let second_expr;
      (second_expr, input) = expect_expr_secondary(mulspace_0()(input).unwrap())?;
      expr = Expression::Sub(Box::new(expr), Box::new(second_expr));
    } else {
      break;
    }
  }
  Ok((expr, input))
}
// <expr-secondary> = <expr-primary> ('*' <expr-primary> | '/' <expr-primary>)*
fn expect_expr_secondary(mut input: &str) -> TokenizeResult<Expression> {
  let mut expr;
  (expr, input) = expect_expr_primary(mulspace_0()(input).unwrap())?;
  loop {
    if let Ok(res) = char('*')(mulspace_0()(input).unwrap()) {
      input = res;
      let second_expr;
      (second_expr, input) = expect_expr_primary(mulspace_0()(input).unwrap())?;
      expr = Expression::Mul(Box::new(expr), Box::new(second_expr));
    } else if let Ok(res) = char('/')(mulspace_0()(input).unwrap()) {
      input = res;
      let second_expr;
      (second_expr, input) = expect_expr_primary(mulspace_0()(input).unwrap())?;
      expr = Expression::Div(Box::new(expr), Box::new(second_expr));
    } else {
      break;
    }
  }
  Ok((expr, input))
}
// TODO: use function call and variable instead of <constant>
// <expr-primary> = <constant> | '(' <expr> ')'
fn expect_expr_primary(input: &str) -> TokenizeResult<Expression> {
  if let Ok(input) = char('(')(mulspace_0()(input).unwrap()) {
    let Ok((expr, input)) = expect_expression(mulspace_0()(input).unwrap()) else {
      return Err(TokenizeError::ExpectedExpression);
    };
    let Ok(input) = char(')')(mulspace_0()(input).unwrap()) else {
      return Err(TokenizeError::UnclosedDelimiter);
    };
    return Ok((expr, input));
  }

  let Ok((constant, input)) = consumed(input, mul(num())) else {
    return Err(TokenizeError::InvalidNumber);
  };
  let constant: i32 = constant.parse().or(Err(TokenizeError::InvalidNumber))?;
  return Ok((Expression::Constant(constant), input));
}

/// consume Identifier and return (it, consumed input)
fn expect_identifier(input: &str) -> TokenizeResult<String> {
  let (identity, input) = consumed(
    input,
    seq(vec![
      or(vec![alpha(), char('_')]),
      optional(mul(or(vec![alpha(), char('_'), num()]))),
    ]),
  )
  .or(Err(TokenizeError::InvalidIdentifier))?;
  Ok((identity.to_string(), input))
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
      assert_eq!(res.0.code.len(), 1);
      let Statement::Return(ref expr) = res.0.code[0] else {
        panic!(
          "Statement is wrong; expect: Return, actual: {:?}",
          res.0.code[0]
        );
      };
      let Expression::Constant(parsed_retval) = expr else {
        panic!("Expression is wrong; expect: Constant, actual: {:?}", expr);
      };
      assert_eq!(*parsed_retval, retval);
    }
  }

  #[test]
  fn test_expect_return() {
    for (input, is_expected_success, retval) in [
      ("return 0", true, 0),
      ("   return 128", true, 128),
      ("return0", false, 0),
    ] {
      let Ok(res) = expect_return(input) else {
        if is_expected_success {
          panic!("input `{}` is expected to succeed but it fails", input);
        } else {
          continue;
        }
      };
      let Expression::Constant(parsed_retval) = res.0 else {
        panic!("Expression is wrong; expect: Constant, actual: {:?}", res.0);
      };
      assert_eq!(parsed_retval, retval);
    }
  }
  #[test]
  fn test_expect_expression() {
    for input in [
      ("10 + 20", 30),   // basic binary
      ("1+2 * 3", 7),    // mul should be prioritized
      ("2 * (4+6)", 20), // bracket should be prioritized
    ] {
      let Ok((expr, _)) = expect_expression(input) else {
        panic!("input `{}` is not parsed as expression", input);
      };
      assert_eq!(expr.eval(), expect, "(input: {})", input);
    }
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
