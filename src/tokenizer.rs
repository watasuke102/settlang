use crate::{error, parser::*};

#[derive(Debug)]
pub enum Statement {
  DeclStatement(Declaration),
  Return(Expression),
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
  Err(error::ParseError::ExpectedStatement)
}
fn expect_declaration(input: &str) -> ParseResult<Declaration> {
  let res = expect_fn_declaration(input)?;
  Ok((Declaration::FnDecl(res.0), res.1))
}

fn expect_return(input: &str) -> ParseResult<Expression> {
  let Ok((_, input)) = expect_str(mulspace_0(input), "return") else {
    return Err(error::ParseError::UnexpectedStr);
  };
  expect_expression(mulspace_1(input)?)
}

#[derive(Debug)]
pub struct Function {
  pub name: String,
  // TODO: arguments
  pub code: Vec<Statement>,
}
fn expect_fn_declaration(mut input: &str) -> Result<(Function, &str), error::ParseError> {
  input = expect_str(mulspace_0(input), "fn")?.1;
  let name;
  (name, input) = expect_identifier(mulspace_1(input)?)?;
  input = expect_char(mulspace_0(input), '(')?.1;
  // TODO: arguments
  input = expect_char(mulspace_0(input), ')')?.1;
  input = expect_char(mulspace_0(input), '{')?.1;

  let code;
  (code, input) = expect_code(input)?;

  input = expect_char(mulspace_0(input), '}')?.1;
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
// <expr> = <expr-secondary> ('+' <expr-secondary> | '-' <expr-secondary>)*
fn expect_expression(mut input: &str) -> ParseResult<Expression> {
  let mut expr;
  (expr, input) = expect_expr_secondary(mulspace_0(input))?;
  loop {
    if let Ok(res) = expect_char(mulspace_0(input), '+') {
      input = res.1;
      let second_expr;
      (second_expr, input) = expect_expr_secondary(mulspace_0(input))?;
      expr = Expression::Add(Box::new(expr), Box::new(second_expr));
    } else if let Ok(res) = expect_char(mulspace_0(input), '-') {
      input = res.1;
      let second_expr;
      (second_expr, input) = expect_expr_secondary(mulspace_0(input))?;
      expr = Expression::Sub(Box::new(expr), Box::new(second_expr));
    } else {
      break;
    }
  }
  Ok((expr, input))
}
// <expr-secondary> = <expr-primary> ('*' <expr-primary> | '/' <expr-primary>)*
fn expect_expr_secondary(mut input: &str) -> ParseResult<Expression> {
  let mut expr;
  (expr, input) = expect_expr_primary(mulspace_0(input))?;
  loop {
    if let Ok(res) = expect_char(mulspace_0(input), '*') {
      input = res.1;
      let second_expr;
      (second_expr, input) = expect_expr_primary(mulspace_0(input))?;
      expr = Expression::Mul(Box::new(expr), Box::new(second_expr));
    } else if let Ok(res) = expect_char(mulspace_0(input), '/') {
      input = res.1;
      let second_expr;
      (second_expr, input) = expect_expr_primary(mulspace_0(input))?;
      expr = Expression::Div(Box::new(expr), Box::new(second_expr));
    } else {
      break;
    }
  }
  Ok((expr, input))
}
// TODO: use function call and variable instead of <constant>
// <expr-primary> = <constant> | '(' <expr> ')'
fn expect_expr_primary(mut input: &str) -> ParseResult<Expression> {
  if let Ok((_, input)) = expect_char(mulspace_0(input), '(') {
    let Ok((expr, input)) = expect_expression(mulspace_0(input)) else {
      return Err(error::ParseError::ExpectedExpression);
    };
    let (_, input) = expect_char(mulspace_0(input), ')')?;
    return Ok((expr, input));
  }

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
  fn expect_expression_returns_constant_if_input_is_number() {
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
  fn expect_expression_returns_add() {
    let input = "1 + 2";
    let res = expect_expression(input);
    assert!(res.is_ok());
    let res = res.unwrap();
    let Expression::Add(lhs, rhs) = res.0 else {
      panic!(
        "expect_expression({}) succeeded but does not return Expression::Add (returned: {:?})",
        input, res.0
      );
    };
    assert_eq!(res.1, "");
    assert_eq!(*lhs, Expression::Constant(1));
    assert_eq!(*rhs, Expression::Constant(2));
  }
  #[test]
  fn expect_expression_returns_sub_contains_add() {
    // It means (1+2) - 3, so expect Sub(Add(1,2), 3)
    let input = "1+2 - 3";
    let res = expect_expression(input);
    assert!(res.is_ok());
    let res = res.unwrap();
    let Expression::Add(lhs, rhs) = res.0 else {
      panic!(
        "expect_expression({}) succeeded but does not return Expression::Add (returned: {:?})",
        input, res.0
      );
    };
    let Expression::Sub(rhs_first, rhs_second) = *rhs else {
      panic!(
        "Right-hand side of Expression::Add is not Expression::Sub(returned: {:?})",
        rhs
      );
    };
    assert_eq!(res.1, "");
    assert_eq!(*lhs, Expression::Constant(1));
    assert_eq!(*rhs_first, Expression::Constant(2));
    assert_eq!(*rhs_second, Expression::Constant(3));
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
