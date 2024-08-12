use std::vec;

use crate::{
  error::{ParseError, TokenizeError},
  parser::*,
};
type TokenizeResult<'s, T> = Result<(T, /*input:*/ &'s str), TokenizeError>;

#[derive(Debug)]
pub enum Statement {
  DeclStatement(Declaration),
  ExprStatement(Expression),
  Return(Expression),
}

#[derive(Debug)]
pub enum Declaration {
  FnDecl(Function),
  VarDecl(Variable),
}

pub fn expect_code(mut input: &str) -> TokenizeResult<Vec<Statement>> {
  let mut statements = Vec::new();
  loop {
    input = match mul(space())(input) {
      Ok(consumed) => consumed,
      Err(ParseError::NoMatch) => input,
      Err(ParseError::EmptyInput) => break,
    };
    let res = match expect_statement(input) {
      Ok(res) => res,
      Err(TokenizeError::NoMatch) => break,
      Err(e) => return Err(e),
    };
    statements.push(res.0);
    input = res.1;
  }
  Ok((statements, input))
}

fn expect_statement(input: &str) -> TokenizeResult<Statement> {
  match expect_return(input) {
    Ok(res) => return Ok((Statement::Return(res.0), res.1)),
    Err(TokenizeError::NoMatch) => (),
    Err(e) => return Err(e),
  }
  match expect_declaration(input) {
    Ok(res) => return Ok((Statement::DeclStatement(res.0), res.1)),
    Err(TokenizeError::NoMatch) => (),
    Err(e) => return Err(e),
  }
  match expect_expression(input) {
    Ok(res) => return Ok((Statement::ExprStatement(res.0), res.1)),
    Err(TokenizeError::NoMatch) => (),
    Err(e) => return Err(e),
  }

  Err(TokenizeError::NoMatch)
}

fn expect_return(input: &str) -> TokenizeResult<Expression> {
  let Ok(input) = seq(vec![str("return".to_string()), mul(space())])(input) else {
    return Err(TokenizeError::NoMatch);
  };
  expect_expression(input)
}

fn expect_declaration(input: &str) -> TokenizeResult<Declaration> {
  match expect_fn_declaration(input) {
    Ok(res) => return Ok((Declaration::FnDecl(res.0), res.1)),
    Err(TokenizeError::NoMatch) => (),
    Err(e) => return Err(e),
  }
  match expect_var_declaration(input) {
    Ok(res) => return Ok((Declaration::VarDecl(res.0), res.1)),
    Err(TokenizeError::NoMatch) => (),
    Err(e) => return Err(e),
  }

  Err(TokenizeError::NoMatch)
}

#[derive(Debug)]
pub struct Variable {
  pub name:          String,
  pub vartype:       String,
  pub initial_value: Expression,
}
fn expect_var_declaration(input: &str) -> TokenizeResult<Variable> {
  let Ok(input) = seq(vec![str("let".to_string()), mul(space())])(input) else {
    return Err(TokenizeError::NoMatch);
  };
  let ((name, vartype), input) = expect_var_spec(input)?;
  let input = char('=')(mulspace_0()(input).unwrap()).or(Err(TokenizeError::ExpectedKeyword))?;
  let (initial_value, input) = expect_expression(mulspace_0()(input).unwrap())?;
  Ok((
    Variable {
      name,
      vartype,
      initial_value,
    },
    input,
  ))
}
/// (variable name, type name)
fn expect_var_spec(input: &str) -> TokenizeResult<(String, String)> {
  let (name, input) = expect_identifier(input)?;
  let input = char(':')(mulspace_0()(input).unwrap()).or(Err(TokenizeError::ExpectedKeyword))?;
  let (type_ident, input) =
    expect_identifier(mulspace_0()(input).unwrap()).or(Err(TokenizeError::ExpectedType))?;
  Ok(((name, type_ident), input))
}

#[derive(Debug)]
pub struct Function {
  pub name:        String,
  pub args:        Vec<Argument>,
  pub return_type: Option<String>,
  pub code:        Vec<Statement>,
}
#[derive(Debug)]
pub struct Argument {
  pub name:    String,
  pub vartype: String,
}
fn expect_fn_declaration(input: &str) -> TokenizeResult<Function> {
  let Ok(input) = seq(vec![str("fn".to_string()), mul(space())])(mulspace_0()(input).unwrap())
  else {
    return Err(TokenizeError::NoMatch);
  };
  let (name, input) = expect_identifier(input)?;
  let mut input =
    char('(')(mulspace_0()(input).unwrap()).or(Err(TokenizeError::ExpectedKeyword))?;
  let mut args = Vec::new();
  loop {
    match expect_var_spec(mulspace_0()(input).unwrap()) {
      Ok(((name, vartype), consumed)) => {
        args.push(Argument { name, vartype });
        input = consumed;
      }
      Err(TokenizeError::NoMatch) => break,
      Err(e) => return Err(e),
    }
    match char(',')(mulspace_0()(input).unwrap()) {
      Ok(consumed) => {
        input = consumed;
      }
      Err(_) => break,
    }
  }
  input = char(')')(mulspace_0()(input).unwrap()).or(Err(TokenizeError::UnclosedDelimiter))?;

  let return_type = match str("->".to_string())(mulspace_0()(input).unwrap()) {
    Ok(consumed) => {
      let (type_ident, consumed) =
        expect_identifier(mulspace_0()(consumed).unwrap()).or(Err(TokenizeError::ExpectedType))?;
      input = consumed;
      Some(type_ident)
    }
    Err(_) => None,
  };

  let input = char('{')(mulspace_0()(input).unwrap()).or(Err(TokenizeError::ExpectedKeyword))?;
  let (code, input) = expect_code(mulspace_0()(input).unwrap())?;
  let input = char('}')(mulspace_0()(input).unwrap()).or(Err(TokenizeError::UnclosedDelimiter))?;

  Ok((
    Function {
      name,
      args,
      return_type,
      code,
    },
    input,
  ))
}

#[derive(Debug, PartialEq)]
pub enum Expression {
  Constant(i32),
  Variable(String),
  FnCall(String, Vec<Expression>),
  Add(Box<Expression>, Box<Expression>),
  Sub(Box<Expression>, Box<Expression>),
  Mul(Box<Expression>, Box<Expression>),
  Div(Box<Expression>, Box<Expression>),
}
impl Expression {
  pub fn eval(&self) -> Result<i32, ()> {
    use Expression::*;
    match self {
      Constant(a) => Ok(*a),
      Add(lhs, rhs) => Ok(lhs.eval()? + rhs.eval()?),
      Sub(lhs, rhs) => Ok(lhs.eval()? - rhs.eval()?),
      Mul(lhs, rhs) => Ok(lhs.eval()? * rhs.eval()?),
      Div(lhs, rhs) => Ok(lhs.eval()? / rhs.eval()?),
      _ => Err(()),
    }
  }
}
// <expr> = <expr-secondary> ('+' <expr-secondary> | '-' <expr-secondary>)*
fn expect_expression(mut input: &str) -> TokenizeResult<Expression> {
  let mut expr;
  (expr, input) = expect_expr_secondary(input)?;
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
  return Ok((expr, input));

  // <expr-secondary> = <expr-primary> ('*' <expr-primary> | '/' <expr-primary>)*
  fn expect_expr_secondary(mut input: &str) -> TokenizeResult<Expression> {
    let mut expr;
    (expr, input) = expect_expr_primary(input)?;
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
  // <expr-primary> = <constant> | '(' <expr> ')'
  fn expect_expr_primary(input: &str) -> TokenizeResult<Expression> {
    if let Ok(input) = char('(')(input) {
      let Ok((expr, input)) = expect_expression(mulspace_0()(input).unwrap()) else {
        return Err(TokenizeError::ExpectedExpression);
      };
      let Ok(input) = char(')')(mulspace_0()(input).unwrap()) else {
        return Err(TokenizeError::UnclosedDelimiter);
      };
      return Ok((expr, input));
    }

    match expect_constant(input) {
      Ok(res) => return Ok((Expression::Constant(res.0), res.1)),
      Err(TokenizeError::NoMatch) => (),
      Err(e) => return Err(e),
    }

    let (ident, input) = expect_identifier(input)?;
    // check whether <ident> is function
    let Ok(mut input) = char('(')(mulspace_0()(input).unwrap()) else {
      return Ok((Expression::Variable(ident), input));
    };

    let mut args = Vec::new();
    loop {
      match expect_expression(mulspace_0()(input).unwrap()) {
        Ok((expr, consumed)) => {
          args.push(expr);
          input = consumed;
        }
        Err(TokenizeError::NoMatch) => break,
        Err(e) => return Err(e),
      }
      match char(',')(mulspace_0()(input).unwrap()) {
        Ok(consumed) => {
          input = consumed;
        }
        Err(_) => break,
      }
    }

    let Ok(input) = char(')')(mulspace_0()(input).unwrap()) else {
      return Err(TokenizeError::UnclosedDelimiter);
    };

    return Ok((Expression::FnCall(ident, args), input));

    fn expect_constant(mut input: &str) -> TokenizeResult<i32> {
      let mut sign = 1;
      if let Ok(consumed) = char('+')(input) {
        input = consumed;
      } else if let Ok(consumed) = char('-')(input) {
        input = consumed;
        sign = -1;
      }
      let Ok((constant, input)) = consumed(input, mul(num())) else {
        return Err(TokenizeError::NoMatch);
      };
      let constant: i32 = constant.parse().or(Err(TokenizeError::InvalidNumber))?;
      Ok((constant * sign, input))
    }
  }
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
  .or(Err(TokenizeError::NoMatch))?;
  Ok((identity.to_string(), input))
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_expect_var_declare() {
    for (input, is_expected_success, varname, vartype) in [
      ("let name: i32 = 0", true, "name", "i32"),
      ("let nospace:i32=0", true, "nospace", "i32"),
      ("let no_type = 0", false, "", ""),
      ("let uninitialized:i32", false, "", ""),
    ] {
      let var = match expect_var_declaration(input) {
        Ok(res) => {
          if is_expected_success {
            res.0
          } else {
            panic!("input `{}` is expected to fail but it succeeded", input);
          }
        }
        Err(err) => {
          if is_expected_success {
            panic!(
              "input `{}` is expected to succeed but it fails ({:?})",
              input, err
            );
          } else {
            continue;
          }
        }
      };
      assert_eq!(var.name, varname);
      assert_eq!(var.vartype, vartype);
    }
  }

  #[test]
  fn test_expect_fn_declare() {
    for (input, is_expected_success, fname, retval) in [
      ("fn main()->i32{return 0}", true, "main", 0),
      ("fn      func ( ) -> i32 { return 128 }", true, "func", 128),
      ("fnmain() { return 0 }", false, "", 0),
      ("fn main) { return 0 }", false, "", 0),
      ("fn main() - > i32 { return 0 }", false, "", 0),
    ] {
      let func = match expect_fn_declaration(input) {
        Ok(res) => {
          if is_expected_success {
            res.0
          } else {
            panic!("input `{}` is expected to fail but it succeeded", input);
          }
        }
        Err(err) => {
          if is_expected_success {
            panic!(
              "input `{}` is expected to succeed but it fails ({:?})",
              input, err
            );
          } else {
            continue;
          }
        }
      };
      assert_eq!(func.name, fname.to_string());
      assert_eq!(func.args.len(), 0);
      assert_eq!(func.code.len(), 1);
      assert_eq!(func.code.len(), 1);
      assert_eq!(func.return_type, Some("i32".to_string()));
      let Statement::Return(ref expr) = func.code[0] else {
        panic!(
          "Statement is wrong; expect: Return, actual: {:?}",
          func.code[0]
        );
      };
      let Expression::Constant(parsed_retval) = expr else {
        panic!("Expression is wrong; expect: Constant, actual: {:?}", expr);
      };
      assert_eq!(*parsed_retval, retval);
    }
  }
  #[test]
  fn expect_fn_for_arguments() {
    for (input, expected_arg_names) in [
      ("fn single(a: i32) {}", vec!["a"]),
      (
        "fn triple_trailing(first:i32,second:i32,third:i32){}",
        vec!["first", "second", "third"],
      ),
    ] {
      let (function, _) = expect_fn_declaration(input).expect(input);
      for i in 0..expected_arg_names.len() {
        assert_eq!(function.args[i].name, expected_arg_names[i]);
        assert_eq!(function.args[i].vartype, "i32");
      }
    }
  }

  #[test]
  fn test_expect_return() {
    for (input, is_expected_success, retval) in [
      ("return 0", true, 0),
      ("return    128", true, 128),
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
    for (input, expect) in [
      ("111", 111),      // constant
      ("+11", 11),       // positive constant
      ("-1", -1),        // negative constant
      ("10 + 20", 30),   // basic binary
      ("1+2 * 3", 7),    // mul should be prioritized
      ("2 * (4+6)", 20), // bracket should be prioritized
      (
        "6
        -2",
        4,
      ), // multiline expression (maybe same behavior with JS?)
      (
        "3 #* comment *# +7
        # comment
        -5",
        5,
      ), // comment is equivalent to space
    ] {
      let Ok((expr, _)) = expect_expression(input) else {
        panic!("input `{}` is not parsed as expression", input);
      };
      assert_eq!(expr.eval(), Ok(expect), "(input: {})", input);
    }
  }
  #[test]
  fn expect_expression_succeeded_to_parse_expr_with_var() {
    let varname = "var".to_string();
    let input = format!("{} + 1", varname);
    let Ok((res, _)) = expect_expression(&input) else {
      panic!("failed to parse input `{}` as an expression", input);
    };
    let Expression::Add(var, _) = res else {
      panic!(
        "input `{}` was not parsed as Expression::Add (result: {:?})",
        input, res
      );
    };
    assert_eq!(*var, Expression::Variable(varname));
  }
  #[test]
  fn expect_expression_succeed_to_parse_fncall() {
    for (input, name, args) in [
      ("noarg()", "noarg", vec![]),
      ("constant_args ( 1,2, 3)", "constant_args", vec![1, 2, 3]),
      ("trailing_comma(1, 1,)", "trailing_comma", vec![1, 1]),
    ] {
      let Ok((res, _)) = expect_expression(input) else {
        panic!("failed to parse input `{}` as an expression", input);
      };
      let Expression::FnCall(parsed_name, parsed_args) = res else {
        panic!(
          "input `{}` is expected to be parsed as FnCall but actually {:?}",
          input, res
        );
      };
      assert_eq!(parsed_name, name);
      for i in 0..args.len() {
        assert_eq!(
          parsed_args[i].eval().unwrap(),
          args[i],
          "input: `{}`",
          input
        );
      }
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
