use std::vec;

use crate::{
  error::{ParseError, TokenizeError},
  parser::*,
  source_code::SourceCode,
};
type TokenizeResult<'s, T> = Result<T, TokenizeError>;

#[derive(Debug)]
pub enum Statement {
  FnDecl(Function),
  VarDecl(Variable),
  ExprStatement(Expression),
  Return(Expression),
}

pub fn expect_code(code: &mut SourceCode) -> TokenizeResult<Vec<Statement>> {
  let mut statements = Vec::new();
  loop {
    if let Err(ParseError::EmptyInput) = mul(space())(code) {
      break;
    }
    let statement = match expect_statement(code) {
      Ok(statement) => statement,
      Err(TokenizeError::NoMatch) => break,
      Err(e) => return Err(e),
    };
    statements.push(statement);
  }
  Ok(statements)
}

fn expect_statement(code: &mut SourceCode) -> TokenizeResult<Statement> {
  match expect_return(code) {
    Ok(res) => return Ok(Statement::Return(res)),
    Err(TokenizeError::NoMatch) => (),
    Err(e) => return Err(e),
  }
  match expect_fn_declaration(code) {
    Ok(res) => return Ok(Statement::FnDecl(res)),
    Err(TokenizeError::NoMatch) => (),
    Err(e) => return Err(e),
  }
  match expect_var_declaration(code) {
    Ok(res) => return Ok(Statement::VarDecl(res)),
    Err(TokenizeError::NoMatch) => (),
    Err(e) => return Err(e),
  }
  match expect_expression(code) {
    Ok(res) => return Ok(Statement::ExprStatement(res)),
    Err(TokenizeError::NoMatch) => (),
    Err(e) => return Err(e),
  }

  Err(TokenizeError::NoMatch)
}

fn expect_return(code: &mut SourceCode) -> TokenizeResult<Expression> {
  seq(vec![str("return"), mul(space())])(code).or(Err(TokenizeError::NoMatch))?;
  expect_expression(code)
}

#[derive(Debug)]
pub struct Variable {
  pub name:          String,
  pub vartype:       String,
  pub initial_value: Expression,
}
fn expect_var_declaration(code: &mut SourceCode) -> TokenizeResult<Variable> {
  seq(vec![str("let"), mul(space())])(code).or(Err(TokenizeError::NoMatch))?;
  let (name, vartype) = expect_var_spec(code)?;
  char('=')(code.skip_space()).or(Err(TokenizeError::Expected("= <initial value>")))?;
  let initial_value = expect_expression(code.skip_space())?;
  Ok(Variable {
    name,
    vartype,
    initial_value,
  })
}
/// (variable name, type name)
fn expect_var_spec(code: &mut SourceCode) -> TokenizeResult<(String, String)> {
  let name = expect_identifier(code)?;
  char(':')(code.skip_space()).or(Err(TokenizeError::Expected(": <type>")))?;
  let type_ident = expect_identifier(code.skip_space()).or(Err(TokenizeError::ExpectedType))?;
  Ok((name, type_ident))
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
fn expect_fn_declaration(code: &mut SourceCode) -> TokenizeResult<Function> {
  seq(vec![str("fn"), mul(space())])(code.skip_space()).or(Err(TokenizeError::NoMatch))?;
  let name = expect_identifier(code)?;
  char('(')(code.skip_space()).or(Err(TokenizeError::Expected("(<arguments>?)")))?;
  let mut args = Vec::new();
  loop {
    match expect_var_spec(code.skip_space()) {
      Ok((name, vartype)) => {
        args.push(Argument { name, vartype });
      }
      Err(TokenizeError::NoMatch) => break,
      Err(e) => return Err(e),
    }
    if char(',')(code.skip_space()).is_err() {
      break;
    }
  }
  char(')')(code.skip_space()).or(Err(TokenizeError::UnclosedDelimiter))?;

  let return_type = match str("->")(code.skip_space()) {
    Ok(()) => {
      let type_ident = expect_identifier(code.skip_space()).or(Err(TokenizeError::ExpectedType))?;
      Some(type_ident)
    }
    Err(ParseError::PartialMatch(_)) => return Err(TokenizeError::Expected("-> <type>")),
    Err(_) => None,
  };

  char('{')(code.skip_space()).or(Err(TokenizeError::Expected("{ <code>? }")))?;
  let fn_code = expect_code(code.skip_space())?;
  char('}')(code.skip_space()).or(Err(TokenizeError::UnclosedDelimiter))?;

  Ok(Function {
    name,
    args,
    return_type,
    code: fn_code,
  })
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
fn expect_expression(code: &mut SourceCode) -> TokenizeResult<Expression> {
  let mut expr = expect_expr_secondary(code)?;
  loop {
    if char('+')(code.skip_space()).is_ok() {
      let second_expr = expect_expr_secondary(code.skip_space())?;
      expr = Expression::Add(Box::new(expr), Box::new(second_expr));
    } else if char('-')(code.skip_space()).is_ok() {
      let second_expr = expect_expr_secondary(code.skip_space())?;
      expr = Expression::Sub(Box::new(expr), Box::new(second_expr));
    } else {
      break;
    }
  }
  return Ok(expr);

  // <expr-secondary> = <expr-primary> ('*' <expr-primary> | '/' <expr-primary>)*
  fn expect_expr_secondary(code: &mut SourceCode) -> TokenizeResult<Expression> {
    let mut expr = expect_expr_primary(code)?;
    loop {
      if char('*')(code.skip_space()).is_ok() {
        let second_expr = expect_expr_primary(code.skip_space())?;
        expr = Expression::Mul(Box::new(expr), Box::new(second_expr));
      } else if char('/')(code.skip_space()).is_ok() {
        let second_expr = expect_expr_primary(code.skip_space())?;
        expr = Expression::Div(Box::new(expr), Box::new(second_expr));
      } else {
        break;
      }
    }
    Ok(expr)
  }
  // <expr-primary> = <constant> | '(' <expr> ')'
  fn expect_expr_primary(code: &mut SourceCode) -> TokenizeResult<Expression> {
    if char('(')(code).is_ok() {
      let Ok(expr) = expect_expression(code.skip_space()) else {
        return Err(TokenizeError::ExpectedExpression);
      };
      char(')')(code.skip_space()).or(Err(TokenizeError::UnclosedDelimiter))?;
      return Ok(expr);
    }

    match expect_constant(code) {
      Ok(res) => return Ok(Expression::Constant(res)),
      Err(TokenizeError::NoMatch) => (),
      Err(e) => return Err(e),
    }

    let ident = expect_identifier(code)?;
    // check whether <ident> is function
    let Ok(()) = char('(')(code.skip_space()) else {
      return Ok(Expression::Variable(ident));
    };

    let mut args = Vec::new();
    loop {
      match expect_expression(code.skip_space()) {
        Ok(expr) => {
          args.push(expr);
        }
        Err(TokenizeError::NoMatch) => break,
        Err(e) => return Err(e),
      }
      if char(',')(code.skip_space()).is_err() {
        break;
      }
    }
    char(')')(code.skip_space()).or(Err(TokenizeError::UnclosedDelimiter))?;

    return Ok(Expression::FnCall(ident, args));

    fn expect_constant(code: &mut SourceCode) -> TokenizeResult<i32> {
      let mut sign = 1;
      if let Ok(()) = char('+')(code) {
      } else if let Ok(()) = char('-')(code) {
        sign = -1;
      }
      let Ok(constant) = consumed(code, mul(num())) else {
        return Err(TokenizeError::NoMatch);
      };
      let constant: i32 = constant.parse().or(Err(TokenizeError::InvalidNumber))?;
      Ok(constant * sign)
    }
  }
}

/// consume Identifier and return (it, consumed code)
fn expect_identifier(code: &mut SourceCode) -> TokenizeResult<String> {
  let identity = consumed(
    code,
    seq(vec![
      or(vec![alpha(), char('_')]),
      optional(mul(or(vec![alpha(), char('_'), num()]))),
    ]),
  )
  .or(Err(TokenizeError::NoMatch))?;
  Ok(identity)
}

#[cfg(test)]
mod test {
  use super::*;
  #[test]
  fn test_expect_var_declare() {
    for (code, is_expected_success, varname, vartype) in [
      ("let name: i32 = 0", true, "name", "i32"),
      ("let nospace:i32=0", true, "nospace", "i32"),
      ("let no_type = 0", false, "", ""),
      ("let uninitialized:i32", false, "", ""),
    ] {
      let var = match expect_var_declaration(&mut SourceCode::new(code)) {
        Ok(res) => {
          if is_expected_success {
            res
          } else {
            panic!("code `{}` is expected to fail but it succeeded", code);
          }
        }
        Err(err) => {
          if is_expected_success {
            panic!(
              "code `{}` is expected to succeed but it fails ({:?})",
              code, err
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
    for (code, is_expected_success, fname, retval) in [
      ("fn main()->i32{return 0}", true, "main", 0),
      ("fn      func ( ) -> i32 { return 128 }", true, "func", 128),
      ("fnmain() { return 0 }", false, "", 0),
      ("fn main) { return 0 }", false, "", 0),
      ("fn main() - > i32 { return 0 }", false, "", 0),
    ] {
      let func = match expect_fn_declaration(&mut SourceCode::new(code)) {
        Ok(res) => {
          if is_expected_success {
            res
          } else {
            panic!("code `{}` is expected to fail but it succeeded", code);
          }
        }
        Err(err) => {
          if is_expected_success {
            panic!(
              "code `{}` is expected to succeed but it fails ({:?})",
              code, err
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
    for (code, expected_arg_names) in [
      ("fn single(a: i32) {}", vec!["a"]),
      (
        "fn triple_trailing(first:i32,second:i32,third:i32){}",
        vec!["first", "second", "third"],
      ),
    ] {
      let function = expect_fn_declaration(&mut SourceCode::new(code)).expect(code);
      for i in 0..expected_arg_names.len() {
        assert_eq!(function.args[i].name, expected_arg_names[i]);
        assert_eq!(function.args[i].vartype, "i32");
      }
    }
  }

  #[test]
  fn test_expect_return() {
    for (code, is_expected_success, retval) in [
      ("return 0", true, 0),
      ("return    128", true, 128),
      ("return0", false, 0),
    ] {
      let Ok(res) = expect_return(&mut SourceCode::new(&code)) else {
        if is_expected_success {
          panic!("code `{}` is expected to succeed but it fails", code);
        } else {
          continue;
        }
      };
      let Expression::Constant(parsed_retval) = res else {
        panic!("Expression is wrong; expect: Constant, actual: {:?}", res);
      };
      assert_eq!(parsed_retval, retval);
    }
  }
  #[test]
  fn test_expect_expression() {
    for (code, expect) in [
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
      let Ok(expr) = expect_expression(&mut SourceCode::new(code)) else {
        panic!("code `{}` is not parsed as expression", code);
      };
      assert_eq!(expr.eval(), Ok(expect), "(code: {})", code);
    }
  }
  #[test]
  fn expect_expression_succeeded_to_parse_expr_with_var() {
    let varname = "var".to_string();
    let code = format!("{} + 1", varname);
    let Ok(res) = expect_expression(&mut SourceCode::new(&code)) else {
      panic!("failed to parse code `{}` as an expression", code);
    };
    let Expression::Add(var, _) = res else {
      panic!(
        "code `{}` was not parsed as Expression::Add (result: {:?})",
        code, res
      );
    };
    assert_eq!(*var, Expression::Variable(varname));
  }
  #[test]
  fn expect_expression_succeed_to_parse_fncall() {
    for (code, name, args) in [
      ("noarg()", "noarg", vec![]),
      ("constant_args ( 1,2, 3)", "constant_args", vec![1, 2, 3]),
      ("trailing_comma(1, 1,)", "trailing_comma", vec![1, 1]),
    ] {
      let Ok(res) = expect_expression(&mut SourceCode::new(code)) else {
        panic!("failed to parse code `{}` as an expression", code);
      };
      let Expression::FnCall(parsed_name, parsed_args) = res else {
        panic!(
          "code `{}` is expected to be parsed as FnCall but actually {:?}",
          code, res
        );
      };
      assert_eq!(parsed_name, name);
      for i in 0..args.len() {
        assert_eq!(parsed_args[i].eval().unwrap(), args[i], "code: `{}`", code);
      }
    }
  }
  #[test]
  fn expect_identifier_returns_str_before_whitespace() {
    let code = "name01  name02";
    let res = expect_identifier(&mut SourceCode::new(code));
    assert_eq!(res.unwrap(), "name01".to_string());
  }
  #[test]
  fn expect_identifier_fails_when_code_begins_with_number() {
    let code = "0name";
    let res = expect_identifier(&mut SourceCode::new(code));
    assert!(res.is_err());
  }
}
