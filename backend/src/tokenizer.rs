use std::vec;

use crate::{
  error::{ParseError, TokenizeError},
  parser::*,
  source_code::{self, SourceCode},
};
type TokenizeResult<'s, T> = Result<T, TokenizeError>;

#[derive(Debug, Clone)]
pub enum StatementKind {
  FnDecl(Function),
  VarDecl(Variable),
  SetterCall(SetterCall),
  ExprStatement(Expression),
  Return(Option<Expression>),
}
#[derive(Debug, Clone)]
pub struct Statement {
  pub kind:  StatementKind,
  pub begin: source_code::Position,
  pub end:   source_code::Position,
}
#[derive(Debug, Clone)]
pub struct Type {
  pub type_ident: String,
  pub pos:        source_code::Position,
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
  let pos = code.lines_and_cols();
  match expect_return(code) {
    Ok(res) => {
      return Ok(Statement {
        kind:  StatementKind::Return(res),
        begin: pos,
        end:   code.lines_and_cols(),
      })
    }
    Err(TokenizeError::NoMatch) => (),
    Err(e) => return Err(e),
  }
  match expect_fn_declaration(code) {
    Ok(res) => {
      return Ok(Statement {
        kind:  StatementKind::FnDecl(res),
        begin: pos,
        end:   code.lines_and_cols(),
      })
    }
    Err(TokenizeError::NoMatch) => (),
    Err(e) => return Err(e),
  }
  match expect_var_declaration(code) {
    Ok(res) => {
      return Ok(Statement {
        kind:  StatementKind::VarDecl(res),
        begin: pos,
        end:   code.lines_and_cols(),
      })
    }
    Err(TokenizeError::NoMatch) => (),
    Err(e) => return Err(e),
  }
  match expect_setter_call(code) {
    Ok(res) => {
      return Ok(Statement {
        kind:  StatementKind::SetterCall(res),
        begin: pos,
        end:   code.lines_and_cols(),
      })
    }
    Err(TokenizeError::NoMatch) => (),
    Err(e) => return Err(e),
  }
  match expect_expression(code) {
    Ok(res) => {
      return Ok(Statement {
        kind:  StatementKind::ExprStatement(res),
        begin: pos,
        end:   code.lines_and_cols(),
      })
    }
    Err(TokenizeError::NoMatch) => (),
    Err(e) => return Err(e),
  }

  Err(TokenizeError::NoMatch)
}

fn expect_return(code: &mut SourceCode) -> TokenizeResult<Option<Expression>> {
  str("ret")(code).or(Err(TokenizeError::NoMatch))?;
  code.skip_space();
  match expect_expression(code) {
    Ok(res) => Ok(Some(res)),
    Err(TokenizeError::NoMatch) => Ok(None),
    Err(e) => Err(e),
  }
}

#[derive(Debug, Clone)]
pub struct SetterCall {
  pub varname: String,
  pub args:    Vec<Expression>,
}
fn expect_setter_call(code: &mut SourceCode) -> TokenizeResult<SetterCall> {
  let initial_pos = code.pos();
  let varname = expect_identifier(code).or_else(|err| {
    code.unwind(initial_pos);
    Err(err)
  })?;
  println!("before: {}", code.remaining_code());
  seq(vec![
    optional(mul(space())),
    char('.'),
    optional(mul(space())),
    str("set"),
    optional(mul(space())),
    char('('),
  ])(code)
  .or_else(|_| {
    println!("remain: {}", code.remaining_code());
    code.unwind(initial_pos);
    Err(TokenizeError::NoMatch)
  })?;

  let mut setter_call = SetterCall {
    varname,
    args: Vec::new(),
  };

  loop {
    match expect_expression(code.skip_space()) {
      Ok(expr) => setter_call.args.push(expr),
      Err(TokenizeError::NoMatch) => break,
      Err(e) => return Err(e),
    }
    if char(',')(code.skip_space()).is_err() {
      break;
    }
  }
  char(')')(code.skip_space()).or(Err(TokenizeError::UnclosedDelimiter))?;

  Ok(setter_call)
}

#[derive(Debug, Clone)]
pub struct Variable {
  pub name:          String,
  pub vartype:       Type,
  pub setter:        Option<Setter>,
  pub initial_value: Expression,
}
#[derive(Debug, Clone)]
pub struct Setter {
  pub name: String,
  pub pos:  source_code::Position,
}
fn expect_var_declaration(code: &mut SourceCode) -> TokenizeResult<Variable> {
  seq(vec![str("let"), mul(space())])(code).or(Err(TokenizeError::NoMatch))?;
  let (name, vartype, setter) = expect_var_spec(code)?;
  char('=')(code.skip_space()).or(Err(TokenizeError::Expected("= <initial value>")))?;
  let initial_value = expect_expression(code.skip_space())?;
  Ok(Variable {
    name,
    vartype,
    setter,
    initial_value,
  })
}
/// (variable name, type, setter)
fn expect_var_spec(code: &mut SourceCode) -> TokenizeResult<(String, Type, Option<Setter>)> {
  let name = expect_identifier(code)?;
  char(':')(code.skip_space()).or(Err(TokenizeError::Expected(": <type>")))?;
  code.skip_space();
  let pos = code.lines_and_cols();
  let type_ident = expect_identifier(code).or(Err(TokenizeError::ExpectedType))?;
  let setter = if char('|')(code.skip_space()).is_ok() {
    let pos = code.skip_space().lines_and_cols();
    let name = expect_identifier(code).or(Err(TokenizeError::ExpectedFunction))?;
    Some(Setter { name, pos })
  } else {
    None
  };
  Ok((name, Type { type_ident, pos }, setter))
}

#[derive(Debug, Clone)]
pub struct Function {
  pub name:         String,
  pub args:         Vec<Argument>,
  pub return_type:  Option<Type>,
  pub code:         Vec<Statement>,
  pub declared_pos: source_code::Position,
}
#[derive(Debug, Clone)]
pub struct Argument {
  pub name:    String,
  pub vartype: Type,
  pub setter:  Option<Setter>,
}
fn expect_fn_declaration(code: &mut SourceCode) -> TokenizeResult<Function> {
  seq(vec![str("fn"), mul(space())])(code.skip_space()).or(Err(TokenizeError::NoMatch))?;
  let declared_pos = code.lines_and_cols();
  let name = expect_identifier(code)?;
  char('(')(code.skip_space()).or(Err(TokenizeError::Expected("(<arguments>?)")))?;
  let mut args = Vec::new();
  loop {
    match expect_var_spec(code.skip_space()) {
      Ok((name, vartype, setter)) => {
        args.push(Argument {
          name,
          vartype,
          setter,
        });
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
      code.skip_space();
      let pos = code.lines_and_cols();
      let type_ident = expect_identifier(code).or(Err(TokenizeError::ExpectedType))?;
      Some(Type { type_ident, pos })
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
    declared_pos,
  })
}

#[derive(Debug, Clone)]
pub struct Expression {
  pub element: ExprElement,
  pub begin:   source_code::Position,
  pub end:     source_code::Position,
}
impl Expression {
  /// create Expression that replaced only element and return it
  pub fn replaced(&self, element: ExprElement) -> Self {
    Expression {
      element,
      begin: self.begin,
      end: self.end,
    }
  }
}
impl PartialEq for Expression {
  fn eq(&self, other: &Self) -> bool {
    self.element == other.element
  }
  fn ne(&self, other: &Self) -> bool {
    self.element != other.element
  }
}
#[derive(Debug, Clone, PartialEq)]
pub enum ExprElement {
  Constant(i32),
  Variable(String, source_code::Position),
  FnCall(String, Vec<Expression>, source_code::Position),
  Add(Box<ExprElement>, Box<ExprElement>),
  Sub(Box<ExprElement>, Box<ExprElement>),
  Mul(Box<ExprElement>, Box<ExprElement>),
  Div(Box<ExprElement>, Box<ExprElement>),
  Mod(Box<ExprElement>, Box<ExprElement>),
}
impl ExprElement {
  // this function is used for test so basically raise warning (unused function)
  pub fn _eval(&self) -> Result<i32, ()> {
    use self::ExprElement::*;
    match self {
      Constant(a) => Ok(*a),
      Add(lhs, rhs) => Ok(lhs._eval()? + rhs._eval()?),
      Sub(lhs, rhs) => Ok(lhs._eval()? - rhs._eval()?),
      Mul(lhs, rhs) => Ok(lhs._eval()? * rhs._eval()?),
      Div(lhs, rhs) => Ok(lhs._eval()? / rhs._eval()?),
      Mod(lhs, rhs) => Ok(lhs._eval()? % rhs._eval()?),
      _ => Err(()),
    }
  }
}

// FIXME (?) : [(char('+'), ExprElement::Add), (char('-'), ExprElement::Sub)] is INVALID
fn expect_expression(code: &mut SourceCode) -> TokenizeResult<Expression> {
  let begin = code.lines_and_cols();
  let mut expr = expect_expr_mul(code)?;

  let tester = [char('+'), char('-')];
  let constructor = [ExprElement::Add, ExprElement::Sub];
  'outer: loop {
    let initial_pos = code.pos();
    for i in 0..tester.len() {
      let (test, make_element) = (&tester[i], &constructor[i]);
      if test(code.skip_space()).is_ok() {
        let second_expr = expect_expr_mul(code.skip_space())?;
        expr = make_element(Box::new(expr), Box::new(second_expr));
        continue 'outer;
      }
    }
    code.unwind(initial_pos);
    break;
  }

  return Ok(Expression {
    element: expr,
    begin,
    end: code.lines_and_cols(),
  });

  fn expect_expr_mul(code: &mut SourceCode) -> TokenizeResult<ExprElement> {
    let mut expr = expect_expr_primary(code)?;

    let tester = [char('*'), char('/'), char('%')];
    let constructor = [ExprElement::Mul, ExprElement::Div, ExprElement::Mod];
    'outer: loop {
      let initial_pos = code.pos();
      for i in 0..tester.len() {
        let (test, make_element) = (&tester[i], &constructor[i]);
        if test(code.skip_space()).is_ok() {
          let second_expr = expect_expr_primary(code.skip_space())?;
          expr = make_element(Box::new(expr), Box::new(second_expr));
          continue 'outer;
        }
      }
      code.unwind(initial_pos);
      break;
    }

    Ok(expr)
  }
  fn expect_expr_primary(code: &mut SourceCode) -> TokenizeResult<ExprElement> {
    if char('(')(code).is_ok() {
      let Ok(expr) = expect_expression(code.skip_space()) else {
        return Err(TokenizeError::ExpectedExpression);
      };
      char(')')(code.skip_space()).or(Err(TokenizeError::UnclosedDelimiter))?;
      return Ok(expr.element);
    }

    if seq(vec![str("true"), mul(space())])(code).is_ok() {
      return Ok(ExprElement::Constant(1));
    }
    if seq(vec![str("false"), mul(space())])(code).is_ok() {
      return Ok(ExprElement::Constant(0));
    }
    match expect_constant(code) {
      Ok(res) => return Ok(ExprElement::Constant(res)),
      Err(TokenizeError::NoMatch) => (),
      Err(e) => return Err(e),
    }

    let use_pos = code.lines_and_cols();
    let ident = expect_identifier(code)?;
    // check whether <ident> is function
    let Ok(()) = char('(')(code.skip_space()) else {
      return Ok(ExprElement::Variable(ident, use_pos));
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

    return Ok(ExprElement::FnCall(ident, args, use_pos));

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
    for (code, expect) in [
      ("let name: i32 = 0", Some(("name", "i32", None, 0))),
      ("let nospace:i32=10", Some(("nospace", "i32", None, 10))),
      (
        "let mutable: i64 | setter0 = 0",
        Some(("mutable", "i64", Some("setter0"), 0)),
      ),
      (
        "let mutable:i64|setter1=1",
        Some(("mutable", "i64", Some("setter1"), 1)),
      ),
      ("let no_type = 0", None),
      ("let uninitialized:i32", None),
    ] {
      let (var, (varname, vartype, setter_name, initial)) =
        match expect_var_declaration(&mut SourceCode::new(code)) {
          Ok(res) => {
            if let Some(expect) = expect {
              (res, expect)
            } else {
              panic!("code `{}` is expected to fail but it succeeded", code);
            }
          }
          Err(err) => {
            if expect.is_some() {
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
      assert_eq!(var.vartype.type_ident, vartype);
      assert_eq!(var.initial_value.element._eval().unwrap(), initial);
      match var.setter {
        Some(parsed_setter) => assert_eq!(&parsed_setter.name, setter_name.unwrap()),
        None => assert!(setter_name.is_none()),
      }
    }
  }

  #[test]
  fn test_expect_fn_declare() {
    for (code, is_expected_success, fname, retval) in [
      ("fn main()->i32{ret 0}", true, "main", 0),
      ("fn      func ( ) -> i32 { ret 128 }", true, "func", 128),
      ("fnmain() { ret 0 }", false, "", 0),
      ("fn main) { ret 0 }", false, "", 0),
      ("fn main() - > i32 { ret 0 }", false, "", 0),
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
      assert!(func.return_type.is_some());
      assert_eq!(func.return_type.unwrap().type_ident, "i32".to_string());
      let code = func.code[0].clone();
      let StatementKind::Return(expr) = code.kind else {
        panic!("Statement is wrong; expect: Return, actual: {:?}", code);
      };
      let expr = expr.expect("No return value expression");
      let ExprElement::Constant(parsed_retval) = expr.element else {
        panic!("Expression is wrong; expect: Constant, actual: {:?}", expr);
      };
      assert_eq!(parsed_retval, retval);
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
        assert_eq!(function.args[i].vartype.type_ident, "i32");
      }
    }
  }

  #[test]
  fn test_expect_return() {
    for (code, is_expected_success, retval) in [
      ("ret 0", true, 0),
      ("ret    128", true, 128),
      ("ret(1)", true, 1),
      ("ret0", false, 0),
    ] {
      let Ok(expr) = expect_return(&mut SourceCode::new(&code)) else {
        if is_expected_success {
          panic!("code `{}` is expected to succeed but it fails", code);
        } else {
          continue;
        }
      };
      let expr = expr.expect("No return value expression");
      let ExprElement::Constant(parsed_retval) = expr.element else {
        panic!("Expression is wrong; expect: Constant, actual: {:?}", expr);
      };
      assert_eq!(parsed_retval, retval);
    }

    // no return value
    for code in [
      "ret ",   // ret with space
      "ret\n}", // next line is the end of function
      "ret}",   // end of function, no space
    ] {
      let expr = expect_return(&mut SourceCode::new(code))
        .unwrap_or_else(|e| panic!("failed to parse '{:?}' as return statement : {:?}", code, e));
      assert!(expr.is_none());
    }
  }
  #[test]
  fn test_expect_setter_call() {
    for (code, expect) in [
      ("a.set(1,2,3,4,5)", Some(("a", vec![1, 2, 3, 4, 5]))),
      ("var . set()", Some(("var", vec![]))),
      (
        r"mulline
      . set
      ()",
        Some(("mulline", vec![])),
      ),
      (
        "trailing_comma.set(0,0,)",
        Some(("trailing_comma", vec![0, 0])),
      ),
      ("unclosed.set(", None),
      (".set()", None),
    ] {
      println!("=========== code: `{}`", code);
      let (parsed_setter_call, (varname, args)) =
        match expect_setter_call(&mut SourceCode::new(code)) {
          Ok(res) => {
            if let Some(expect) = expect {
              (res, expect)
            } else {
              panic!("code `{}` is expected to fail but it succeeded", code);
            }
          }
          Err(err) => {
            if expect.is_some() {
              panic!(
                "code `{}` is expected to succeed but it fails ({:?})",
                code, err
              );
            } else {
              continue;
            }
          }
        };
      assert_eq!(&parsed_setter_call.varname, varname);
      assert_eq!(parsed_setter_call.args.len(), args.len());
      for i in 0..args.len() {
        assert_eq!(parsed_setter_call.args[i].element._eval().unwrap(), args[i]);
      }
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
      ("2+-10%4", 0),    // signed mod (same behavior with Rust)
      ("true + 1", 2),   // boolean    (same behavior with C++)
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
      assert_eq!(
        expr.element._eval(),
        Ok(expect),
        "(code: {}, expr: {:?})",
        code,
        expr.element
      );
    }
  }
  #[test]
  fn expect_expression_succeeded_to_parse_expr_with_var() {
    let varname = "var".to_string();
    let code = format!("{} + 1", varname);
    let Ok(res) = expect_expression(&mut SourceCode::new(&code)) else {
      panic!("failed to parse code `{}` as an expression", code);
    };
    let ExprElement::Add(var, _) = res.element else {
      panic!(
        "code `{}` was not parsed as ExprElement::Add (result: {:?})",
        code, res
      );
    };
    let ExprElement::Variable(parsed_varname, _) = *var else {
      panic!(
        "right-hand side of `{}` was not parsed as variable (result: {:?})",
        code, var
      );
    };
    assert_eq!(parsed_varname, varname);
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
      let ExprElement::FnCall(parsed_name, parsed_args, _) = res.element else {
        panic!(
          "code `{}` is expected to be parsed as FnCall but actually {:?}",
          code, res
        );
      };
      assert_eq!(parsed_name, name);
      for i in 0..args.len() {
        assert_eq!(
          parsed_args[i].element._eval().unwrap(),
          args[i],
          "code: `{}`",
          code
        );
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
