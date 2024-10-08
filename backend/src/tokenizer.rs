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
  ForLoop(For),
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
impl PartialEq for Type {
  fn eq(&self, other: &Self) -> bool {
    self.type_ident == other.type_ident
  }
  fn ne(&self, other: &Self) -> bool {
    self.type_ident != other.type_ident
  }
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
  for test in [
    expect_fn_declaration,
    expect_var_declaration,
    expect_return,
    expect_for_loop,
    expect_setter_call,
    expect_expr_statement,
  ] {
    match test(code) {
      Ok(res) => {
        return Ok(Statement {
          kind:  res,
          begin: pos,
          end:   code.lines_and_cols(),
        })
      }
      Err(TokenizeError::NoMatch) => (),
      Err(e) => return Err(e),
    }
  }
  Err(TokenizeError::NoMatch)
}

/// (variable name, type, setter)
fn expect_var_spec(code: &mut SourceCode) -> TokenizeResult<(String, Type, Option<Setter>)> {
  let name = expect_ident(code)?;
  char(':')(code.skip_space()).or(Err(TokenizeError::Expected(": <type>")))?;
  code.skip_space();
  let vartype = expect_type(code)?;
  let setter = if char('|')(code.skip_space()).is_ok() {
    let pos = code.skip_space().lines_and_cols();
    let name = expect_ident(code).or(Err(TokenizeError::ExpectedFunction))?;
    Some(Setter { name, pos })
  } else {
    None
  };
  Ok((name, vartype, setter))
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
fn expect_fn_declaration(code: &mut SourceCode) -> TokenizeResult<StatementKind> {
  expect_keyword(code, "fn")?;
  code.skip_space();
  let declared_pos = code.lines_and_cols();
  let name = expect_ident(code)?;
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
    Ok(()) => Some(expect_type(code.skip_space())?),
    Err(ParseError::PartialMatch(_)) => return Err(TokenizeError::Expected("-> <type>")),
    Err(_) => None,
  };

  char('{')(code.skip_space()).or(Err(TokenizeError::Expected("{ <code>? }")))?;
  let fn_code = expect_code(code.skip_space())?;
  char('}')(code.skip_space()).or(Err(TokenizeError::UnclosedDelimiter))?;

  Ok(StatementKind::FnDecl(Function {
    name,
    args,
    return_type,
    code: fn_code,
    declared_pos,
  }))
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
fn expect_var_declaration(code: &mut SourceCode) -> TokenizeResult<StatementKind> {
  expect_keyword(code, "let")?;
  let (name, vartype, setter) = expect_var_spec(code.skip_space())?;
  char('=')(code.skip_space()).or(Err(TokenizeError::Expected("= <initial value>")))?;
  let initial_value = expect_expression(code.skip_space())?;
  Ok(StatementKind::VarDecl(Variable {
    name,
    vartype,
    setter,
    initial_value,
  }))
}

fn expect_return(code: &mut SourceCode) -> TokenizeResult<StatementKind> {
  expect_keyword(code, "ret")?;
  match expect_expression(code.skip_space()) {
    Ok(res) => Ok(StatementKind::Return(Some(res))),
    Err(TokenizeError::NoMatch) => Ok(StatementKind::Return(None)),
    Err(e) => Err(e),
  }
}

#[derive(Debug, Clone)]
pub struct For {
  pub varname:   String,
  pub inclusive: bool,
  pub begin:     Expression,
  pub end:       Expression,
  pub code:      Vec<Statement>,
}
fn expect_for_loop(code: &mut SourceCode) -> TokenizeResult<StatementKind> {
  expect_keyword(code, "for")?;
  // use closure to prevent moving value
  let err = || TokenizeError::Expected("for <varname> in <begin>..<end>");
  let varname = expect_ident(code.skip_space())?;
  seq(vec![str("in"), mul(space())])(code.skip_space()).or(Err(err()))?;
  let begin = expect_expression(code.skip_space()).or(Err(err()))?;
  str("..")(code.skip_space()).or(Err(err()))?;
  let inclusive = char('=')(code.skip_space()).is_ok();
  let end = expect_expression(code.skip_space()).or(Err(err()))?;

  char('{')(code.skip_space()).or(Err(TokenizeError::Expected("{ <code>? }")))?;
  let for_code = expect_code(code.skip_space())?;
  char('}')(code.skip_space()).or(Err(TokenizeError::UnclosedDelimiter))?;

  Ok(StatementKind::ForLoop(For {
    varname,
    inclusive,
    begin,
    end,
    code: for_code,
  }))
}

#[derive(Debug, Clone)]
pub struct SetterCall {
  pub varname: String,
  pub args:    Vec<Expression>,
}
fn expect_setter_call(code: &mut SourceCode) -> TokenizeResult<StatementKind> {
  let initial_pos = code.pos();
  let varname = expect_ident(code).or_else(|err| {
    code.unwind(initial_pos);
    Err(err)
  })?;
  seq(vec![
    optional(mul(space())),
    char('.'),
    optional(mul(space())),
    str("set"),
    optional(mul(space())),
    char('('),
  ])(code)
  .or_else(|_| {
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

  Ok(StatementKind::SetterCall(setter_call))
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
  Int(i64),
  StrLiteral(String),
  Variable(String, source_code::Position),
  Cast(Type, Box<ExprElement>),
  FnCall(String, Vec<Expression>, source_code::Position),
  IfExpr(Box<If>),

  Mul(Box<ExprElement>, Box<ExprElement>),
  Div(Box<ExprElement>, Box<ExprElement>),
  Mod(Box<ExprElement>, Box<ExprElement>),
  Add(Box<ExprElement>, Box<ExprElement>),
  Sub(Box<ExprElement>, Box<ExprElement>),
  Less(Box<ExprElement>, Box<ExprElement>),
  LessEq(Box<ExprElement>, Box<ExprElement>),
  Greater(Box<ExprElement>, Box<ExprElement>),
  GreaterEq(Box<ExprElement>, Box<ExprElement>),
  Eq(Box<ExprElement>, Box<ExprElement>),
  NonEq(Box<ExprElement>, Box<ExprElement>),
  LogicAnd(Box<ExprElement>, Box<ExprElement>),
  LogicOr(Box<ExprElement>, Box<ExprElement>),
}
impl ExprElement {
  // this function is used for test so basically raise warning (unused function)
  pub fn _eval(&self) -> Result<i64, ()> {
    use self::ExprElement::*;
    match self {
      Int(a) => Ok(*a),
      Add(lhs, rhs) => Ok(lhs._eval()? + rhs._eval()?),
      Sub(lhs, rhs) => Ok(lhs._eval()? - rhs._eval()?),
      Mul(lhs, rhs) => Ok(lhs._eval()? * rhs._eval()?),
      Div(lhs, rhs) => Ok(lhs._eval()? / rhs._eval()?),
      Mod(lhs, rhs) => Ok(lhs._eval()? % rhs._eval()?),
      Eq(lhs, rhs) => Ok((lhs._eval()? == rhs._eval()?) as i64),
      NonEq(lhs, rhs) => Ok((lhs._eval()? != rhs._eval()?) as i64),
      Less(lhs, rhs) => Ok((lhs._eval()? < rhs._eval()?) as i64),
      LessEq(lhs, rhs) => Ok((lhs._eval()? <= rhs._eval()?) as i64),
      Greater(lhs, rhs) => Ok((lhs._eval()? > rhs._eval()?) as i64),
      GreaterEq(lhs, rhs) => Ok((lhs._eval()? >= rhs._eval()?) as i64),
      LogicAnd(lhs, rhs) => Ok(((lhs._eval()? != 0) && (rhs._eval()? != 0)) as i64),
      LogicOr(lhs, rhs) => Ok(((lhs._eval()? != 0) || (rhs._eval()? != 0)) as i64),
      _ => Err(()),
    }
  }
}
fn expect_expr_statement(code: &mut SourceCode) -> TokenizeResult<StatementKind> {
  Ok(StatementKind::ExprStatement(expect_expression(code)?))
}
fn expect_expression(code: &mut SourceCode) -> TokenizeResult<Expression> {
  let begin = code.lines_and_cols();
  return Ok(Expression {
    element: expect_expr_logical_or(code)?,
    begin,
    end: code.lines_and_cols(),
  });

  fn expect_expr_logical_or(code: &mut SourceCode) -> TokenizeResult<ExprElement> {
    let mut expr = expect_expr_logical_and(code)?;
    loop {
      let initial_pos = code.pos();
      if str("||")(code.skip_space()).is_ok() {
        let second_expr = expect_expr_logical_and(code.skip_space())?;
        expr = ExprElement::LogicOr(Box::new(expr), Box::new(second_expr));
      } else {
        code.unwind(initial_pos);
        break;
      }
    }
    Ok(expr)
  }
  fn expect_expr_logical_and(code: &mut SourceCode) -> TokenizeResult<ExprElement> {
    let mut expr = expect_expr_eq(code)?;
    loop {
      let initial_pos = code.pos();
      if str("&&")(code.skip_space()).is_ok() {
        let second_expr = expect_expr_eq(code.skip_space())?;
        expr = ExprElement::LogicAnd(Box::new(expr), Box::new(second_expr));
      } else {
        code.unwind(initial_pos);
        break;
      }
    }
    Ok(expr)
  }
  fn expect_expr_eq(code: &mut SourceCode) -> TokenizeResult<ExprElement> {
    let mut expr = expect_expr_cmp(code)?;

    // FIXME (?) : [(str("=="), ExprElement::Eq),(str("!="), ExprElement::NonEq)] is INVALID
    let tester = [str("=="), str("!=")];
    let constructor = [ExprElement::Eq, ExprElement::NonEq];
    'outer: loop {
      let initial_pos = code.pos();
      for i in 0..tester.len() {
        let (test, make_element) = (&tester[i], &constructor[i]);
        if test(code.skip_space()).is_ok() {
          let second_expr = expect_expr_cmp(code.skip_space())?;
          expr = make_element(Box::new(expr), Box::new(second_expr));
          continue 'outer;
        }
      }
      code.unwind(initial_pos);
      break;
    }

    Ok(expr)
  }
  fn expect_expr_cmp(code: &mut SourceCode) -> TokenizeResult<ExprElement> {
    let mut expr = expect_expr_add(code)?;

    let tester = [str("<="), str(">="), str("<"), str(">")];
    let constructor = [
      ExprElement::LessEq,
      ExprElement::GreaterEq,
      ExprElement::Less,
      ExprElement::Greater,
    ];
    'outer: loop {
      let initial_pos = code.pos();
      for i in 0..tester.len() {
        let (test, make_element) = (&tester[i], &constructor[i]);
        if test(code.skip_space()).is_ok() {
          let second_expr = expect_expr_add(code.skip_space())?;
          expr = make_element(Box::new(expr), Box::new(second_expr));
          continue 'outer;
        }
      }
      code.unwind(initial_pos);
      break;
    }

    Ok(expr)
  }
  fn expect_expr_add(code: &mut SourceCode) -> TokenizeResult<ExprElement> {
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

    Ok(expr)
  }
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
      // cast
      if char('_')(code).is_ok() {
        return Ok(ExprElement::Cast(
          expect_type(code)?,
          Box::new(expr.element),
        ));
      }
      return Ok(expr.element);
    }
    match expect_if(code) {
      Ok(res) => return Ok(ExprElement::IfExpr(Box::new(res))),
      Err(TokenizeError::NoMatch) => (),
      Err(e) => return Err(e),
    }

    match expect_str_literal(code) {
      Ok(res) => return Ok(res),
      Err(TokenizeError::NoMatch) => (),
      Err(e) => return Err(e),
    }

    match expect_constant(code) {
      Ok(res) => return Ok(res),
      Err(TokenizeError::NoMatch) => (),
      Err(e) => return Err(e),
    }

    let use_pos = code.lines_and_cols();
    let ident = expect_ident(code)?;
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

    fn expect_str_literal(code: &mut SourceCode) -> TokenizeResult<ExprElement> {
      consumed(code, seq(vec![char('"'), until(char('"'))])).map_or_else(
        |_| {
          if char('"')(code).is_ok() {
            Err(TokenizeError::UnclosedDelimiter)
          } else {
            Err(TokenizeError::NoMatch)
          }
        },
        // s is surrounded by "
        |s| Ok(ExprElement::StrLiteral(s[1..s.len() - 1].to_string())),
      )
    }
    fn expect_constant(code: &mut SourceCode) -> TokenizeResult<ExprElement> {
      // boolean literal
      if expect_keyword(code, "false").is_ok() {
        return Ok(ExprElement::Int(0));
      }
      if expect_keyword(code, "true").is_ok() {
        return Ok(ExprElement::Int(1));
      }
      // numeric literal
      let mut sign = 1;
      if let Ok(()) = char('+')(code) {
      } else if let Ok(()) = char('-')(code) {
        sign = -1;
      }
      let Ok(constant) = consumed(code, mul(num())) else {
        return Err(TokenizeError::NoMatch);
      };
      let constant = constant
        .parse::<i64>()
        .or_else(|e| Err(TokenizeError::InvalidNumber(e.to_string())))?;
      Ok(ExprElement::Int(constant * sign))
    }
  }
}
#[derive(Debug, Clone)]
pub struct If {
  pub cond:      Expression,
  pub then:      Vec<Statement>,
  pub otherwise: Option<Vec<Statement>>,
}
// FIXME?
impl PartialEq for If {
  fn eq(&self, _: &Self) -> bool {
    false
  }
  fn ne(&self, _: &Self) -> bool {
    true
  }
}
fn expect_if(code: &mut SourceCode) -> TokenizeResult<If> {
  seq(vec![str("if"), mul(space())])(code.skip_space()).or(Err(TokenizeError::NoMatch))?;
  let cond = expect_expression(code)?;

  char('{')(code.skip_space()).or(Err(TokenizeError::Expected("{ <code>? }")))?;
  let then = expect_code(code.skip_space())?;
  char('}')(code.skip_space()).or(Err(TokenizeError::UnclosedDelimiter))?;

  let otherwise = if str("else")(code.skip_space()).is_ok() {
    code.skip_space();
    let begin = code.lines_and_cols();
    // `if {} else **{}**`
    if char('{')(code).is_ok() {
      let otherwise = expect_code(code.skip_space())?;
      char('}')(code.skip_space()).or(Err(TokenizeError::UnclosedDelimiter))?;
      Some(otherwise)
    } else {
      // (maybe) `if {} else **if ...**`
      match expect_if(code) {
        Ok(res) => {
          let expr = Expression {
            element: ExprElement::IfExpr(Box::new(res)),
            begin,
            end: code.lines_and_cols(),
          };
          Some(vec![Statement {
            kind: StatementKind::ExprStatement(expr),
            begin,
            end: code.lines_and_cols(),
          }])
        }
        Err(TokenizeError::NoMatch) => None,
        Err(e) => return Err(e),
      }
    }
  } else {
    None
  };

  Ok(If {
    cond,
    then,
    otherwise,
  })
}

/// consume and return Ident (keyword, fn name, var name, type name)
fn expect_ident(code: &mut SourceCode) -> TokenizeResult<String> {
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
// (Why does this function require lifetime parameter??)
fn expect_keyword<'a>(code: &'a mut SourceCode, keyword: &'static str) -> TokenizeResult<'a, ()> {
  let initial_pos = code.pos();
  let res = expect_ident(code);
  if res.is_ok_and(|res| res == keyword) {
    Ok(())
  } else {
    code.unwind(initial_pos);
    Err(TokenizeError::NoMatch)
  }
}
fn expect_type(code: &mut SourceCode) -> TokenizeResult<Type> {
  let pos = code.lines_and_cols();
  let type_ident = expect_ident(code).or(Err(TokenizeError::ExpectedType))?;
  Ok(Type { type_ident, pos })
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
              let StatementKind::VarDecl(res) = res else {
                unreachable!();
              };
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
            let StatementKind::FnDecl(res) = res else {
              unreachable!();
            };
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
      assert_eq!(
        func.code.len(),
        1,
        "(code: '{}', parsed : {:#?})",
        code,
        func.code
      );
      assert!(func.return_type.is_some());
      assert_eq!(func.return_type.unwrap().type_ident, "i32".to_string());
      let code = func.code[0].clone();
      let StatementKind::Return(expr) = code.kind else {
        panic!("Statement is wrong; expect: Return, actual: {:?}", code);
      };
      let expr = expr.expect("No return value expression");
      let ExprElement::Int(parsed_retval) = expr.element else {
        panic!("Expression is wrong; expect: Constant, actual: {:?}", expr);
      };
      assert_eq!(parsed_retval, retval);
    }
  }
  #[test]
  fn expect_fn_arguments() {
    for (code, expected_arg_names) in [
      ("fn single(a: i32) {}", vec!["a"]),
      (
        "fn triple_trailing(first:i32,second:i32,third:i32){}",
        vec!["first", "second", "third"],
      ),
    ] {
      let StatementKind::FnDecl(function) =
        expect_fn_declaration(&mut SourceCode::new(code)).expect(code)
      else {
        unreachable!();
      };
      for i in 0..expected_arg_names.len() {
        assert_eq!(function.args[i].name, expected_arg_names[i]);
        assert_eq!(function.args[i].vartype.type_ident, "i32");
      }
    }
  }

  #[test]
  fn test_expect_for_loop() {
    for (code, expect) in [
      ("for i in 0..9{}", Some(("i", false, 0, 9))),
      ("for j in -2..=5 {}", Some(("j", true, -2, 5))),
      ("for k in 1+2..5-2 {}", Some(("k", false, 3, 3))),
      ("for x in false .. =true {}", Some(("x", true, 0, 1))),
      (
        r"for
          y
          in
          0
          ..
          =
          1 {}",
        Some(("y", true, 0, 1)),
      ),
      ("for a in ..1{}", None),
      ("for b in 2.. {}", None),
      ("for d 3..4 {}", None),
    ] {
      let (for_loop, (varname, inclusive, begin, end)) =
        match expect_for_loop(&mut SourceCode::new(code)) {
          Ok(res) => {
            if let Some(expect) = expect {
              let StatementKind::ForLoop(res) = res else {
                unreachable!();
              };
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
      assert_eq!(&for_loop.varname, varname);
      assert_eq!(for_loop.inclusive, inclusive);
      assert_eq!(for_loop.begin.element._eval().unwrap(), begin);
      assert_eq!(for_loop.end.element._eval().unwrap(), end);
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
      let Ok(res) = expect_return(&mut SourceCode::new(&code)) else {
        if is_expected_success {
          panic!("code `{}` is expected to succeed but it fails", code);
        } else {
          continue;
        }
      };
      let StatementKind::Return(expr) = res else {
        unreachable!();
      };
      let expr = expr.expect("No return value expression");
      let ExprElement::Int(parsed_retval) = expr.element else {
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
      let StatementKind::Return(expr) = expect_return(&mut SourceCode::new(code))
        .unwrap_or_else(|e| panic!("failed to parse '{:?}' as return statement : {:?}", code, e))
      else {
        unreachable!()
      };
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
      let (parsed_setter_call, (varname, args)) =
        match expect_setter_call(&mut SourceCode::new(code)) {
          Ok(res) => {
            if let Some(expect) = expect {
              let StatementKind::SetterCall(res) = res else {
                unreachable!();
              };
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
      // arithmetic operating
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
      // comparison
      ("1 == 1", 1),
      ("1 != 1", 0),
      ("1 <= 1", 1),
      ("1 >= 1", 1),
      ("9 <  2", 0),
      ("7 >  1", 1),
      // logical operating
      ("0 || 1", 1),
      ("1 && 0", 0),
      ("true && false || 1+2*4", 1), // rhs of || is true (not zero)
    ] {
      let expr = expect_expression(&mut SourceCode::new(code))
        .unwrap_or_else(|e| panic!("code `{}` is not parsed as expression (err: {:?})", code, e));
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
  fn test_expect_expression_cast() {
    for (code, expect_expr, expect_cast_type) in [
      ("(-1)_i64", Some(-1), Some("i64")),
      ("(0)_ i32", None, None),
      ("(1) _i64", Some(1), None),
      ("(2) _ i32", Some(2), None),
    ] {
      let (expr, expect_expr) = match expect_expression(&mut SourceCode::new(code)) {
        Ok(res) => {
          if let Some(expect) = expect_expr {
            (res, expect)
          } else {
            panic!(
              "code `{}` is successfully parsed but it isn't expected (parsed: {:?})",
              code, res
            );
          }
        }
        Err(e) => {
          if expect_expr.is_none() {
            continue;
          } else {
            panic!("code `{}` is not parsed as expression (err: {:?})", code, e);
          }
        }
      };
      let ExprElement::Cast(to_type, inner_expr) = expr.element else {
        if expect_cast_type.is_none() {
          continue;
        } else {
          panic!("code `{}` is not parsed as Cast", code);
        }
      };
      assert_eq!(to_type.type_ident, expect_cast_type.unwrap());
      assert_eq!(inner_expr._eval().unwrap(), expect_expr);
    }
  }

  #[test]
  fn test_expect_expression_str() {
    for (code, expect) in [
      (r#""hello""#, Some("hello")),
      (r#""first""second""#, Some("first")),
      (r#""hello"#, None),
      (r"'hello'", None),
    ] {
      let (expr, expect) = match expect_expression(&mut SourceCode::new(code)) {
        Ok(res) => {
          if let Some(expect) = expect {
            (res, expect)
          } else {
            panic!(
              "code `{}` is successfully parsed but it isn't expected (parsed: {:?})",
              code, res
            );
          }
        }
        Err(e) => {
          if expect.is_none() {
            continue;
          } else {
            panic!("code `{}` is not parsed as expression (err: {:?})", code, e);
          }
        }
      };
      let ExprElement::StrLiteral(parsed) = expr.element else {
        panic!("code `{}` is not parsed as StrLiteral", code);
      };
      assert_eq!(&parsed, expect);
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
  fn expect_expression_succeed_to_parse_if() {
    let code = "if 0{1}";
    let if_expr = expect_if(&mut SourceCode::new(code))
      .unwrap_or_else(|e| panic!("failed to parse `{}` : {:?}", code, e));
    assert_eq!(if_expr.cond.element._eval().unwrap(), 0);
    let StatementKind::ExprStatement(ref parsed_then) = if_expr.then[0].kind else {
      panic!(
        "first statement of `then` block is not expression (actual: {:?}, code: {})",
        if_expr.then[0].kind, code
      );
    };
    assert_eq!(parsed_then.element._eval().unwrap(), 1);
    assert!(if_expr.otherwise.is_none());
  }
  #[test]
  fn expect_expression_succeed_to_parse_if_else() {
    let code = "if 0 {1} else {2}";
    let if_expr = expect_if(&mut SourceCode::new(code))
      .unwrap_or_else(|e| panic!("failed to parse `{}` : {:?}", code, e));
    assert_eq!(if_expr.cond.element._eval().unwrap(), 0);
    let StatementKind::ExprStatement(ref parsed_then) = if_expr.then[0].kind else {
      panic!(
        "first statement of `then` block is not expression (actual: {:?}, code: {})",
        if_expr.then[0].kind, code
      );
    };
    assert_eq!(parsed_then.element._eval().unwrap(), 1);
    let parsed_otherwise = if_expr.otherwise.unwrap();
    let StatementKind::ExprStatement(ref parsed_otherwise) = parsed_otherwise[0].kind else {
      panic!(
        "first statement of `else` block is not expression (actual: {:?}, code: {})",
        parsed_otherwise[0].kind, code
      );
    };
    assert_eq!(parsed_otherwise.element._eval().unwrap(), 2);
  }
  #[test]
  fn expect_expression_succeed_to_parse_if_elif() {
    let code = "if 0 {1} else if 2 {3}";
    let if_expr = expect_if(&mut SourceCode::new(code))
      .unwrap_or_else(|e| panic!("failed to parse `{}` : {:?}", code, e));
    assert_eq!(if_expr.cond.element._eval().unwrap(), 0);
    let StatementKind::ExprStatement(ref parsed_then) = if_expr.then[0].kind else {
      panic!(
        "first statement of `then` block is not expression (actual: {:?}, code: {})",
        if_expr.then[0].kind, code
      );
    };
    assert_eq!(parsed_then.element._eval().unwrap(), 1);
    let parsed_otherwise = if_expr.otherwise.unwrap();
    let StatementKind::ExprStatement(ref parsed_otherwise) = parsed_otherwise[0].kind else {
      panic!(
        "first statement of `else` block is not expression (actual: {:?}, code: {})",
        parsed_otherwise[0].kind, code
      );
    };

    // else if
    let ExprElement::IfExpr(ref elif) = parsed_otherwise.element else {
      panic!(
        "first statement of `else` block is not 'if' expression (actual: {:?}, code: {})",
        parsed_otherwise, code
      );
    };
    assert_eq!(elif.cond.element._eval().unwrap(), 2);
    let StatementKind::ExprStatement(ref parsed_then) = elif.then[0].kind else {
      panic!(
        "first statement of `then` block is not expression (actual: {:?}, code: {})",
        elif.then[0].kind, code
      );
    };
    assert_eq!(parsed_then.element._eval().unwrap(), 3);
  }
  #[test]
  fn expect_identifier_returns_str_before_whitespace() {
    let code = "name01  name02";
    let res = expect_ident(&mut SourceCode::new(code));
    assert_eq!(res.unwrap(), "name01".to_string());
  }
  #[test]
  fn expect_identifier_fails_when_code_begins_with_number() {
    let code = "0name";
    let res = expect_ident(&mut SourceCode::new(code));
    assert!(res.is_err());
  }
}
