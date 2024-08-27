use crate::{compile, source_code};

#[derive(Debug, PartialEq)]
pub enum ParseError {
  NoMatch,
  PartialMatch(String),
  EmptyInput,
}

#[derive(Debug, PartialEq)]
pub enum TokenizeError {
  NoMatch,
  ExpectedExpression,
  Expected(&'static str),
  ExpectedType,
  ExpectedFunction,
  InvalidNumber(String),
  UnclosedDelimiter,
}
impl TokenizeError {
  pub fn to_string(&self, code: &source_code::SourceCode) -> String {
    let error_pos = code.lines_and_cols();
    format!(
      "[error] {} -> {:?}\n{}\nFailed to parse",
      error_pos,
      self,
      code.pointed_string(&error_pos)
    )
  }
}

#[derive(Debug, PartialEq)]
pub enum CompileError {
  UndefinedVariable(String, source_code::Position),
  UndefinedFunction(String, source_code::Position),
  DuplicatedDecl(String, source_code::Position),
  ReturnNotFound(String, compile::Type),
  // function name, expect, actual
  WrongArgumentLen(String, usize, usize, source_code::Position),
  InvalidType(String, source_code::Position),
  // from, to, begin, end
  InvalidCast(
    compile::Type,
    compile::Type,
    source_code::Position,
    source_code::Position,
  ),
  // expect, actual, begin, end
  MismatchReturnExprType(
    compile::Type,
    compile::Type,
    source_code::Position,
    source_code::Position,
  ),
  // setter name, varname, vartype, setter retval type
  MismatchSetterReturnType(
    String,
    String,
    compile::Type,
    compile::Type,
    source_code::Position,
  ),
  // 'then' type, 'otherwise' type, begin, end
  MismatchIfLastExpr(
    compile::Type,
    compile::Type,
    source_code::Position,
    source_code::Position,
  ),
  TryMutateImmutableVar(String, source_code::Position),
  NotAllowedStatementInLimitedBlock(source_code::Position,source_code::Position),
  GlobalStatementWithMain(Vec<usize>),
}
impl CompileError {
  pub fn to_string(&self, code: &source_code::SourceCode) -> String {
    use CompileError::*;
    match self {
        UndefinedVariable(name, pos) => format!(
          "[error] {} -> variable `{}` is undefined\n{}",
          pos,
          name,
          code.pointed_string(pos)
        ),
        UndefinedFunction(name, pos) => format!(
          "[error] {} -> function `{}` is undefined\n{}",
          pos,
          name,
          code.pointed_string(pos)
        ),
        ReturnNotFound(name, return_type) => format!(
          "[error] function `{}` has return type ({:?}) but does not have return statement",
          name, return_type,
        ),
        DuplicatedDecl(name, pos) => format!(
          "[error] {} -> `{}` is already defined\n{}",
          pos,
          name,
          code.pointed_string(pos)
        ),
        WrongArgumentLen(name, expect, actual, pos) => format!(
          "[error] {} -> function `{}` takes {} arg(s) but {} arg(s) was passed\n{}",
          pos,
          name,
          expect,
          actual,
          code.pointed_string(pos),
        ),
        InvalidType(name, pos) => format!(
          "[error] {} -> `{}` is invalid type name\n{}",
          pos,
          name,
          code.pointed_string(pos),
        ),
        InvalidCast(from, to, begin, end) => format!(
          "[error] {} -> cannot cast from {:?} to {:?}\n{}",
          begin,
          from,
          to,
          code.ranged_string(begin, end)
        ),
        MismatchSetterReturnType(setter_name,varname, vartype, setter_retval_type, pos) => format!(
          "[error] {} -> variable '{}' is {:?} but setter function '{}' returns {:?}\n{}",
          pos,
          varname,
          vartype,
          setter_name,
          setter_retval_type,
          code.pointed_string(pos)
        ),
        MismatchReturnExprType(expect, actual, begin, end) => format!(
        "[error] {} -> mismatched type; function return type is {:?} but return value is {:?}\n{}",
        begin,
        expect,
        actual,
        code.ranged_string(begin, end)
      ),
      MismatchIfLastExpr(then_type, otherwise_type, begin, end) => format!(
        "[error] {} -> last expression type of 'if' is different; if {{ {:?} }} else {{ {:?} }}\n{}",
        begin,
        then_type,
        otherwise_type, 
        code.ranged_string(begin, end)
      ),
        TryMutateImmutableVar(varname, pos) => format!(
          "[error] {} -> try to mutate the variable '{}' but it is immutable (does not have setter)\n{}",
          pos,
          varname,
          code.pointed_string(pos)
        ),
        NotAllowedStatementInLimitedBlock(begin,end)=> format!(
          "[error] {} -> this statement is not allowed in if/for block\n{}",
          begin,
          code.ranged_string(begin, end)
        ),
        GlobalStatementWithMain(lines) => {
          let lines = lines
            .into_iter()
            .map(|line| line.to_string())
            .collect::<Vec<String>>()
            .join(", ");
          [
            // insert width that is the same width of "[error]" before "lines: {}" to align output
            format!("[error] Statement(s) found in the outside of main()"),
            format!("        lines: {}", lines),
          ]
          .join("\n")
        }
      }
  }
}
