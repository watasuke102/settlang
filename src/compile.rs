use std::{
  cell::{self, RefCell},
  collections::{HashMap, VecDeque},
  ops::Deref,
  rc::Rc,
};

use crate::{error::CompileError, tokenizer};

type CompileResult<T> = Result<T, Vec<CompileError>>;
fn errors_or<T>(errors: Vec<CompileError>, res: T) -> CompileResult<T> {
  if errors.len() == 0 {
    Ok(res)
  } else {
    Err(errors)
  }
}

#[derive(Debug)]
pub struct Program {
  functions: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum Type {
  Void,
  #[default]
  I32,
  I64,
}
impl Type {
  fn cast(a: &Self, b: &Self) -> Option<Self> {
    if a == &Self::Void || b == &Self::Void {
      return None;
    }
    if (a == &Self::I32 && b == &Self::I64) // _
     || (a == &Self::I64 && b == &Self::I32)
    {
      return Some(Self::I64);
    }
    if a == b {
      return Some(a.clone());
    }
    None
  }
}
impl TryFrom<tokenizer::Type> for Type {
  type Error = CompileError;
  fn try_from(s: tokenizer::Type) -> Result<Self, Self::Error> {
    match s.type_ident.as_str() {
      "i32" => Ok(Type::I32),
      "i64" => Ok(Type::I64),
      _ => Err(CompileError::InvalidType(s.type_ident, s.pos)),
    }
  }
}

struct Argument {
  name:    String,
  vartype: Type,
}
struct UncompiledFunction {
  idx:             usize,
  name:            String,
  args:            Vec<Argument>,
  return_type:     Type,
  code:            Vec<tokenizer::Statement>,
  same_level_func: HashMap<String, UncompiledFnCarrier>,
  owning_func:     HashMap<String, UncompiledFnCarrier>,
  parent:          Option<UncompiledFnCarrier>,
}
impl UncompiledFunction {
  fn new(
    idx: usize,
    token: &tokenizer::Function,
    parent: Option<UncompiledFnCarrier>,
  ) -> CompileResult<Self> {
    let mut errors = Vec::new();
    let return_type = token
      .return_type
      .as_ref()
      .and_then(|t| Type::try_from(t.clone()).map_err(|e| errors.push(e)).ok())
      .unwrap_or(Type::Void);

    let args = token
      .args
      .iter()
      .filter_map(|arg| match Type::try_from(arg.vartype.clone()) {
        Ok(vartype) => Some(Argument {
          name: arg.name.clone(),
          vartype,
        }),
        Err(e) => {
          errors.push(e);
          None
        }
      })
      .collect();
    errors_or(
      errors,
      UncompiledFunction {
        idx,
        name: token.name.clone(),
        args,
        return_type,
        code: token.code.clone(),
        same_level_func: HashMap::new(),
        owning_func: HashMap::new(),
        parent,
      },
    )
  }
  fn get_accessible_fn_by_name(&self, name: &String) -> Option<UncompiledFnCarrier> {
    if let Some(f) = self.owning_func.get(name) {
      return Some(f.clone());
    }
    if let Some(f) = self.same_level_func.get(name) {
      return Some(f.clone());
    }
    match &self.parent {
      Some(parent) => parent.borrow().get_accessible_fn_by_name(name),
      None => None,
    }
  }
}
type UncompiledFnCarrier = Rc<RefCell<UncompiledFunction>>;
#[derive(Debug, Default)]
struct Function {
  idx:         usize,
  name:        String,
  args:        Vec<Type>,
  return_type: Type,
  code:        Vec<Statement>,
  variables:   Vec<Variable>,
}

#[derive(Debug, Clone)]
struct Variable {
  idx:           usize,
  name:          String,
  vartype:       Type,
  initial_value: Expression,
}

#[derive(Debug)]
enum Statement {
  ExprStatement(Expression),
  Return(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprCommand {
  Add,
  Sub,
  Mul,
  Div,
  Cast(Type, Type), // from, to
  PushImm(i32),
  PushVar(usize),
  FnCall(usize),
  GetInitialValueFromArg(usize), // index of arguments
}
// express Expressions by stack machine
#[derive(Debug, Clone)]
struct Expression {
  expr_stack:  Vec<ExprCommand>,
  result_type: Type,
}
impl Expression {
  fn arg(idx: usize, result_type: Type) -> Self {
    Expression {
      expr_stack: vec![ExprCommand::GetInitialValueFromArg(idx)],
      result_type,
    }
  }
  fn from_token(
    token: &tokenizer::Expression,
    uncompiled: &cell::Ref<UncompiledFunction>,
    var_in_scope: &HashMap<String, Variable>,
  ) -> CompileResult<Self> {
    let mut expr_stack = Vec::new();
    let mut errors = Vec::new();
    let mut result_type = Type::I32;

    // convert lhs and rhs (both are token), push them if no errors occured
    let mut extract =
      |lhs: &tokenizer::Expression, rhs: &tokenizer::Expression, push_if_succeed: ExprCommand| {
        let lhs = Expression::from_token(&lhs, uncompiled, var_in_scope).or_else(|mut res| {
          errors.append(&mut res);
          Err(())
        });
        let rhs = Expression::from_token(&rhs, uncompiled, var_in_scope).or_else(|mut res| {
          errors.append(&mut res);
          Err(())
        });
        let (Ok(mut lhs), Ok(mut rhs)) = (lhs, rhs) else {
          return;
        };
        match Type::cast(&lhs.result_type, &rhs.result_type) {
          Some(t) => result_type = t,
          None => {
            errors.push(CompileError::InvalidCast(
              lhs.result_type.clone(),
              rhs.result_type.clone(),
            ));
            return;
          }
        }
        // lhs: push expr and cast if type does not match
        expr_stack.append(&mut lhs.expr_stack);
        if lhs.result_type != result_type {
          expr_stack.push(ExprCommand::Cast(
            lhs.result_type.clone(),
            result_type.clone(),
          ));
        }
        // rhs: push expr and cast if type does not match
        expr_stack.append(&mut rhs.expr_stack);
        if rhs.result_type != result_type {
          expr_stack.push(ExprCommand::Cast(
            rhs.result_type.clone(),
            result_type.clone(),
          ));
        }
        expr_stack.push(push_if_succeed);
      };
    use tokenizer::Expression::*;
    match token {
      Add(lhs, rhs) => extract(lhs.deref(), rhs.deref(), ExprCommand::Add),
      Sub(lhs, rhs) => extract(lhs.deref(), rhs.deref(), ExprCommand::Sub),
      Mul(lhs, rhs) => extract(lhs.deref(), rhs.deref(), ExprCommand::Mul),
      Div(lhs, rhs) => extract(lhs.deref(), rhs.deref(), ExprCommand::Div),
      Constant(v) => {
        result_type = Type::I32;
        expr_stack.push(ExprCommand::PushImm(*v));
      }
      Variable(var_name, pos) => match var_in_scope.get(var_name) {
        Some(var) => {
          expr_stack.push(ExprCommand::PushVar(var.idx));
          result_type = var.vartype.clone();
        }
        None => errors.push(CompileError::UndefinedVariable(
          var_name.clone(),
          pos.clone(),
        )),
      },
      FnCall(fn_name, arguments, pos) => 'fn_call: {
        let Some(called_fn) = uncompiled.get_accessible_fn_by_name(fn_name) else {
          errors.push(CompileError::UndefinedFunction(
            fn_name.clone(),
            pos.clone(),
          ));
          break 'fn_call;
        };
        let called_fn = called_fn.borrow();
        // TODO: argument validation (number, type, etc?)
        let mut failed = false;
        let mut arguments_expr = arguments
          .iter()
          .flat_map(
            |expr| match Expression::from_token(expr, uncompiled, var_in_scope) {
              Ok(expr) => expr.expr_stack,
              Err(mut err) => {
                failed = true;
                errors.append(&mut err);
                vec![]
              }
            },
          )
          .collect();
        if !failed {
          expr_stack.append(&mut arguments_expr);
          expr_stack.push(ExprCommand::FnCall(called_fn.idx));
          result_type = called_fn.return_type.clone();
        }
      }
    }

    errors_or(
      errors,
      Expression {
        expr_stack,
        result_type,
      },
    )
  }
}

/// enumerate same level functions like Breadth-First Search
/// ex: fn top(){  fn f0(){}  fn f1(){}  } <- f0 and f1 is same level
fn enumerate_same_level_functions(
  fn_idx: &mut usize,
  parent_func: &Option<UncompiledFnCarrier>,
  functions: &Vec<tokenizer::Statement>,
) -> CompileResult<Vec<UncompiledFnCarrier>> {
  let mut errors = Vec::new();
  let mut appeared: HashMap<String, ()> = HashMap::new();
  let mut same_level_func = Vec::new();

  for func in functions {
    let tokenizer::StatementKind::FnDecl(ref func) = func.kind else {
      panic!("Unexpected statement (got: {:?})", func);
    };
    let func_name = func.name.clone();
    if appeared.get(&func_name).is_some() {
      errors.push(CompileError::DuplicatedDecl(
        func_name,
        func.declared_pos.clone(),
      ));
      continue;
    }
    appeared.insert(func_name.clone(), ());
    // let func =
    match UncompiledFunction::new(*fn_idx, func, parent_func.clone()) {
      Ok(func) => {
        let func = Rc::new(RefCell::new(func));
        // push parent's owning_func as well
        if let Some(ref parent) = parent_func {
          parent
            .borrow_mut()
            .owning_func
            .insert(func_name.clone(), func.clone());
        }
        same_level_func.push(func);
        *fn_idx += 1;
      }
      Err(mut e) => errors.append(&mut e),
    }
  }

  errors_or(errors, same_level_func)
}

impl Program {
  pub fn from_statements(statements: Vec<tokenizer::Statement>) -> CompileResult<Self> {
    let mut errors = Vec::new();
    let (mut functions, statements): (Vec<_>, Vec<_>) = statements
      .into_iter()
      .partition(|s| matches!(s.kind, tokenizer::StatementKind::FnDecl(_)));
    let mut uncompiled_functions: Vec<UncompiledFnCarrier> = Vec::new();

    let mut fn_idx = 0;
    let mut parent_func: Option<UncompiledFnCarrier> = None;
    let mut remaining_func: VecDeque<UncompiledFnCarrier> = VecDeque::new();
    loop {
      let same_level_func =
        match enumerate_same_level_functions(&mut fn_idx, &parent_func, &functions) {
          Ok(res) => res,
          Err(mut res) => {
            errors.append(&mut res);
            return Err(errors);
          }
        };
      let same_level_func_map: HashMap<String, UncompiledFnCarrier> = same_level_func
        .iter()
        .map(|f| {
          let name = f.borrow().name.clone();
          (name, f.to_owned())
        })
        .collect();
      same_level_func.into_iter().for_each(|f| {
        f.borrow_mut().same_level_func = same_level_func_map.clone();
        remaining_func.push_back(f);
      });
      // prepare for next loop
      let Some(next) = remaining_func.pop_front() else {
        break;
      };
      parent_func = Some(next.clone());
      functions = next
        .borrow()
        .code
        .to_owned()
        .into_iter()
        .filter(|s| matches!(s.kind, tokenizer::StatementKind::FnDecl(_)))
        .collect();
      uncompiled_functions.push(next);
    }
    uncompiled_functions.sort_by_key(|e| e.borrow().idx);
    debug_log_uncompiled_functions(&uncompiled_functions);

    // wrap entire code by function `main() -> void` if main() is not defined
    let should_wrap_virtual_main = !uncompiled_functions
      .iter()
      .any(|f| f.borrow().name == "main");
    if should_wrap_virtual_main {
      let main = UncompiledFunction {
        idx:             uncompiled_functions.len(),
        name:            String::from("main"),
        args:            vec![],
        return_type:     Type::Void,
        code:            statements,
        same_level_func: HashMap::new(),
        owning_func:     uncompiled_functions
          .clone()
          .into_iter()
          .map(|f| {
            let name = f.borrow().name.clone();
            (name, f.to_owned())
          })
          .collect(),
        parent:          None,
      };
      uncompiled_functions.push(Rc::new(RefCell::new(main)));
    } else if statements.len() != 0 {
      errors.push(CompileError::GlobalStatementWithMain(
        statements.iter().map(|s| s.pos.lines).collect(),
      ));
    }

    // compile UncompiledFunctions
    let compiled_functions = uncompiled_functions
      .into_iter()
      .map(Function::compile)
      .filter_map(|compiled| match compiled {
        Ok(compiled) => Some(compiled),
        Err(mut res) => {
          errors.append(&mut res);
          None
        }
      })
      .collect();

    errors_or(
      errors,
      Program {
        functions: compiled_functions,
      },
    )
  }
}

impl Function {
  fn compile(uncompiled: UncompiledFnCarrier) -> CompileResult<Self> {
    let mut errors = Vec::new();
    let uncompiled = uncompiled.borrow();

    let mut func = Function {
      idx: uncompiled.idx,
      name: uncompiled.name.clone(),
      args: uncompiled
        .args
        .iter()
        .map(|arg| arg.vartype.clone())
        .collect(),
      return_type: uncompiled.return_type.clone(),
      ..Function::default()
    };
    let mut var_in_scope: HashMap<String, Variable> = HashMap::new();
    for (i, arg) in uncompiled.args.iter().enumerate() {
      func.add_variable(
        arg.name.clone(),
        arg.vartype.clone(),
        Expression::arg(i, arg.vartype.clone()),
        &mut var_in_scope,
      );
    }

    for statement in &uncompiled.code {
      use tokenizer::StatementKind::*;
      match &statement.kind {
        FnDecl(_) => continue,
        VarDecl(var) => {
          let vartype = match Type::try_from(var.vartype.clone()) {
            Ok(t) => t,
            Err(e) => {
              errors.push(e);
              continue;
            }
          };
          let mut initial_value =
            match Expression::from_token(&var.initial_value, &uncompiled, &var_in_scope) {
              Ok(expr) => expr,
              Err(mut res) => {
                errors.append(&mut res);
                continue;
              }
            };
          if initial_value.result_type != vartype {
            initial_value.expr_stack.push(ExprCommand::Cast(
              initial_value.result_type.clone(),
              vartype.clone(),
            ));
          }
          func.add_variable(var.name.clone(), vartype, initial_value, &mut var_in_scope);
        }
        ExprStatement(expr) => match Expression::from_token(expr, &uncompiled, &var_in_scope) {
          Ok(expr) => func.code.push(Statement::ExprStatement(expr)),
          Err(mut res) => errors.append(&mut res),
        },
        Return(expr) => {
          let retval = match Expression::from_token(expr, &uncompiled, &var_in_scope) {
            Ok(expr) => expr,
            Err(mut res) => {
              errors.append(&mut res);
              continue;
            }
          };
          // TODO: type check
          func.code.push(Statement::Return(retval));
        }
      }
    }

    // TODO: existance of Return statement

    errors_or(errors, func)
  }
  fn add_variable(
    &mut self,
    name: String,
    vartype: Type,
    initial_value: Expression,
    var_in_scope: &mut HashMap<String, Variable>,
  ) {
    let var = Variable {
      idx: self.variables.len(),
      name: name.clone(),
      vartype,
      initial_value,
    };
    var_in_scope.insert(name, var.clone());
    self.variables.push(var);
  }
}

#[cfg(test)]
mod test {
  use super::*;
  #[test]
  fn convert_type_from_string() {
    for (type_ident, expect) in [
      ("i32", Some(Type::I32)),
      ("i64", Some(Type::I64)),
      ("i65535", None),
    ] {
      let pos = crate::source_code::Position { lines: 0, cols: 0 };
      let vartype = tokenizer::Type {
        type_ident: type_ident.to_string(),
        pos,
      };
      assert_eq!(Type::try_from(vartype).ok(), expect);
    }
  }
}

// Show result (temporary; for testing)
fn debug_log_uncompiled_functions(uncompiled_functions: &Vec<UncompiledFnCarrier>) {
  for f in uncompiled_functions {
    fn map_to_strings(m: &HashMap<String, UncompiledFnCarrier>) -> String {
      m.iter()
        .map(|f| format!("'{}'={}, ", f.0, f.1.borrow().idx))
        .collect()
    }
    let f = f.borrow();
    println!(
      "{:2} : '{}', owned = {:<16} samelevel = {:<16}, parent:{:?}",
      f.idx,
      f.name,
      map_to_strings(&f.owning_func),
      map_to_strings(&f.same_level_func),
      f.parent
        .as_ref()
        .and_then(|f| Some(f.borrow().name.clone())),
    );
  }
}
