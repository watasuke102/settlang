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

#[derive(Debug, Clone, PartialEq)]
enum Type {
  I32,
  I64,
}
impl TryFrom<String> for Type {
  type Error = ();
  fn try_from(s: String) -> Result<Self, Self::Error> {
    match s.as_str() {
      "i32" => Ok(Type::I32),
      "i64" => Ok(Type::I64),
      _ => Err(()),
    }
  }
}

struct UncompiledFunction {
  idx:             usize,
  name:            String,
  args:            Vec<tokenizer::Argument>,
  return_type:     Option<String>,
  code:            Vec<tokenizer::Statement>,
  same_level_func: HashMap<String, UncompiledFnCarrier>,
  owning_func:     HashMap<String, UncompiledFnCarrier>,
  parent:          Option<UncompiledFnCarrier>,
}
impl UncompiledFunction {
  fn new(idx: usize, token: &tokenizer::Function, parent: Option<UncompiledFnCarrier>) -> Self {
    UncompiledFunction {
      idx,
      name: token.name.clone(),
      args: token.args.clone(),
      return_type: token.return_type.clone(),
      code: token.code.clone(),
      same_level_func: HashMap::new(),
      owning_func: HashMap::new(),
      parent,
    }
  }
  fn idx_from_accessible_fn_name(&self, name: &String) -> Option<usize> {
    if let Some(f) = self.owning_func.get(name) {
      return Some(f.borrow().idx);
    }
    if let Some(f) = self.same_level_func.get(name) {
      return Some(f.borrow().idx);
    }
    match &self.parent {
      Some(parent) => parent.borrow().idx_from_accessible_fn_name(name),
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
  return_type: Option<Type>,
  code:        Vec<Statement>,
  variables:   Vec<Variable>,
}

#[derive(Debug)]
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

#[derive(Debug, PartialEq)]
pub enum ExprCommand {
  Add,
  Sub,
  Mul,
  Div,
  PushImm(i32),
  PushVar(usize),
  FnCall(usize),
  GetInitialValueFromArg(usize), // index of arguments
}
// express Expressions by stack machine
#[derive(Debug)]
struct Expression {
  // TODO: should have expression type (Option<Type>)
  expr_stack: Vec<ExprCommand>,
}
impl Expression {
  fn arg(idx: usize) -> Self {
    Expression {
      expr_stack: vec![ExprCommand::GetInitialValueFromArg(idx)],
    }
  }
  fn from_token(
    token: &tokenizer::Expression,
    uncompiled: &cell::Ref<UncompiledFunction>,
    var_idx_map: &HashMap<String, usize>,
  ) -> CompileResult<Self> {
    let mut expr_stack = Vec::new();
    let mut errors = Vec::new();

    // convert lhs and rhs (both are token), push them if no errors occured
    let mut extract =
      |lhs: &tokenizer::Expression, rhs: &tokenizer::Expression, push_if_succeed: ExprCommand| {
        let mut temp_stack = Vec::new();
        let mut failed = false;
        for expr in [lhs, rhs] {
          match Expression::from_token(&expr, uncompiled, var_idx_map) {
            Ok(mut expr) => {
              temp_stack.append(&mut expr.expr_stack);
            }
            Err(mut err) => {
              failed = true;
              errors.append(&mut err);
            }
          }
        }
        if !failed {
          expr_stack.append(&mut temp_stack);
          expr_stack.push(push_if_succeed);
        }
      };
    use tokenizer::Expression::*;
    match token {
      Add(lhs, rhs) => extract(lhs.deref(), rhs.deref(), ExprCommand::Add),
      Sub(lhs, rhs) => extract(lhs.deref(), rhs.deref(), ExprCommand::Sub),
      Mul(lhs, rhs) => extract(lhs.deref(), rhs.deref(), ExprCommand::Mul),
      Div(lhs, rhs) => extract(lhs.deref(), rhs.deref(), ExprCommand::Div),
      Constant(v) => expr_stack.push(ExprCommand::PushImm(*v)),
      Variable(var_name) => match var_idx_map.get(var_name) {
        Some(var_idx) => expr_stack.push(ExprCommand::PushVar(*var_idx)),
        None => errors.push(CompileError::UndefinedVariable),
      },
      FnCall(fn_name, arguments) => 'fn_call: {
        let Some(fn_idx) = uncompiled.idx_from_accessible_fn_name(fn_name) else {
          errors.push(CompileError::UndefinedFunction);
          break 'fn_call;
        };
        // TODO: argument validation (number, type, etc?)
        let mut failed = false;
        let mut arguments_expr = arguments
          .iter()
          .flat_map(
            |expr| match Expression::from_token(expr, uncompiled, var_idx_map) {
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
          expr_stack.push(ExprCommand::FnCall(fn_idx));
        }
      }
    }

    errors_or(errors, Expression { expr_stack })
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
    let tokenizer::Statement::FnDecl(func) = func else {
      panic!("Unexpected statement (got: {:?})", func);
    };
    let func_name = func.name.clone();
    if appeared.get(&func_name).is_some() {
      errors.push(CompileError::DuplicatedDecl);
      continue;
    }
    appeared.insert(func_name.clone(), ());
    let func = Rc::new(RefCell::new(UncompiledFunction::new(
      *fn_idx,
      func,
      parent_func.clone(),
    )));
    // push parent's owning_func as well
    {
      if let Some(ref parent) = parent_func {
        parent
          .borrow_mut()
          .owning_func
          .insert(func_name.clone(), func.clone());
      }
    }
    same_level_func.push(func);
    *fn_idx += 1;
  }

  errors_or(errors, same_level_func)
}

impl Program {
  pub fn from_statements(statements: Vec<tokenizer::Statement>) -> CompileResult<Self> {
    let mut errors = Vec::new();
    let (mut functions, statements): (Vec<_>, Vec<_>) = statements
      .into_iter()
      .partition(|s| matches!(s, tokenizer::Statement::FnDecl(_)));
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
        .filter(|s| matches!(s, tokenizer::Statement::FnDecl(_)))
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
        return_type:     None,
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
      errors.push(CompileError::GlobalStatementWithMain);
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

    let return_type = uncompiled.return_type.as_ref().and_then(|t| {
      Type::try_from(t.clone())
        .or_else(|_| {
          errors.push(CompileError::InvalidType);
          Err(())
        })
        .ok()
    });
    let mut func = Function {
      idx: uncompiled.idx,
      name: uncompiled.name.clone(),
      return_type,
      ..Function::default()
    };
    let mut var_idx_map: HashMap<String, usize> = HashMap::new();
    for (i, arg) in uncompiled.args.iter().enumerate() {
      let Ok(vartype) = Type::try_from(arg.vartype.clone()) else {
        errors.push(CompileError::InvalidType);
        continue;
      };
      func.args.push(vartype.clone());
      func.add_variable(
        arg.name.clone(),
        vartype,
        Expression::arg(i),
        &mut var_idx_map,
      );
    }

    for statement in &uncompiled.code {
      match statement {
        tokenizer::Statement::FnDecl(_) => continue,
        tokenizer::Statement::VarDecl(var) => {
          let Ok(vartype) = Type::try_from(var.vartype.clone()) else {
            errors.push(CompileError::InvalidType);
            continue;
          };
          let initial_value =
            match Expression::from_token(&var.initial_value, &uncompiled, &var_idx_map) {
              Ok(expr) => expr,
              Err(mut res) => {
                errors.append(&mut res);
                continue;
              }
            };
          func.add_variable(var.name.clone(), vartype, initial_value, &mut var_idx_map);
        }
        tokenizer::Statement::ExprStatement(expr) => {
          match Expression::from_token(expr, &uncompiled, &var_idx_map) {
            Ok(expr) => func.code.push(Statement::ExprStatement(expr)),
            Err(mut res) => errors.append(&mut res),
          }
        }
        tokenizer::Statement::Return(expr) => {
          let retval = match Expression::from_token(expr, &uncompiled, &var_idx_map) {
            Ok(expr) => expr,
            Err(mut res) => {
              errors.append(&mut res);
              continue;
            }
          };
          func.code.push(Statement::Return(retval));
        }
      }
    }

    // TODO: check return type and existance of Return statement

    errors_or(errors, func)
  }
  fn add_variable(
    &mut self,
    name: String,
    vartype: Type,
    initial_value: Expression,
    var_idx_map: &mut HashMap<String, usize>,
  ) {
    let idx = self.variables.len();
    var_idx_map.insert(name.clone(), idx);
    self.variables.push(Variable {
      idx,
      name,
      vartype,
      initial_value,
    });
  }
}

#[cfg(test)]
mod test {
  use super::*;
  #[test]
  fn convert_type_from_string() {
    assert_eq!(Type::try_from("i32".to_string()), Ok(Type::I32));
    assert_eq!(Type::try_from("i64".to_string()), Ok(Type::I64));
    assert!(Type::try_from("i65535".to_string()).is_err());
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
