use std::{
  cell::{self, RefCell},
  collections::{HashMap, VecDeque},
  ops::Deref,
  rc::Rc,
};

use crate::{error::CompileError, tokenizer};

type CompileResult<T> = Result<T, Vec<CompileError>>;

#[derive(Debug)]
pub struct Program {
  functions: Vec<Function>,
}

#[derive(Debug, PartialEq)]
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
    if let Some(f) = self.owning_func.get(name) {
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
  args:        HashMap<String, Argument>,
  return_type: Option<Type>,
  code:        Vec<Statement>,
  variables:   Vec<Variable>,
}

#[derive(Debug)]
struct Argument {
  idx:     usize,
  name:    String,
  vartype: Type,
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
}
// express Expressions by stack machine
#[derive(Debug)]
struct Expression {
  // TODO: should have expression type
  expr_stack: Vec<ExprCommand>,
}
impl Expression {
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

    if errors.len() == 0 {
      Ok(Expression { expr_stack })
    } else {
      Err(errors)
    }
  }
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
      let mut appeared: HashMap<String, ()> = HashMap::new();
      // enumerate same level functions like Breadth-First Search
      // ex: fn top(){  fn f0(){}  fn f1(){}  } <- f0 and f1 is same level
      let mut same_level_func = Vec::new();
      for func in &functions {
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
          fn_idx,
          func,
          parent_func.clone(),
        )));
        {
          if let Some(ref parent) = parent_func {
            parent
              .borrow_mut()
              .owning_func
              .insert(func_name.clone(), func.clone());
          }
        }
        same_level_func.push(func);
        fn_idx += 1;
      }

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

    // Show result (temporary; for testing)
    uncompiled_functions.sort_by_key(|e| e.borrow().idx);
    for f in &uncompiled_functions {
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

    // compile UncompiledFunctions
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
    }
    let mut compiled_functions = Vec::new();
    for func in uncompiled_functions.into_iter() {
      match Function::compile(func) {
        Ok(compiled) => {
          if compiled.name == "main" && should_wrap_virtual_main {
            if compiled.variables.len() != 0 {
              errors.push(CompileError::GlobalVariableWithMain);
            }
          }
          compiled_functions.push(compiled);
        }
        Err(mut res) => errors.append(&mut res),
      }
    }
    println!(">>> functions: {:#?}", compiled_functions);

    if errors.len() == 0 {
      Ok(Program {
        functions: compiled_functions,
      })
    } else {
      Err(errors)
    }
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
    for statement in &uncompiled.code {
      match statement {
        tokenizer::Statement::FnDecl(_) => continue,
        tokenizer::Statement::VarDecl(var) => {
          let idx = func.variables.len();
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
          func.variables.push(Variable {
            idx,
            name: var.name.clone(),
            vartype,
            initial_value,
          });
          var_idx_map.insert(var.name.clone(), idx);
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

    if errors.len() == 0 {
      Ok(func)
    } else {
      Err(errors)
    }
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
