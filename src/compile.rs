use std::{
  cell::RefCell,
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
#[derive(Default)]
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

type UncompiledFnCarrier = Rc<RefCell<UncompiledFunction>>;
type AccessibleFnGetter<'uncompiled_function_lifetime> =
  Box<dyn Fn(&String) -> Option<UncompiledFnCarrier> + 'uncompiled_function_lifetime>;
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
    var_in_scope: &HashMap<String, Variable>,
    get_accessible_fn_by_name: &AccessibleFnGetter,
  ) -> CompileResult<Self> {
    let mut expr_stack = Vec::new();
    let mut errors = Vec::new();
    let mut result_type = Type::I32;

    // convert lhs and rhs (both are token), push them if no errors occured
    let mut extract =
      |lhs: &tokenizer::ExprElement, rhs: &tokenizer::ExprElement, push_if_succeed: ExprCommand| {
        let lhs = Expression::from_token(
          &token.replaced(lhs.clone()),
          var_in_scope,
          get_accessible_fn_by_name,
        )
        .or_else(|mut res| {
          errors.append(&mut res);
          Err(())
        });
        let rhs = Expression::from_token(
          &token.replaced(rhs.clone()),
          var_in_scope,
          get_accessible_fn_by_name,
        )
        .or_else(|mut res| {
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
              token.begin,
              token.end,
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
    use tokenizer::ExprElement::*;
    match &token.element {
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
        None => errors.push(CompileError::UndefinedVariable(var_name.clone(), *pos)),
      },
      FnCall(fn_name, arguments, pos) => 'fn_call: {
        let Some(called_fn) = get_accessible_fn_by_name(fn_name) else {
          errors.push(CompileError::UndefinedFunction(fn_name.clone(), *pos));
          break 'fn_call;
        };
        let called_fn = called_fn.borrow();
        if arguments.len() != called_fn.args.len() {
          errors.push(CompileError::WrongArgumentLen(
            called_fn.name.clone(),
            called_fn.args.len(),
            arguments.len(),
            *pos,
          ));
          break 'fn_call;
        }

        let mut failed = false;
        let mut arguments_expr = Vec::new();
        for (i, arg_expr) in arguments.iter().enumerate() {
          match Expression::from_token(arg_expr, var_in_scope, get_accessible_fn_by_name) {
            Ok(mut expr) => {
              if expr.result_type != called_fn.args[i].vartype {
                expr.expr_stack.push(ExprCommand::Cast(
                  expr.result_type.clone(),
                  called_fn.args[i].vartype.clone(),
                ));
              }
              arguments_expr.append(&mut expr.expr_stack);
            }
            Err(mut err) => {
              failed = true;
              errors.append(&mut err);
            }
          }
        }
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
      errors.push(CompileError::DuplicatedDecl(func_name, func.declared_pos));
      continue;
    }
    appeared.insert(func_name.clone(), ());
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
        statements.iter().map(|s| s.begin.lines).collect(),
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

    // return function that returns accessible (in-scope) function by name
    let get_accessible_fn_by_name: AccessibleFnGetter =
      Box::new(|name: &String| uncompiled.get_accessible_fn_by_name(name));
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
          let mut initial_value = match Expression::from_token(
            &var.initial_value,
            &var_in_scope,
            &get_accessible_fn_by_name,
          ) {
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
        ExprStatement(expr) => {
          match Expression::from_token(expr, &var_in_scope, &get_accessible_fn_by_name) {
            Ok(expr) => func.code.push(Statement::ExprStatement(expr)),
            Err(mut res) => errors.append(&mut res),
          }
        }
        Return(expr) => {
          let mut retval = match expr {
            Some(expr) => {
              match Expression::from_token(expr, &var_in_scope, &get_accessible_fn_by_name) {
                Ok(expr) => expr,
                Err(mut res) => {
                  errors.append(&mut res);
                  continue;
                }
              }
            }
            None => Expression {
              expr_stack:  Vec::new(),
              result_type: Type::Void,
            },
          };
          if func.return_type != retval.result_type {
            if func.return_type == Type::Void || retval.result_type == Type::Void {
              errors.push(CompileError::MismatchReturnExprType(
                func.return_type.clone(),
                retval.result_type,
                statement.begin,
                statement.end,
              ));
              continue;
            }
            retval.expr_stack.push(ExprCommand::Cast(
              retval.result_type.clone(),
              func.return_type.clone(),
            ));
          }
          func.code.push(Statement::Return(retval));
        }
      }
    }

    // raise error if the function has return type but does not have return statement
    if func.return_type != Type::Void &&
      !func.code.iter().any(|s| matches!(s, Statement::Return(_)))
    {
      errors.push(CompileError::ReturnNotFound(
        func.name.clone(),
        func.return_type.clone(),
      ));
    }

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
  use crate::source_code::{self, Position};

  use super::*;
  #[test]
  fn convert_type_from_string() {
    for (type_ident, expect) in [
      ("i32", Some(Type::I32)),
      ("i64", Some(Type::I64)),
      ("i65535", None),
    ] {
      let pos = source_code::Position::default();
      let vartype = tokenizer::Type {
        type_ident: type_ident.to_string(),
        pos,
      };
      assert_eq!(Type::try_from(vartype).ok(), expect);
    }
  }
  #[test]
  fn test_expression_from_token() {
    let varname = "a".to_string();
    let var_value = 10;
    let var_in_scope: HashMap<String, Variable> = HashMap::from([(
      varname.clone(),
      Variable {
        idx:           0,
        name:          varname.clone(),
        vartype:       Type::I32,
        initial_value: Expression {
          expr_stack:  vec![ExprCommand::PushImm(var_value)],
          result_type: Type::I32,
        },
      },
    )]);
    let fn_getter: AccessibleFnGetter = Box::new(|name| {
      assert_eq!(name, "f");
      let f = UncompiledFunction {
        idx: 0,
        name: "f".to_string(),
        return_type: Type::Void,
        ..Default::default()
      };
      Some(Rc::new(RefCell::new(f)))
    });
    let eval_expr = |expr: &Expression| -> Option<i32> {
      let mut stack: Vec<i32> = Vec::new();
      for command in expr.expr_stack.iter() {
        use ExprCommand::*;
        match command {
          Add => {
            let lhs = stack.pop().unwrap();
            let rhs = stack.pop().unwrap();
            stack.push(lhs + rhs);
          }
          Sub => {
            let lhs = stack.pop().unwrap();
            let rhs = stack.pop().unwrap();
            stack.push(lhs - rhs);
          }
          Mul => {
            let lhs = stack.pop().unwrap();
            let rhs = stack.pop().unwrap();
            stack.push(lhs * rhs);
          }
          Div => {
            let lhs = stack.pop().unwrap();
            let rhs = stack.pop().unwrap();
            stack.push(lhs / rhs);
          }
          Cast(_, _) => (), // FIXME?
          PushImm(v) => stack.push(*v),
          PushVar(idx) => {
            assert_eq!(*idx, 0);
            stack.push(var_value);
          }
          FnCall(idx) => assert_eq!(*idx, 0),
          _ => unreachable!(),
        }
      }
      assert!(stack.len() <= 1);
      stack.pop()
    };

    {
      use tokenizer::ExprElement::*;
      // test that is expected to succeed
      for (element, expect) in [
        // 2 * (4+6) = 20
        (
          Mul(
            Box::new(Constant(2)),
            Box::new(Add(Box::new(Constant(4)), Box::new(Constant(6)))),
          ),
          Some(20),
        ),
        // just call a function that return type is void
        (
          FnCall(
            "f".to_string(),
            Vec::new(),
            source_code::Position::default(),
          ),
          None,
        ),
      ] {
        let expr = Expression::from_token(
          &tokenizer::Expression {
            element: element.clone(),
            begin:   source_code::Position::default(),
            end:     source_code::Position::default(),
          },
          &var_in_scope,
          &fn_getter,
        )
        .unwrap_or_else(|_| panic!("Expr is expected to succeed: {:?}", element));
        assert_eq!(eval_expr(&expr), expect);
      }
      // test that is expected to fail
      for (element, expect_err) in [
        (
          Variable("b".to_string(), Position::default()),
          CompileError::UndefinedVariable("b".to_string(), Position::default()),
        ),
        (
          Add(
            Box::new(FnCall(
              "f".to_string(),
              Vec::new(),
              source_code::Position::default(),
            )),
            Box::new(Constant(1)),
          ),
          CompileError::InvalidCast(
            Type::Void,
            Type::I32,
            source_code::Position::default(),
            source_code::Position::default(),
          ),
        ),
      ] {
        let errors = Expression::from_token(
          &tokenizer::Expression {
            element: element.clone(),
            begin:   source_code::Position::default(),
            end:     source_code::Position::default(),
          },
          &var_in_scope,
          &fn_getter,
        )
        .err()
        .unwrap_or_else(|| panic!("Expr is expected to fail: {:?}", element));
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0], expect_err);
      }
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
