use std::{
  cell::{Ref, RefCell},
  collections::{HashMap, VecDeque},
  fmt,
  ops::Deref,
  rc::Rc,
};

use crate::{error::CompileError, source_code, tokenizer};

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
  pub functions: Vec<Function>,
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
    if (a == &Self::I32 && b == &Self::I64) || // _
       (a == &Self::I64 && b == &Self::I32)
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
  setter:  Option<tokenizer::Setter>,
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
          setter: arg.setter.clone(),
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
pub struct Function {
  pub idx:         usize,
  pub name:        String,
  pub args:        Vec<Type>,
  pub return_type: Type,
  pub code:        Vec<Statement>,
  pub variables:   Vec<Variable>,
}

#[derive(Clone)]
pub struct Variable {
  pub idx:           usize,
  pub vartype:       Type,
  setter:            Option<UncompiledFnCarrier>,
  pub initial_value: Expression,
}
impl fmt::Debug for Variable {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(
      f,
      "[{}: {:?}, initial={:?}, setter={:?}]",
      self.idx,
      self.vartype,
      self.initial_value,
      self.setter.as_ref().and_then(|f| Some(f.borrow().idx))
    )
  }
}

#[derive(Debug, Clone)]
pub enum Statement {
  ExprStatement(Expression, /** should_drop: */ bool),
  Return(Expression),
  SetterCall(MutationInfo),
}
#[derive(Debug, Clone)]
pub struct MutationInfo {
  pub setter:    usize,
  pub var:       usize,
  pub arg_stack: Vec<ExprCommand>,
}
impl Statement {
  fn new_return(
    expr: &Option<tokenizer::Expression>,
    parent_func: &Ref<UncompiledFunction>,
    statement_begin: source_code::Position,
    statement_end: source_code::Position,
    var_in_scope: &HashMap<String, Variable>,
    get_accessible_fn_by_name: &AccessibleFnGetter,
  ) -> CompileResult<Self> {
    let mut retval = match expr {
      Some(expr) => {
        Expression::from_token(expr, parent_func, &var_in_scope, &get_accessible_fn_by_name)?
      }
      None => Expression {
        expr_stack:  Vec::new(),
        result_type: Type::Void,
      },
    };
    if parent_func.return_type != retval.result_type {
      if parent_func.return_type == Type::Void || retval.result_type == Type::Void {
        return Err(vec![CompileError::MismatchReturnExprType(
          parent_func.return_type.clone(),
          retval.result_type,
          statement_begin,
          statement_end,
        )]);
      }
      retval.expr_stack.push(ExprCommand::Cast(
        retval.result_type.clone(),
        parent_func.return_type.clone(),
      ));
    }
    Ok(Statement::Return(retval))
  }
  fn new_setter_call(
    setter_call: &tokenizer::SetterCall,
    statement_begin: source_code::Position,
    var_in_scope: &HashMap<String, Variable>,
    get_accessible_fn_by_name: &AccessibleFnGetter,
  ) -> CompileResult<Self> {
    // get variable that will be mutated
    let Some(var) = var_in_scope.get(&setter_call.varname) else {
      return Err(vec![CompileError::UndefinedVariable(
        setter_call.varname.clone(),
        statement_begin,
      )])?;
    };
    // get function that is treated as setter
    let Some(ref setter) = var.setter else {
      return Err(vec![CompileError::TryMutateImmutableVar(
        setter_call.varname.clone(),
        statement_begin,
      )]);
    };
    let setter = setter.borrow();
    // prepare arguments of setter
    let arg_stack = exprcomand_from_token_vec(
      &setter_call.args,
      &setter,
      &var_in_scope,
      &get_accessible_fn_by_name,
    )?;
    Ok(Statement::SetterCall(MutationInfo {
      setter: setter.idx,
      var: var.idx,
      arg_stack,
    }))
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprCommand {
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  Less,
  LessEq,
  Greater,
  GreaterEq,
  Eq,
  NonEq,
  LogicOr,
  LogicAnd,
  BitOr,
  BitAnd,
  IfExpr(If),
  Cast(Type, Type), // from, to
  PushImm(i32),
  PushVar(usize),
  FnCall(usize),
  GetInitialValueFromArg(usize), // index of arguments
}
#[derive(Debug, Clone)]
pub struct If {
  pub cond:        Expression,
  pub then:        Vec<Statement>,
  pub otherwise:   Option<Vec<Statement>>,
  pub result_type: Type,
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
// express Expressions by stack machine
#[derive(Debug, Clone)]
pub struct Expression {
  pub expr_stack:  Vec<ExprCommand>,
  pub result_type: Type,
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
    parent_func: &Ref<UncompiledFunction>,
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
          parent_func,
          var_in_scope,
          get_accessible_fn_by_name,
        )
        .or_else(|mut res| {
          errors.append(&mut res);
          Err(())
        });
        let rhs = Expression::from_token(
          &token.replaced(rhs.clone()),
          parent_func,
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
        // cast if type does not match
        if lhs.result_type != result_type {
          lhs.expr_stack.push(ExprCommand::Cast(
            lhs.result_type.clone(),
            result_type.clone(),
          ));
        }
        if rhs.result_type != result_type {
          rhs.expr_stack.push(ExprCommand::Cast(
            rhs.result_type.clone(),
            result_type.clone(),
          ));
        }
        // append lhs and rhs to expr stack
        {
          use ExprCommand::*;
          match push_if_succeed {
            // logic and/or is special; no equivalent instructions in WASM
            LogicAnd | LogicOr => {
              expr_stack.append(&mut lhs.expr_stack);
              // cast lhs to bool (lhs != 0)
              expr_stack.push(ExprCommand::PushImm(0));
              expr_stack.push(ExprCommand::NonEq);
              // cast rhs to bool (lhs != 0)
              expr_stack.append(&mut rhs.expr_stack);
              expr_stack.push(ExprCommand::PushImm(0));
              expr_stack.push(ExprCommand::NonEq);
              // bool(rhs) 'and/or' bool(lhs)
              expr_stack.push(match push_if_succeed {
                LogicAnd => BitAnd,
                LogicOr => BitOr,
                _ => unreachable!(),
              });
            }
            // otherwise push normally
            _ => {
              expr_stack.append(&mut lhs.expr_stack);
              expr_stack.append(&mut rhs.expr_stack);
              expr_stack.push(push_if_succeed);
            }
          }
        }
      };

    use tokenizer::ExprElement::*;
    match &token.element {
      Add(lhs, rhs) => extract(lhs.deref(), rhs.deref(), ExprCommand::Add),
      Sub(lhs, rhs) => extract(lhs.deref(), rhs.deref(), ExprCommand::Sub),
      Mul(lhs, rhs) => extract(lhs.deref(), rhs.deref(), ExprCommand::Mul),
      Div(lhs, rhs) => extract(lhs.deref(), rhs.deref(), ExprCommand::Div),
      Mod(lhs, rhs) => extract(lhs.deref(), rhs.deref(), ExprCommand::Mod),
      Less(lhs, rhs) => extract(lhs.deref(), rhs.deref(), ExprCommand::Less),
      LessEq(lhs, rhs) => extract(lhs.deref(), rhs.deref(), ExprCommand::LessEq),
      Greater(lhs, rhs) => extract(lhs.deref(), rhs.deref(), ExprCommand::Greater),
      GreaterEq(lhs, rhs) => extract(lhs.deref(), rhs.deref(), ExprCommand::GreaterEq),
      Eq(lhs, rhs) => extract(lhs.deref(), rhs.deref(), ExprCommand::Eq),
      NonEq(lhs, rhs) => extract(lhs.deref(), rhs.deref(), ExprCommand::NonEq),
      LogicAnd(lhs, rhs) => extract(lhs.deref(), rhs.deref(), ExprCommand::LogicAnd),
      LogicOr(lhs, rhs) => extract(lhs.deref(), rhs.deref(), ExprCommand::LogicOr),

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
        match exprcomand_from_token_vec(
          arguments,
          &called_fn,
          var_in_scope,
          get_accessible_fn_by_name,
        ) {
          Ok(mut expr) => {
            expr_stack.append(&mut expr);
            expr_stack.push(ExprCommand::FnCall(called_fn.idx));
            result_type = called_fn.return_type.clone();
          }
          Err(mut e) => errors.append(&mut e),
        }
      }
      IfExpr(if_expr) => 'if_expr: {
        let map_statement_in_if = |statement: &tokenizer::Statement| {
          use tokenizer::StatementKind::*;
          match &statement.kind {
            ExprStatement(expr) => {
              Expression::from_token(expr, parent_func, &var_in_scope, &get_accessible_fn_by_name)
                .and_then(|expr| Ok(Statement::ExprStatement(expr, true)))
            }
            Return(expr) => Statement::new_return(
              expr,
              parent_func,
              statement.begin,
              statement.end,
              &var_in_scope,
              &get_accessible_fn_by_name,
            ),
            SetterCall(setter_call) => Statement::new_setter_call(
              setter_call,
              statement.begin,
              &var_in_scope,
              &get_accessible_fn_by_name,
            ),
            _ => Err(vec![CompileError::NotAllowedStatementInIf(statement.begin)]),
          }
        };

        let cond = match Expression::from_token(
          &if_expr.cond,
          parent_func,
          var_in_scope,
          get_accessible_fn_by_name,
        ) {
          Ok(res) => res,
          Err(mut e) => {
            errors.append(&mut e);
            break 'if_expr;
          }
        };
        let (then, err): (Vec<_>, Vec<_>) = if_expr
          .then
          .iter()
          .map(map_statement_in_if)
          .partition(Result::is_ok);
        err
          .into_iter()
          .for_each(|e| errors.append(&mut e.unwrap_err()));
        let mut then: Vec<Statement> = then.into_iter().map(Result::unwrap).collect();
        // if last statement of block is Expression, it is return value
        let mut then_result_type = Type::Void;
        if let Some(last) = then.last_mut() {
          if let Statement::ExprStatement(expr, _) = last {
            then_result_type = expr.result_type.clone();
            // the result of this expression must be keepen on the stack
            *last = Statement::ExprStatement(expr.clone(), false);
          }
        }
        // if 'if' has 'else', check its return value
        let mut otherwise_result_type = Type::Void;
        let otherwise = if_expr.otherwise.as_ref().and_then(|otherwise| {
          let (otherwise, err): (Vec<_>, Vec<_>) = otherwise
            .iter()
            .map(map_statement_in_if)
            .partition(Result::is_ok);
          err
            .into_iter()
            .for_each(|e| errors.append(&mut e.unwrap_err()));
          let mut otherwise: Vec<Statement> = otherwise.into_iter().map(Result::unwrap).collect();
          // if last statement of block is Expression, it is return value
          if let Some(last) = otherwise.last_mut() {
            if let Statement::ExprStatement(expr, _) = last {
              otherwise_result_type = expr.result_type.clone();
              // the result of this expression must be keepen on the stack
              *last = Statement::ExprStatement(expr.clone(), false);
            }
          }
          Some(otherwise)
        });
        // 'then' result type and 'otherwise' result type must be same
        if then_result_type == otherwise_result_type {
          expr_stack.push(ExprCommand::IfExpr(If {
            cond,
            then,
            otherwise,
            result_type: then_result_type.clone(),
          }));
          result_type = then_result_type;
        } else {
          errors.push(CompileError::MismatchIfLastExpr(
            then_result_type.clone(),
            otherwise_result_type,
            token.begin,
            token.end,
          ));
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
fn exprcomand_from_token_vec(
  token_vec: &Vec<tokenizer::Expression>,
  callee: &Ref<UncompiledFunction>,
  var_in_scope: &HashMap<String, Variable>,
  get_accessible_fn_by_name: &AccessibleFnGetter,
) -> CompileResult<Vec<ExprCommand>> {
  let mut errors = Vec::new();
  let mut arguments_expr = Vec::new();
  for (i, arg_expr) in token_vec.iter().enumerate() {
    match Expression::from_token(arg_expr, callee, var_in_scope, get_accessible_fn_by_name) {
      Ok(mut expr) => {
        if expr.result_type != callee.args[i].vartype {
          expr.expr_stack.push(ExprCommand::Cast(
            expr.result_type.clone(),
            callee.args[i].vartype.clone(),
          ));
        }
        arguments_expr.append(&mut expr.expr_stack);
      }
      Err(mut err) => {
        errors.append(&mut err);
      }
    }
  }
  errors_or(errors, arguments_expr)
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
    // _debug_log_uncompiled_functions(&uncompiled_functions);

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
      if let Some(err) = func.add_variable(
        arg.name.clone(),
        arg.vartype.clone(),
        &arg.setter,
        Expression::arg(i, arg.vartype.clone()),
        &uncompiled,
        &mut var_in_scope,
      ) {
        errors.push(err);
      }
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
            &uncompiled,
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
          if let Some(err) = func.add_variable(
            var.name.clone(),
            vartype,
            &var.setter,
            initial_value,
            &uncompiled,
            &mut var_in_scope,
          ) {
            errors.push(err);
          }
        }
        ExprStatement(expr) => {
          match Expression::from_token(expr, &uncompiled, &var_in_scope, &get_accessible_fn_by_name)
          {
            Ok(expr) => func.code.push(Statement::ExprStatement(expr, true)),
            Err(mut res) => errors.append(&mut res),
          }
        }
        Return(expr) => match Statement::new_return(
          expr,
          &uncompiled,
          statement.begin,
          statement.end,
          &var_in_scope,
          &get_accessible_fn_by_name,
        ) {
          Ok(stat) => func.code.push(stat),
          Err(mut e) => errors.append(&mut e),
        },
        SetterCall(setter_call) => match Statement::new_setter_call(
          setter_call,
          statement.begin,
          &var_in_scope,
          &get_accessible_fn_by_name,
        ) {
          Ok(stat) => func.code.push(stat),
          Err(mut e) => errors.append(&mut e),
        },
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
    setter: &Option<tokenizer::Setter>,
    initial_value: Expression,
    uncompiled: &Ref<UncompiledFunction>,
    var_in_scope: &mut HashMap<String, Variable>,
  ) -> Option<CompileError> {
    let mut var = Variable {
      idx: self.variables.len(),
      vartype: vartype.clone(),
      setter: None, // temporally
      initial_value,
    };
    let retval =
      setter.as_ref().and_then(
        |setter| match uncompiled.get_accessible_fn_by_name(&setter.name) {
          Some(setter_func) => {
            let borrowed_setter_func = setter_func.borrow();
            // return type of Setter must be matched with variable type
            if borrowed_setter_func.return_type == vartype {
              var.setter = Some(setter_func.clone());
              None
            } else {
              Some(CompileError::MismatchSetterReturnType(
                setter.name.clone(),
                name.clone(),
                vartype.clone(),
                borrowed_setter_func.return_type.clone(),
                setter.pos,
              ))
            }
          }
          None => Some(CompileError::UndefinedFunction(
            setter.name.clone(),
            setter.pos,
          )),
        },
      );

    var_in_scope.insert(name, var.clone());
    self.variables.push(var);

    retval
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
        vartype:       Type::I32,
        setter:        None,
        initial_value: Expression {
          expr_stack:  vec![ExprCommand::PushImm(var_value)],
          result_type: Type::I32,
        },
      },
    )]);
    let func = Rc::new(RefCell::new(UncompiledFunction {
      idx: 0,
      name: "f".to_string(),
      return_type: Type::Void,
      ..Default::default()
    }));
    let func_borrowed = func.borrow();
    let fn_getter: AccessibleFnGetter = Box::new(|name| {
      assert_eq!(name, "f");
      Some(func.clone())
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
          &func_borrowed,
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
          &func_borrowed,
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
