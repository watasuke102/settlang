use crate::{compile::errors_or, error::CompileError, source_code, tokenizer};
use std::{
  cell::{OnceCell, Ref},
  collections::HashMap,
  ops::Deref,
};

use super::{
  i64_to_i32, AccessibleFnGetter, CompileResult, ImportMap, Statement, Type, UncompiledFunction,
  Variable,
};

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
  ImmI32(i32),
  ImmI64(i64),
  PushVar(usize, Type), // idx, vartype
  FnCall(usize, Type),  //idx, return type
  StdlibCall(usize, Type),
  CastI32ToI64,
  CastI64ToI32,
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
  /// if Expression is immediate, replace and return Ok
  /// return Err(None) if Expression is not Immediate or cannot cast
  /// return Err(Some) if errors are occured in casting
  pub(super) fn try_cast(
    &mut self,
    target: &Type,
    statement_begin: source_code::Position,
    statement_end: source_code::Position,
  ) -> Result<(), Option<Vec<CompileError>>> {
    if self.expr_stack.len() != 1 {
      return Err(None);
    }
    if let ExprCommand::ImmI64(imm) = self.expr_stack[0] &&
      *target == Type::I32
    {
      let imm = i64_to_i32(imm, statement_begin, statement_end).map_err(Option::Some)?;
      self.expr_stack[0] = ExprCommand::ImmI32(imm);
      self.result_type = Type::I32;
      return Ok(());
    }
    Err(None)
  }
  pub(super) fn from_token(
    token: &tokenizer::Expression,
    parent_func: &Ref<UncompiledFunction>,
    var_in_scope: &HashMap<String, Variable>,
    get_accessible_fn_by_name: &AccessibleFnGetter,
    imports: &mut ImportMap,
  ) -> CompileResult<Self> {
    if let Ok(imm) = token.element._eval() {
      return Ok(Expression {
        expr_stack:  vec![ExprCommand::ImmI64(imm)],
        result_type: Type::I64,
      });
    }

    let mut expr_stack = Vec::new();
    let mut errors = Vec::new();
    let mut result_type: OnceCell<Type> = OnceCell::new();

    // convert lhs and rhs (both are token), push them if no errors occured
    let mut extract = |lhs_token: &tokenizer::ExprElement,
                       rhs_token: &tokenizer::ExprElement,
                       push_if_succeed: ExprCommand| {
      let lhs = Expression::from_token(
        &token.replaced(lhs_token.clone()),
        parent_func,
        var_in_scope,
        get_accessible_fn_by_name,
        imports,
      )
      .or_else(|mut res| {
        errors.append(&mut res);
        Err(())
      });
      let rhs = Expression::from_token(
        &token.replaced(rhs_token.clone()),
        parent_func,
        var_in_scope,
        get_accessible_fn_by_name,
        imports,
      )
      .or_else(|mut res| {
        errors.append(&mut res);
        Err(())
      });
      let (Ok(mut lhs), Ok(mut rhs)) = (lhs, rhs) else {
        return;
      };
      // if the type is different, try casting
      if lhs.result_type != rhs.result_type {
        'try_cast: {
          match lhs.try_cast(&rhs.result_type, token.begin, token.end) {
            Ok(_) => break 'try_cast,
            Err(Some(mut err)) => {
              errors.append(&mut err);
              return;
            }
            Err(_) => (),
          }
          match rhs.try_cast(&lhs.result_type, token.begin, token.end) {
            Ok(_) => break 'try_cast,
            Err(Some(mut err)) => {
              errors.append(&mut err);
              return;
            }
            Err(_) => (),
          }
          errors.push(CompileError::MismatchBinopType(
            lhs.result_type.clone(),
            rhs.result_type.clone(),
            token.begin,
            token.end,
          ));
          return;
        }
      }
      // append lhs and rhs to expr stack
      {
        use ExprCommand::*;
        match push_if_succeed {
          // logic and/or is special; no equivalent instructions in WASM
          LogicAnd | LogicOr => {
            expr_stack.append(&mut lhs.expr_stack);
            // cast lhs to bool (lhs != 0)
            expr_stack.push(ExprCommand::ImmI64(0));
            expr_stack.push(ExprCommand::NonEq);
            // cast rhs to bool (lhs != 0)
            expr_stack.append(&mut rhs.expr_stack);
            expr_stack.push(ExprCommand::ImmI64(0));
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
      result_type.get_mut_or_init(|| lhs.result_type.clone());
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

      Int(imm) => match result_type.get_or_init(|| Type::I64) {
        Type::I32 => match i64_to_i32(*imm, token.begin, token.end) {
          Ok(imm) => expr_stack.push(ExprCommand::ImmI32(imm)),
          Err(mut e) => errors.append(&mut e),
        },
        Type::I64 => expr_stack.push(ExprCommand::ImmI64(*imm)),
        _ => panic!("Fail imm : {:?}", token), // FIXME: when this happen?
      },
      Variable(var_name, pos) => match var_in_scope.get(var_name) {
        Some(var) => {
          expr_stack.push(ExprCommand::PushVar(var.idx, var.vartype.clone()));
          result_type.get_or_init(|| var.vartype.clone());
        }
        None => errors.push(CompileError::UndefinedVariable(var_name.clone(), *pos)),
      },
      Cast(to_type, expr) => 'cast: {
        let mut expr = match Expression::from_token(
          &token.replaced(*expr.clone()),
          parent_func,
          var_in_scope,
          get_accessible_fn_by_name,
          imports,
        ) {
          Ok(res) => res,
          Err(mut e) => {
            errors.append(&mut e);
            break 'cast;
          }
        };
        let to_type = match Type::try_from(to_type.clone()) {
          Ok(t) => t,
          Err(e) => {
            errors.push(e);
            break 'cast;
          }
        };
        // if inner type is same with cast target type, no comand is required
        if expr.result_type != to_type {
          let Some(cast_command) = Type::cast(&expr.result_type, &to_type) else {
            errors.push(CompileError::InvalidCast(
              expr.result_type.clone(),
              to_type.clone(),
              token.begin,
              token.end,
            ));
            break 'cast;
          };
          expr.expr_stack.push(cast_command);
        }
        expr_stack.append(&mut expr.expr_stack);
        *result_type.get_mut_or_init(|| to_type.clone()) = to_type.clone();
      }
      FnCall(fn_name, arguments, pos) => 'fn_call: {
        // stdlib
        {
          let mut errors_arg = Vec::new();
          let mut args_expr = Vec::new();
          let mut args_type = Vec::new();
          for arg_expr in arguments {
            let mut expr = match Expression::from_token(
              arg_expr,
              parent_func,
              var_in_scope,
              get_accessible_fn_by_name,
              imports,
            ) {
              Ok(res) => res,
              Err(mut err) => {
                errors_arg.append(&mut err);
                continue;
              }
            };
            args_expr.append(&mut expr.expr_stack);
            args_type.push(expr.result_type);
          }
          if !errors_arg.is_empty() {
            errors.append(&mut errors_arg);
            break 'fn_call;
          }

          if let Some(stdlib) = imports.get(fn_name.clone(), args_type) {
            expr_stack.append(&mut args_expr);
            expr_stack.push(ExprCommand::StdlibCall(stdlib.idx, stdlib.return_type));
            result_type.get_or_init(|| stdlib.return_type.clone());
            break 'fn_call;
          }
        }
        // user-defined function
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
          imports,
        ) {
          Ok(mut expr) => {
            expr_stack.append(&mut expr);
            expr_stack.push(ExprCommand::FnCall(
              called_fn.idx,
              called_fn.return_type.clone(),
            ));
            result_type.get_or_init(|| called_fn.return_type.clone());
          }
          Err(mut e) => errors.append(&mut e),
        }
      }
      IfExpr(if_expr) => 'if_expr: {
        let cond = match Expression::from_token(
          &if_expr.cond,
          parent_func,
          var_in_scope,
          get_accessible_fn_by_name,
          imports,
        ) {
          Ok(res) => res,
          Err(mut e) => {
            errors.append(&mut e);
            break 'if_expr;
          }
        };
        let mut then = match Statement::vec_from_limited_block(
          &if_expr.then,
          parent_func,
          var_in_scope,
          get_accessible_fn_by_name,
          imports,
        ) {
          Ok(res) => res,
          Err(mut e) => {
            errors.append(&mut e);
            Vec::new()
          }
        };
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
          let mut otherwise = match Statement::vec_from_limited_block(
            &otherwise,
            parent_func,
            var_in_scope,
            get_accessible_fn_by_name,
            imports,
          ) {
            Ok(res) => res,
            Err(mut e) => {
              errors.append(&mut e);
              return None;
            }
          };
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
          result_type.get_or_init(|| then_result_type);
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
        result_type: result_type.get().unwrap_or(&Type::Void).clone(),
      },
    )
  }
}
pub(super) fn exprcomand_from_token_vec(
  token_vec: &Vec<tokenizer::Expression>,
  callee: &Ref<UncompiledFunction>,
  var_in_scope: &HashMap<String, Variable>,
  get_accessible_fn_by_name: &AccessibleFnGetter,
  imports: &mut ImportMap,
) -> CompileResult<Vec<ExprCommand>> {
  let mut errors = Vec::new();
  let mut arguments_expr = Vec::new();
  for (i, arg_expr) in token_vec.iter().enumerate() {
    let mut expr = match Expression::from_token(
      arg_expr,
      callee,
      var_in_scope,
      get_accessible_fn_by_name,
      imports,
    ) {
      Ok(res) => res,
      Err(mut err) => {
        errors.append(&mut err);
        continue;
      }
    };
    if expr.result_type == callee.args[i].vartype {
      arguments_expr.append(&mut expr.expr_stack);
    } else {
      match expr.try_cast(&callee.args[i].vartype, arg_expr.begin, arg_expr.end) {
        Ok(_) => arguments_expr.append(&mut expr.expr_stack),
        Err(Some(mut err)) => errors.append(&mut err),
        Err(None) => errors.push(CompileError::MismatchFnArgType(
          i,
          callee.name.clone(),
          callee.args[i].vartype.clone(),
          expr.result_type.clone(),
          arg_expr.begin,
          arg_expr.end,
        )),
      }
    }
  }
  errors_or(errors, arguments_expr)
}
