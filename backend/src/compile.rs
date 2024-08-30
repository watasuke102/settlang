use std::{
  cell::{Ref, RefCell},
  collections::{HashMap, VecDeque},
  fmt,
  rc::Rc,
};

use crate::{
  error::CompileError,
  source_code,
  stdlib::{self},
  tokenizer,
};

pub mod expression;

use expression::*;

type CompileResult<T> = Result<T, Vec<CompileError>>;
fn errors_or<T>(errors: Vec<CompileError>, res: T) -> CompileResult<T> {
  if errors.len() == 0 {
    Ok(res)
  } else {
    Err(errors)
  }
}
fn i64_to_i32(
  imm: i64,
  begin: source_code::Position,
  end: source_code::Position,
) -> CompileResult<i32> {
  match imm.try_into() {
    Ok(res) => Ok(res),
    Err(e) => Err(vec![CompileError::FailedToCastImm(
      imm.to_string(),
      Type::I32,
      e.to_string(),
      begin,
      end,
    )]),
  }
}

#[derive(Debug)]
pub struct Program {
  pub functions: Vec<Function>,
  pub imports:   Vec<Import>,
  pub data:      Vec<u8>, // \0 separated String
}
#[derive(Debug, Default)]
pub struct ImportMap {
  map: HashMap<(String, Vec<Type>), Import>,
}
impl ImportMap {
  pub fn get(&mut self, name: String, args: Vec<Type>) -> Option<Import> {
    if let Some(import) = self.map.get(&(name.clone(), args.clone())) {
      Some(import.clone())
    } else if let Some(stdlib) = stdlib::get_stdlib(&name, &args) {
      let import = Import {
        idx:         self.map.len(),
        module_name: stdlib.module_name,
        name:        stdlib.name.clone(),
        args:        args.clone(),
        return_type: stdlib.return_type,
      };
      self
        .map
        .insert((name.clone(), args.clone()), import.clone());
      Some(import)
    } else {
      None
    }
  }
}
#[derive(Debug, Clone)]
pub struct Import {
  pub idx:         usize,
  pub module_name: String,
  pub name:        String,
  pub args:        Vec<Type>,
  pub return_type: Type,
}

#[derive(Debug, Default)]
pub struct StringData {
  map:  HashMap<String, usize>, //offset
  data: Vec<u8>,
}
impl StringData {
  pub fn get_offset(&mut self, data: &String) -> usize {
    match self.map.get(data) {
      Some(offset) => *offset,
      None => {
        let offset = self.data.len();
        let mut data = data.clone();
        data.push('\0');
        self.data.extend_from_slice(data.as_bytes());
        self.map.insert(data, offset);
        offset
      }
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub enum Type {
  Void,
  #[default]
  I32,
  I64,
  StrLiteral,
}
impl Type {
  fn cast(from: &Self, to: &Self) -> Option<ExprCommand> {
    debug_assert_ne!(from, to, "make sure from != to before call Type::cast");
    use Type::*;
    match (from, to) {
      (I32, I64) => Some(ExprCommand::CastI32ToI64),
      (I64, I32) => Some(ExprCommand::CastI64ToI32),
      _ => None,
    }
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
  pub idx:     usize,
  pub vartype: Type,
  setter:      Option<UncompiledFnCarrier>,
}
impl fmt::Debug for Variable {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(
      f,
      "[{}: {:?}, setter={:?}]",
      self.idx,
      self.vartype,
      self.setter.as_ref().and_then(|f| Some(f.borrow().idx))
    )
  }
}

#[derive(Debug, Clone)]
pub enum Statement {
  VarInitialize(usize, Expression),
  ExprStatement(Expression, /** should_drop: */ bool),
  Return(Expression),
  SetterCall(MutationInfo),
  ForLoop(For),
}
#[derive(Debug, Clone)]
pub struct MutationInfo {
  pub setter:    usize,
  pub var:       usize,
  pub arg_stack: Vec<ExprCommand>,
}
#[derive(Debug, Clone)]
pub struct For {
  pub cnt_var_idx: usize,
  pub begin:       Expression,
  pub end:         Expression,
  pub code:        Vec<Statement>,
}
impl Statement {
  fn new_return(
    expr: &Option<tokenizer::Expression>,
    parent_func: &Ref<UncompiledFunction>,
    statement_begin: source_code::Position,
    statement_end: source_code::Position,
    var_in_scope: &HashMap<String, Variable>,
    get_accessible_fn_by_name: &AccessibleFnGetter,
    imports: &mut ImportMap,
    str_data: &mut StringData,
  ) -> CompileResult<Self> {
    let mut retval = match expr {
      Some(expr) => Expression::from_token(
        expr,
        parent_func,
        &var_in_scope,
        &get_accessible_fn_by_name,
        imports,
        str_data,
      )?,
      None => Expression {
        expr_stack:  Vec::new(),
        result_type: Type::Void,
      },
    };
    if parent_func.return_type == retval.result_type {
      return Ok(Statement::Return(retval));
    }

    match retval.try_cast(&parent_func.return_type, statement_begin, statement_end) {
      Ok(_) => Ok(Statement::Return(retval)),
      Err(Some(err)) => Err(err),
      _ => Err(vec![CompileError::MismatchReturnExprType(
        parent_func.return_type,
        retval.result_type,
        statement_begin,
        statement_end,
      )]),
    }
  }
  fn new_setter_call(
    setter_call: &tokenizer::SetterCall,
    statement_begin: source_code::Position,
    var_in_scope: &HashMap<String, Variable>,
    get_accessible_fn_by_name: &AccessibleFnGetter,
    imports: &mut ImportMap,
    str_data: &mut StringData,
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
      imports,
      str_data,
    )?;
    Ok(Statement::SetterCall(MutationInfo {
      setter: setter.idx,
      var: var.idx,
      arg_stack,
    }))
  }
  // FIXME: REMOVE THIS after implementing block
  fn vec_from_limited_block(
    token_vec: &Vec<tokenizer::Statement>,
    parent_func: &Ref<UncompiledFunction>,
    var_in_scope: &HashMap<String, Variable>,
    get_accessible_fn_by_name: &AccessibleFnGetter,
    imports: &mut ImportMap,
    str_data: &mut StringData,
  ) -> CompileResult<Vec<Statement>> {
    let (res, errors): (Vec<_>, Vec<_>) = token_vec
      .iter()
      .map(|statement| {
        use tokenizer::StatementKind::*;
        match &statement.kind {
          ExprStatement(expr) => Expression::from_token(
            expr,
            parent_func,
            &var_in_scope,
            &get_accessible_fn_by_name,
            imports,
            str_data,
          )
          .and_then(|expr| Ok(Statement::ExprStatement(expr, true))),
          Return(expr) => Statement::new_return(
            expr,
            parent_func,
            statement.begin,
            statement.end,
            &var_in_scope,
            &get_accessible_fn_by_name,
            imports,
            str_data,
          ),
          SetterCall(setter_call) => Statement::new_setter_call(
            setter_call,
            statement.begin,
            &var_in_scope,
            &get_accessible_fn_by_name,
            imports,
            str_data,
          ),
          _ => Err(vec![CompileError::NotAllowedStatementInLimitedBlock(
            statement.begin,
            statement.end,
          )]),
        }
      })
      .partition(Result::is_ok);

    errors_or(
      errors
        .into_iter()
        .map(Result::unwrap_err)
        .flatten()
        .collect(),
      res.into_iter().map(Result::unwrap).collect(),
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
    let mut imports = ImportMap::default();
    let mut str_data = StringData::default();
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
      .map(|f| Function::compile(f, &mut imports, &mut str_data))
      .filter_map(|compiled| match compiled {
        Ok(compiled) => Some(compiled),
        Err(mut res) => {
          errors.append(&mut res);
          None
        }
      })
      .collect();
    let mut imports: Vec<Import> = imports.map.into_iter().map(|e| e.1).collect();
    imports.sort_by_key(|e| e.idx);

    errors_or(
      errors,
      Program {
        functions: compiled_functions,
        imports,
        data: str_data.data,
      },
    )
  }
}

impl Function {
  fn compile(
    uncompiled: UncompiledFnCarrier,
    imports: &mut ImportMap,
    str_data: &mut StringData,
  ) -> CompileResult<Self> {
    let mut errors = Vec::new();
    let uncompiled = uncompiled.borrow();

    let mut func = Function {
      idx: uncompiled.idx,
      name: uncompiled.name.clone(),
      args: uncompiled.args.iter().map(|arg| arg.vartype).collect(),
      return_type: uncompiled.return_type,
      ..Function::default()
    };
    let mut var_in_scope: HashMap<String, Variable> = HashMap::new();
    for arg in uncompiled.args.iter() {
      if let Err(err) = func.add_variable(
        arg.name.clone(),
        arg.vartype,
        &arg.setter,
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
            imports,
            str_data,
          ) {
            Ok(expr) => expr,
            Err(mut res) => {
              errors.append(&mut res);
              continue;
            }
          };
          if initial_value.result_type != vartype {
            match initial_value.try_cast(&vartype, statement.begin, statement.end) {
              Ok(_) => (),
              Err(Some(mut err)) => errors.append(&mut err),
              Err(None) => errors.push(CompileError::MismatchVarInitType(
                var.name.clone(),
                vartype,
                initial_value.result_type,
                statement.begin,
                statement.end,
              )),
            }
          }
          match func.add_variable(
            var.name.clone(),
            vartype,
            &var.setter,
            &uncompiled,
            &mut var_in_scope,
          ) {
            Ok(idx) => func.code.push(Statement::VarInitialize(idx, initial_value)),
            Err(e) => errors.push(e),
          }
        }
        ExprStatement(expr) => {
          match Expression::from_token(
            expr,
            &uncompiled,
            &var_in_scope,
            &get_accessible_fn_by_name,
            imports,
            str_data,
          ) {
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
          imports,
          str_data,
        ) {
          Ok(stat) => func.code.push(stat),
          Err(mut e) => errors.append(&mut e),
        },
        SetterCall(setter_call) => match Statement::new_setter_call(
          setter_call,
          statement.begin,
          &var_in_scope,
          &get_accessible_fn_by_name,
          imports,
          str_data,
        ) {
          Ok(stat) => func.code.push(stat),
          Err(mut e) => errors.append(&mut e),
        },
        ForLoop(for_loop) => 'for_loop: {
          let begin = match Expression::from_token(
            &for_loop.begin,
            &uncompiled,
            &var_in_scope,
            &get_accessible_fn_by_name,
            imports,
            str_data,
          ) {
            Ok(res) => res,
            Err(mut e) => {
              errors.append(&mut e);
              break 'for_loop;
            }
          };
          let mut end = match Expression::from_token(
            &for_loop.end,
            &uncompiled,
            &var_in_scope,
            &get_accessible_fn_by_name,
            imports,
            str_data,
          ) {
            Ok(res) => res,
            Err(mut e) => {
              errors.append(&mut e);
              break 'for_loop;
            }
          };
          // both begin and end should be same and integer
          if begin.result_type != end.result_type {
            errors.push(CompileError::MismatchForRangeType(
              begin.result_type,
              end.result_type,
              for_loop.begin.begin,
              for_loop.end.end,
            ));
            break 'for_loop;
          }
          if end.result_type == Type::I32 {
            if for_loop.inclusive {
              end.expr_stack.push(ExprCommand::ImmI32(1));
              end.expr_stack.push(ExprCommand::Add);
            }
          } else if end.result_type == Type::I64 {
            if for_loop.inclusive {
              end.expr_stack.push(ExprCommand::ImmI64(1));
              end.expr_stack.push(ExprCommand::Add);
            }
          } else {
            errors.push(CompileError::WrongForRangeType(
              end.result_type,
              for_loop.begin.begin,
              for_loop.end.end,
            ));
            break 'for_loop;
          }
          // store variable info that has the same name
          let prev_var = var_in_scope.remove(&for_loop.varname);
          // regist counter variable
          let cnt_var_idx = func
            .add_variable(
              for_loop.varname.clone(),
              begin.result_type,
              &None,
              &uncompiled,
              &mut var_in_scope,
            )
            .expect(
              "add_variable() only returns Err related to setter, so this is expected to succeed",
            );
          let code = match Statement::vec_from_limited_block(
            &for_loop.code,
            &uncompiled,
            &var_in_scope,
            &get_accessible_fn_by_name,
            imports,
            str_data,
          ) {
            Ok(res) => res,
            Err(mut e) => {
              errors.append(&mut e);
              Vec::new()
            }
          };
          func.code.push(Statement::ForLoop(For {
            cnt_var_idx,
            begin,
            end,
            code,
          }));
          // remove or restore counter variable
          if let Some(prev_var) = prev_var {
            var_in_scope.insert(for_loop.varname.clone(), prev_var.to_owned());
          } else {
            var_in_scope.remove(&for_loop.varname).unwrap();
          }
        }
      }
    }

    // raise error if the function has return type but does not have return statement
    if func.return_type != Type::Void &&
      !func.code.iter().any(|s| matches!(s, Statement::Return(_)))
    {
      errors.push(CompileError::ReturnNotFound(
        func.name.clone(),
        func.return_type,
      ));
    }

    errors_or(errors, func)
  }
  fn add_variable(
    &mut self,
    name: String,
    vartype: Type,
    setter: &Option<tokenizer::Setter>,
    uncompiled: &Ref<UncompiledFunction>,
    var_in_scope: &mut HashMap<String, Variable>,
  ) -> Result<usize, CompileError> {
    let idx = self.variables.len();
    let mut var = Variable {
      idx,
      vartype,
      setter: None, // temporally
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
                vartype,
                borrowed_setter_func.return_type,
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

    retval.map_or(Ok(idx), Result::Err)
  }
}

#[cfg(test)]
mod test {
  use tokenizer::ExprElement;

  use crate::source_code::Position;

  use super::*;
  #[test]
  fn convert_type_from_string() {
    for (type_ident, expect) in [
      ("i32", Some(Type::I32)),
      ("i64", Some(Type::I64)),
      ("i65535", None),
    ] {
      let pos = Position::default();
      let vartype = tokenizer::Type {
        type_ident: type_ident.to_string(),
        pos,
      };
      assert_eq!(Type::try_from(vartype).ok(), expect);
    }
  }
  #[test]
  fn test_expression_from_token() {
    let mut imports = ImportMap::default();
    let mut str_data = StringData::default();
    let varname = "a".to_string();
    let var_in_scope: HashMap<String, Variable> = HashMap::from([(
      varname.clone(),
      Variable {
        idx:     0,
        vartype: Type::I32,
        setter:  None,
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
    let eval_expr = |expr: &Expression| -> Option<i64> {
      let mut stack: Vec<i64> = Vec::new();
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
          ImmI32(v) => stack.push(*v as i64),
          ImmI64(v) => stack.push(*v),
          PushVar(idx, _) => {
            assert_eq!(*idx, 0);
            stack.push(10);
          }
          FnCall(idx, _) => assert_eq!(*idx, 0),
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
            Box::new(Int(2)),
            Box::new(Add(Box::new(Int(4)), Box::new(Int(6)))),
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
          &mut imports,
          &mut str_data,
        )
        .unwrap_or_else(|err| {
          panic!(
            "Expr is expected to succeed: expr={:?}, err={:?}",
            element, err
          )
        });
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
            Box::new(Int(1)),
          ),
          CompileError::MismatchBinopType(
            Type::Void,
            Type::I64,
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
          &mut imports,
          &mut str_data,
        )
        .err()
        .unwrap_or_else(|| panic!("Expr is expected to fail: {:?}", element));
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0], expect_err);
      }
    }
  }
  #[test]
  fn succeed_to_cast() {
    fn new(element: ExprElement) -> tokenizer::Expression {
      tokenizer::Expression {
        element,
        begin: Position::default(),
        end: Position::default(),
      }
    }
    let func: UncompiledFnCarrier = Rc::new(RefCell::new(UncompiledFunction {
      idx: 0,
      name: "f".to_string(),
      args: vec![Argument {
        name:    "x".to_string(),
        vartype: Type::I32,
        setter:  None,
      }],
      return_type: Type::I32,
      ..Default::default()
    }));
    let pos = Position::default();
    let varname = "a".to_string();
    let var_in_scope: HashMap<String, Variable> = [(
      varname.clone(),
      Variable {
        idx:     0,
        vartype: Type::I32,
        setter:  None,
      },
    )]
    .into_iter()
    .collect();
    let get_accessible_fn_by_name: AccessibleFnGetter = Box::new(|_: &String| None);
    let mut imports = ImportMap::default();
    let mut str_data = StringData::default();

    {
      use tokenizer::ExprElement::Int;
      let Statement::Return(return_stat) = Statement::new_return(
        &Some(new(Int(0))), // i64
        &func.borrow(),     // return type is i32
        pos,
        pos,
        &var_in_scope,
        &get_accessible_fn_by_name,
        &mut imports,
        &mut str_data,
      )
      .unwrap() else {
        unreachable!();
      };
      assert_eq!(return_stat.result_type, Type::I32);
    }
    {
      use tokenizer::ExprElement::{Add, Int, Variable};
      let expr = Expression::from_token(
        &new(Add(
          Box::new(Int(0)),                         // i64
          Box::new(Variable(varname.clone(), pos)), // i32
        )),
        &func.borrow(),
        &var_in_scope,
        &get_accessible_fn_by_name,
        &mut imports,
        &mut str_data,
      )
      .unwrap();
      assert_eq!(expr.expr_stack[0], ExprCommand::ImmI32(0));
      assert_eq!(expr.expr_stack[1], ExprCommand::PushVar(0, Type::I32));
      assert_eq!(expr.expr_stack[2], ExprCommand::Add);
    }
    // todo: function call, variable initialization
  }
}
