use std::{
  cell::RefCell,
  collections::{HashMap, VecDeque},
  rc::Rc,
};

use crate::{error::CompileError, tokenizer};

type CompileResult<T> = Result<T, Vec<CompileError>>;

#[derive(Debug)]
pub struct Program {
  functions: HashMap<String, Function>,
}

#[derive(Debug)]
enum Type {
  I32,
  I64,
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
  fn idx_from_accessible_fn_name(&self, name: String) -> Option<usize> {
    if let Some(f) = self.owning_func.get(&name) {
      return Some(f.borrow().idx);
    }
    if let Some(f) = self.owning_func.get(&name) {
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
  variables:   HashMap<String, Variable>,
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
  Return(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
  Constant(i32),
}

impl Program {
  pub fn from_statements(original_statements: Vec<tokenizer::Statement>) -> CompileResult<Self> {
    let mut errors = Vec::new();
    let mut uncompiled_functions: Vec<UncompiledFnCarrier> = Vec::new();

    let mut fn_idx = 0;
    let mut parent_func: Option<UncompiledFnCarrier> = None;
    let mut statements: Vec<tokenizer::Statement> = original_statements;
    let mut remaining_func: VecDeque<UncompiledFnCarrier> = VecDeque::new();
    loop {
      let mut appeared: HashMap<String, ()> = HashMap::new();
      // enumerate same level functions like Breadth-First Search
      // ex: fn top(){  fn f0(){}  fn f1(){}  } <- f0 and f1 is same level
      let mut same_level_func = Vec::new();
      for func in statements.iter().filter_map(|s| match s {
        tokenizer::Statement::FnDecl(f) => Some(f),
        _ => None,
      }) {
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
      statements = next.borrow().code.clone();
      uncompiled_functions.push(next);
    }
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

    // let program = {
    //   let main = UncompiledFunction::new(
    //     usize::MAX,
    //     "main".to_string(),
    //     vec![],
    //     None,
    //     statements,
    //     None,
    //   )?;
    //   if main.functions.get("main").is_none() {
    //     Program {
    //       functions: HashMap::from([("main".to_string(), main)]),
    //     }
    //   } else {
    //     if main.variables.len() != 0 {
    //       errors.push(CompileError::GlobalVariableWithMain);
    //     }
    //     // TODO: code.len shoud be 0?
    //     Program {
    //       functions: main.functions,
    //     }
    //   }
    // };

    // if errors.len() == 0 {
    //   Ok(program)
    // } else {
    Err(errors)
    // }
  }
}

impl UncompiledFunction {
  pub fn new(idx: usize, token: &tokenizer::Function, parent: Option<UncompiledFnCarrier>) -> Self {
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
}
