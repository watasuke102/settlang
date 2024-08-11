use tokenizer::{Declaration, Statement};

mod error;
mod parser;
mod tokenizer;

fn print_statement(statement: Statement, indent: usize) {
  let indent_space = "  ".repeat(indent);
  match statement {
    Statement::DeclStatement(decl) => match decl {
      Declaration::FnDecl(func) => {
        println!("{}[declare] fn {}() {{", indent_space, func.name);
        func
          .code
          .into_iter()
          .for_each(|s| print_statement(s, indent + 1));
        println!("{}}}", indent_space);
      }
      Declaration::VarDecl(var) => println!(
        "{}[variable] {}: type={:?}, initial_value={:?}",
        indent_space, var.name, var.vartype, var.initial_value
      ),
    },
    Statement::ExprStatement(expr) => {
      println!(
        "{}[expr] {:?} (eval: {:?})",
        indent_space,
        expr,
        expr.eval()
      )
    }
    Statement::Return(retval) => println!(
      "{}[return] retval: {:?} (eval: {:?})",
      indent_space,
      retval,
      retval.eval()
    ),
  }
}

fn main() {
  let input = r"
fn main() -> i32 {
  return 0
}
fn blank(){}
fn test() {
  -1
  2
  (+3)
  4+11   3-2
  10+1 * 6/3
  let value: i32 = 0
}
#*
fn inside_comment() {
}
*#
fn expr() {
  return 10+20 - 1+3*6/(1+1) - 2
}
fn variables() -> i32 {
  let a: i32 = 10
  let b: i32 = 5
  return a + b*2
}

";
  match tokenizer::expect_code(input) {
    Ok((statements, input)) => {
      println!(
        "consumed_input.len: {} || {}",
        input.len(),
        if input.len() == 0 {
          "Succeeded to parse!"
        } else {
          "Failed to parse"
        }
      );
      println!("=== parse result ===");
      statements.into_iter().for_each(|s| print_statement(s, 0))
    }
    Err(e) => {
      println!("Failed to parse ({:?})", e);
    }
  }
}
