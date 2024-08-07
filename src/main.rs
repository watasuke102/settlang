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
    },
    Statement::ExprStatement(expr) => {
      println!("{}[expr] {:?} (eval: {})", indent_space, expr, expr.eval())
    }
    Statement::Return(retval) => println!(
      "{}[return] retval: {:?} (eval: {})",
      indent_space,
      retval,
      retval.eval()
    ),
  }
}

fn main() {
  let input = r"
fn main() {
  return 0 
}
fn test() {
  -1
  2
  4+11   3-2
  10+1 * 6/3
  return 128
}
fn expr() {
  return 10+20 - 1+3*6/(1+1) - 2
}
";
  println!("=== parse result ===");
  tokenizer::expect_code(input)
    .unwrap()
    .0
    .into_iter()
    .for_each(|s| print_statement(s, 0));
}
