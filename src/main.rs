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
    Statement::Return(retval) => println!("{}[return] retval: {:?}", indent_space, retval),
  }
}

fn main() {
  let input = r"
fn main() {
  return 0 
}
fn test() {
  return 128
}
";
  println!("=== parse result ===");
  tokenizer::expect_code(input)
    .unwrap()
    .0
    .into_iter()
    .for_each(|s| print_statement(s, 0));
}
