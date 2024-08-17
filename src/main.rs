#![feature(stmt_expr_attributes)]
use compile::Program;
use source_code::SourceCode;
use tokenizer::Statement;

mod compile;
mod error;
mod parser;
mod source_code;
mod tokenizer;

fn print_statement(statement: &Statement, indent: usize) {
  let indent_space = "  ".repeat(indent);
  match statement {
    Statement::FnDecl(func) => {
      println!(
        "{}[declare] fn {}({}){} {{",
        indent_space,
        func.name,
        func
          .args
          .iter()
          .map(|e| format!("{}: {}", e.name, e.vartype))
          .collect::<Vec<String>>()
          .join(", "),
        if func.return_type.is_some() {
          format!(" -> {}", func.return_type.as_ref().unwrap())
        } else {
          "".to_string()
        },
      );
      func
        .code
        .iter()
        .for_each(|s| print_statement(&s, indent + 1));
      println!("{}}}", indent_space);
    }
    Statement::VarDecl(var) => println!(
      "{}[variable] {}: type={:?}, initial_value={:?}",
      indent_space, var.name, var.vartype, var.initial_value
    ),
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
  for code in [
    // basic
    r"
fn main() -> i32 {
  return 0
}
",
    // no main
    r"
return 2
",
    r"
fn f1() -> i32 {
  return 1
}
fn f2() -> i32 {
  return 2
}
return 3
",
    // nest
    r"
fn f0() {
    fn f2() {
        fn f5() {
            f0() # ok
            # f8() <- NG
        }
        fn f6() -> i32 {}
    }
}
fn f1() {
    fn f3() {
        fn f7() {
            f8() # ok
            fn f8() -> i32 {}
        }
    }
    fn f4() -> i32 {}
}
",
    // // some expressions
    // r"
    // fn blank(){}
    // fn test() {
    //   -1
    //   2
    //   (+3)
    //   4+11   3-2
    //   10+1 * 6/3
    //   let value: i32 = 0
    // }
    // fn expr() {
    //   return
    //     10+20
    //     -
    //     # comment is treated as spaces
    //     1+3*6/(1+1) - 2
    // }
    // #*
    // fn inside_comment() {
    // }
    // *#
    // ",
    // // function
    // r"
    // fn add(a: i32, b: i32) -> i32 {
    //   return a+b
    // }
    // fn variables() - > i32 {
    //   let a: i32 = 10
    //   let b: i32 = 5
    //   return a + b*2 + add(b, 5)
    // }
    // ",
    // // error 0
    // r"
    // fn error0( -> i32 {}
    // ",
    // // error 1
    // r"
    // fn error1() 1+1
    // ",
  ] {
    // basically above examples start with '\n'
    // so print `input{}`, not `input\n{}`
    println!("=== input{}", code);
    let mut code = SourceCode::new(code);
    println!("=== parse result");
    let statements = match tokenizer::expect_code(&mut code) {
      Ok(statements) => {
        println!(
          "consumed_input.len: {} || {}",
          code.remaining_len(),
          if code.remaining_len() == 0 {
            "Succeeded to parse!"
          } else {
            "Failed to parse"
          }
        );
        statements.iter().for_each(|s| print_statement(&s, 0));
        statements
      }
      Err(e) => {
        let error_pos = code.lines_and_cols();
        let line_str = error_pos.lines.to_string();
        println!("[error] {} -> {:?}", error_pos, e);
        println!(
          "  {} | {}",
          line_str,
          code.line(error_pos.lines).unwrap_or_default()
        );
        println!(
          "  {} | {}^",
          " ".repeat(line_str.len()),
          " ".repeat(error_pos.cols - 2)
        );
        continue;
      }
    };
    println!();
    println!("=== compile result");
    let prog = Program::from_statements(statements);
    match prog {
      Ok(prog) => println!("Succeeded to compile || {:?}", prog),
      Err(errors) => println!("Failed to compile || {:?}", errors),
    }
    println!("----------------------------------------------");
  }
}
