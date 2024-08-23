use std::io::Write;

fn main() {
  for (i, code) in [
    // basic
    r"
fn main() -> i32 {
  ret 1+1
}
",
    // no main
    r"
ret 2
",
    r"
fn f1() -> i32 {
  ret 1
}
fn f2() -> i32 {
  ret 2
}
ret f1() + f2() * 3
",
    // nest
    r"
fn f0() {
    fn f2() {
        fn f5() {
            f0() # ok
            # f8() <- NG
        }
        fn f6() -> i32 { ret 4 }
    }
}
fn f1() {
    fn f3() {
        fn f7() {
            f8() # ok
            fn f8() -> i32 { ret 8 }
        }
    }
    fn f4() -> i32 { ret 4 }
}
",
    // variables
    r"
fn variables() -> i32 {
  let a: i32 = 10
  let b: i32 = 5
  let c: i64 = 600
  let d: i64 = 10000
  let a: i64 = 500 # shadowing
}
",
    // some expressions
    r"
fn test() {
  -1
  2
  (+3)
  4+11   3-2
  10+1 * 6/3
  let value: i32 = 0
}
fn expr() {
  ret
    10+20
    -
    # comment is treated as spaces
    1+3*6/(1+1) - 2
}
#*
fn inside_comment() {
}
*#
",
    // function
    r"
fn add_2a_b(a: i32, b: i32) -> i64 {
  let a: i32 = a*2 # shadowing
  ret a+b
}
fn variables() -> i32 {
  let a: i32 = 10
  let b: i32 = 5
  ret a + b*2 + add_2a_b(5, b)
}
fn main() -> i32 {
  ret variables()
}
    ",
    // errors
    r"
fn error0( -> i32 {}
",
    r"
fn error1() 1+1
",
    r"
fn main() {}
(1)
(2)
(3)
(4)
(5)
ret 0
",
    "fn main(){}let x:i32 = 0",
    r"
fn no_return_type_but_has_return() { ret 0 }
fn no_return_statement() 
  -> i32 {
}
fn voidfunc(){}
voidfunc() # ok
# fn f() {}  fn f() -> i32 {}
let x: WrongType = 0
call_f()
ret variable
voidfunc() + 1 # ??
(2)
voidfunc(1, 2, 3)
",
    r"
fn f(){}
  f()
  +
  1
9",
  ]
  .iter()
  .enumerate()
  {
    println!("----------------------------------------------");
    // basically above examples start with '\n'
    // so print `input{}`, not `input\n{}`
    println!("=== input : ```{}```", code);
    println!("=== compile result");
    let mut code = backend::source_code::SourceCode::new(code);
    let program = match backend::compile(&mut code) {
      Ok(program) => {
        if code.remaining_len() != 0 {
          println!("[error] code is not consumed entierly");
          println!("        remaining=```{}```", code.remaining_code());
          continue;
        }
        program
      }
      Err(error) => {
        println!("{}", error);
        continue;
      }
    };
    println!("Succeeded to compile || {:#?}", program);
    match backend::builder::wasm::build(program) {
      Ok(wasm) => {
        let filename = format!("out/{}.wasm", i);
        match std::fs::File::create(&filename).and_then(|mut file| file.write_all(&wasm)) {
          Ok(_) => println!("Saved as '{}'", filename),
          Err(e) => println!("Failed to save WASM file : {:?}", e),
        }
      }
      Err(error) => println!("Failed to build WASM file : {}", error),
    }
  }
}
