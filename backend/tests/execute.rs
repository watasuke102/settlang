use std::fs;

use wasmer::imports;

fn build_and_execute(name: &str, code: &str, expect: i64) {
  // build
  let mut code = backend::source_code::SourceCode::new(code);
  let program =
    backend::compile(&mut code).unwrap_or_else(|e| panic!("failed to build '{}'\n{}", name, e));
  let wasm = backend::builder::wasm::build(program)
    .unwrap_or_else(|e| panic!("failed to build '{}'\n{}", name, e));
  // prepare wasmer
  let mut store = wasmer::Store::default();
  let module = wasmer::Module::from_binary(&store, &wasm[0..])
    .unwrap_or_else(|e| panic!("failed to build '{}'\n{}", name, e));
  let import_object = imports! {};
  let instance = wasmer::Instance::new(&mut store, &module, &import_object)
    .unwrap_or_else(|e| panic!("failed to build '{}'\n{}", name, e));
  // execute
  let result = instance
    .exports
    .get_function("main")
    .unwrap_or_else(|e| panic!("failed to build '{}'\n{}", name, e))
    .call(&mut store, &[])
    .unwrap_or_else(|e| panic!("failed to build '{}'\n{}", name, e));
  let result = result[0]
    .i32()
    .map_or_else(|| result[0].i64(), |res| Some(res as i64))
    .unwrap_or_else(|| panic!("failed to build '{}'\nResult is None", name));
  assert_eq!(result, expect, "(test name: '{}')", name);
}

#[test]
fn test_example() {
  for f in fs::read_dir("../examples").unwrap().into_iter() {
    let f = f.unwrap();
    let file_name = f.file_name().into_string().unwrap();
    let chars: Vec<char> = file_name.chars().collect();
    let dot = chars.iter().position(|c| *c == '.').unwrap_or(0);
    // check extension
    if &file_name[dot + 1..] != "stt" {
      continue;
    }
    let underbar = chars.iter().position(|c| *c == '_').unwrap();
    let name = &file_name[underbar + 1..dot];
    let expect_result = file_name[0..underbar].parse::<i64>().unwrap();
    let code_string = fs::read_to_string(f.path()).unwrap();
    build_and_execute(name, &code_string, expect_result);
  }
}
