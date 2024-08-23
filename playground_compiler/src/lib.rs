#![feature(vec_into_raw_parts)]

#[cfg(not(target_arch = "wasm32"))]
compile_error!("target arch is not wasm");

#[no_mangle]
pub fn build(input_ptr: *mut u8, input_len: usize) -> *const u8 {
  let mut output: Vec<u8> = 0u32.to_be_bytes().to_vec();

  let (input, _, _) = String::with_capacity(input_len).into_raw_parts();
  let input = unsafe {
    core::ptr::copy(input_ptr, input, input_len);
    String::from_raw_parts(input, input_len, input_len)
  };
  let mut code = backend::source_code::SourceCode::new(&input);

  let mut contents = 'build: {
    let program = match backend::compile(&mut code) {
      Ok(program) => program,
      Err(e) => break 'build e.as_bytes().to_vec(),
    };
    if code.remaining_len() != 0 {
      break 'build format!(
        "[error] code is not consumed entierly\n>> remaining=```{}```",
        code.remaining_code()
      )
      .as_bytes()
      .to_vec();
    }

    match backend::builder::wasm::build(program) {
      Ok(wasm) => {
        output = 1u32.to_be_bytes().to_vec();
        wasm
      }
      Err(error) => format!("[error] failed to build WASM file : {}", error)
        .as_bytes()
        .to_vec(),
    }
  };

  output.extend(contents.len().to_be_bytes());
  output.append(&mut contents);

  output.into_raw_parts().0
}
