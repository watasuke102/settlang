use std::{env, fs, io::Write, process};

struct BuildOptions {
  output_path: Option<String>,
  input_path:  Option<String>,
  verbose:     bool,
}
impl BuildOptions {
  fn new() -> Self {
    BuildOptions {
      output_path: None,
      input_path:  None,
      verbose:     false,
    }
  }
}

fn main() {
  let mut built_options = BuildOptions::new();
  let mut args = env::args().into_iter();
  let command_name = args.next().unwrap();
  loop {
    let Some(arg) = args.next() else { break };
    match arg.as_str() {
      "-h" | "--help" => {
        println!(
          "Usage: {} [-h|--help] [--verbose] [-o|--out] <file>",
          command_name
        );
        process::exit(0);
      }
      "-v" | "--version" => {
        println!("Settlang v{}", env!("CARGO_PKG_VERSION"));
        process::exit(0);
      }
      "--verbose" => built_options.verbose = true,
      "-o" | "--out" => {
        if built_options.output_path.is_some() {
          println!("[error] '-o'/'--out' is specified multiple times");
          process::exit(1);
        }
        built_options.output_path = Some(args.next().unwrap_or_else(|| {
          println!("[error] missing output path after '{}'", arg);
          process::exit(1);
        }));
      }
      _ => {
        if built_options.input_path.is_some() {
          println!("[error] please specify only one input file");
          process::exit(1);
        }
        built_options.input_path = Some(arg);
      }
    }
  }

  let Some(input_path) = built_options.input_path else {
    println!("[error] no input file");
    process::exit(1);
  };

  let code_string = fs::read_to_string(input_path).unwrap_or_else(|error| {
    println!("[error] {}", error);
    process::exit(1);
  });
  let mut code = backend::source_code::SourceCode::new(&code_string);
  let program = backend::compile(&mut code).unwrap_or_else(|error| {
    println!("{}", error);
    process::exit(1);
  });
  if built_options.verbose {
    println!("=== compile result\nremaining_len={}", code.remaining_len());
  }
  if code.remaining_len() != 0 {
    println!("[error] code is not consumed entierly");
    if built_options.verbose {
      println!("        remaining=```{}```", code.remaining_code());
    }
    process::exit(1);
  }

  if built_options.verbose {
    println!("Succeeded to compile || {:#?}", program);
  }
  match backend::builder::wasm::build(program) {
    Ok(wasm) => {
      let filename = built_options.output_path.unwrap_or("out.wasm".to_string());
      match fs::File::create(&filename).and_then(|mut file| file.write_all(&wasm)) {
        Ok(_) => {
          if built_options.verbose {
            println!("Saved as '{}'", filename)
          }
        }
        Err(e) => println!("[error] failed to save WASM file : {}", e),
      }
    }
    Err(error) => println!("[error] failed to build WASM file : {}", error),
  }
}
