mod error;
mod parser;
mod tokenizer;

fn main() {
  let input = "fn main() { return 0 }";
  println!("parse result: {:#?}", parse(input));
}

fn parse(input: &str) -> Result<(), error::ParseError> {
  unimplemented!()
}
