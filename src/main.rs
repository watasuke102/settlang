mod error;
mod parser;
mod tokenizer;

fn main() {
  let input = r"
fn main() {
  return 0 
}
fn test() {
  return 128
}
";
  println!("parse result: {:#?}", tokenizer::expect_code(input));
}
