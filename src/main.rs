fn main() {
}

fn whitespace(mut input: &str) -> &str {
  loop {
    let mut chars = input.chars();
    if chars.next() == Some(' ') {
      input = chars.as_str();
    } else {
      break;
    }
  }
  input
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_whitespace() {
    assert_eq!(whitespace("    "), "");
  }
}
