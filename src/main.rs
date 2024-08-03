fn main() {
}

/// expect 0 or more (space | linebreak)s
fn space_0(mut input: &str) -> &str {
  loop {
    let mut chars = input.chars();
    if !chars.next().unwrap_or('0').is_whitespace() {
      break;
    }
    input = chars.as_str();
  }
  input
}

/// If input begins with test, consume it and return (true, <consumed input>)
/// otherwise return (false, <non-consumed input>)
fn expect_str<'s>(input: &'s str, test: &'s str) -> (bool, &'s str) {
  assert_ne!(test.len(), 0);
  let mut input_chars = input.chars();
  let mut test_chars = test.chars();
  loop {
    // End of test; means every char is matched
    let Some(test_next) = test_chars.next() else {
      return (true, input_chars.as_str());
    };
    // end of input
    let Some(input_next) = input_chars.next() else {
      break;
    };
    if input_next != test_next {
      break;
    }
  }
  return (false, input);
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_whitespace() {
    assert_eq!(space_0("    "), "");
    assert_eq!(space_0("    fn  "), "fn  ");
  }
  #[test]
  fn test_linebreak() {
    assert_eq!(
      space_0(
        r"  
  "
      ),
      ""
    );
  }
  #[test]
  fn expect_str_is_true_when_same_input() {
    let input = "fn";
    let test = "fn";
    let (is_matched, returned_input) = expect_str(input, test);
    assert!(is_matched);
    assert_eq!(returned_input, "");
  }
  #[test]
  fn expect_str_is_false_if_different_input() {
    let input = "test";
    let test = "fn";
    let (is_matched, returned_input) = expect_str(input, test);
    assert!(!is_matched);
    assert_eq!(returned_input, input);
  }
  #[test]
  fn expect_str_is_false_when_same_input_with_pre_spaces() {
    let input = "     fn";
    let test = "fn";
    let (is_matched, returned_input) = expect_str(input, test);
    assert!(!is_matched);
    assert_eq!(returned_input, input);
  }
}
