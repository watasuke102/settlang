fn main() {
}

#[derive(Debug)]
enum ParseError {
  UnexpectedStr,
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

/// If input begins with test, return consumed input
fn expect_str<'s>(input: &'s str, test: &'s str) -> Result<&'s str, ParseError> {
  assert_ne!(test.len(), 0);
  let mut input_chars = input.chars();
  let mut test_chars = test.chars();
  loop {
    // End of test; means every char is matched
    let Some(test_next) = test_chars.next() else {
      return Ok(input_chars.as_str());
    };
    // end of input
    let Some(input_next) = input_chars.next() else {
      break;
    };
    if input_next != test_next {
      break;
    }
  }
  return Err(ParseError::UnexpectedStr);
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
  #[test]
  fn expect_str_returns_same_input_when_input_is_same() {
    let input = "fn";
    let test = "fn";
    let res = expect_str(input, test);
    assert!(res.is_ok());
    assert_eq!(res.unwrap(), "");
  }
  #[test]
  fn expect_str_fails_when_input_is_different() {
    let input = "test";
    let test = "fn";
    let res = expect_str(input, test);
    assert!(res.is_err());
  }
  #[test]
  fn expect_str_fails_when_same_input_with_pre_spaces() {
    let input = "     fn";
    let test = "fn";
    let res = expect_str(input, test);
    assert!(res.is_err());
  }
}
