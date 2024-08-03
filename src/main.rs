fn main() {
  let input = "fn main() { return 0 }";
  println!("parse result: {:#?}", parse(input));
}

fn parse(input: &str) -> Result<(), ParseError> {
  unimplemented!()
}

#[derive(Debug)]
enum ParseError {
  UnexpectedStr,
  UnexpectedChar,
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
/// expect 1 or more (space | linebreak)s
/// if input does not begin with whitespace, raise UnexpectedChar
fn space_1(input: &str) -> Result<&str, ParseError> {
  let mut chars = input.chars();
  if !chars.next().unwrap_or('0').is_whitespace() {
    return Err(ParseError::UnexpectedChar);
  }
  Ok(space_0(chars.as_str()))
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

/// If input begins with c, return consumed input
fn expect_char(input: &str, c: char) -> Result<&str, ParseError> {
  let mut chars = input.chars();
  if chars.next() == Some(c) {
    Ok(chars.as_str())
  } else {
    Err(ParseError::UnexpectedChar)
  }
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
  fn space1_remove_whitespaces_when_input_begins_with_alphabet() {
    let input = "     main";
    let res = space_1(input);
    assert!(res.is_ok());
    assert_eq!(res.unwrap(), "main");
  }
  #[test]
  fn space1_fails_when_input_begins_with_alphabet() {
    let input = "main";
    assert!(space_1(input).is_err());
  }
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
  #[test]
  fn expect_char_returns_consumed_input_when_input_begins_with_same_with_test() {
    let input = "ABCDE";
    let test = 'A';
    let res = expect_char(input, test);
    assert!(res.is_ok());
    assert_eq!(res.unwrap(), "BCDE");
  }
  #[test]
  fn expect_char_fails_when_input_with_different_char_with_test() {
    let input = "ABCDE";
    let test = 'X';
    let res = expect_char(input, test);
    assert!(res.is_err());
  }
}
