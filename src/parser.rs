use crate::error::ParseError;

// pub type Expection<'s> = ;
type Expecter = Box<dyn Fn(&str) -> Result</*input:*/ &str, ParseError>>;

/// expect char which has White_Space property in Unicode
pub fn space() -> Expecter {
  Box::new(|input| {
    let mut chars = input.chars();
    let c = chars.next().ok_or(ParseError::EmptyInput)?;
    if c.is_whitespace() {
      Ok(chars.as_str())
    } else {
      Err(ParseError::NoMatch)
    }
  })
}
pub fn num() -> Expecter {
  Box::new(|input| {
    let mut chars = input.chars();
    let c = chars.next().ok_or(ParseError::EmptyInput)?;
    match c {
      '0'..='9' => Ok(chars.as_str()),
      _ => Err(ParseError::NoMatch),
    }
  })
}
pub fn alpha() -> Expecter {
  Box::new(|input| {
    let mut chars = input.chars();
    let c = chars.next().ok_or(ParseError::EmptyInput)?;
    match c {
      'A'..='Z' | 'a'..='z' => Ok(chars.as_str()),
      _ => Err(ParseError::NoMatch),
    }
  })
}

pub fn char(expect: char) -> Expecter {
  Box::new(move |input| {
    let mut chars = input.chars();
    let c = chars.next().ok_or(ParseError::EmptyInput)?;
    if c == expect {
      Ok(chars.as_str())
    } else {
      Err(ParseError::NoMatch)
    }
  })
}
pub fn str(expect: String) -> Expecter {
  Box::new(move |input| {
    let mut input_chars = input.chars();
    let mut expect_chars = expect.as_str().chars();
    loop {
      let Some(expect_c) = expect_chars.next() else {
        return Ok(input_chars.as_str());
      };
      let input_c = input_chars.next().ok_or(ParseError::EmptyInput)?;
      if input_c != expect_c {
        return Err(ParseError::NoMatch);
      }
    }
  })
}

pub fn mul(expecter: Expecter) -> Expecter {
  Box::new(move |original_input| {
    let mut input = original_input;
    loop {
      match expecter(input) {
        Ok(consumed) => input = consumed,
        Err(_) => {
          if input == original_input {
            return Err(ParseError::NoMatch);
          } else {
            return Ok(input);
          }
        }
      }
    }
  })
}
pub fn seq(expecters: Vec<Expecter>) -> Expecter {
  Box::new(move |mut input| {
    for expecter in &expecters {
      match expecter(input) {
        Ok(consumed) => input = consumed,
        Err(e) => return Err(e),
      }
    }
    Ok(input)
  })
}
pub fn or(expecters: Vec<Expecter>) -> Expecter {
  Box::new(move |input| {
    for expecter in &expecters {
      match expecter(input) {
        Ok(input) => return Ok(input),
        Err(ParseError::NoMatch) => (),
        Err(e) => return Err(e),
      }
    }
    Err(ParseError::NoMatch)
  })
}
/// ignore ParseError::NoMatch
pub fn optional(expecter: Expecter) -> Expecter {
  Box::new(move |input| match expecter(input) {
    Ok(consumed) => return Ok(consumed),
    Err(ParseError::NoMatch) => return Ok(input),
    Err(e) => return Err(e),
  })
}

/// return (remaining input, retrieved string)
pub fn consumed(input: &str, expecter: Expecter) -> Result<(&str, &str), ParseError> {
  let consumed = expecter(input)?;
  let offset = (consumed.as_ptr() as usize) - (input.as_ptr() as usize);
  Ok((&input[..offset], consumed))
}

#[cfg(test)]
mod test {
  use super::*;
  fn tester(f: Expecter) -> impl Fn((&str, Result<&str, ParseError>)) {
    move |(input, expect)| assert_eq!(f(input), expect)
  }

  #[test]
  fn test_space() {
    [
      (" ", Ok("")),
      ("	<-Tab", Ok("<-Tab")),
      (
        r"
hello",
        Ok("hello"),
      ),
      ("abc", Err(ParseError::NoMatch)),
    ]
    .map(tester(space()));
  }
  #[test]
  fn test_num() {
    [
      ("123", Ok("23")),
      ("abc", Err(ParseError::NoMatch)),
      ("四", Err(ParseError::NoMatch)),
    ]
    .map(tester(num()));
  }
  #[test]
  fn test_alpha() {
    [("abc", Ok("bc")), ("123", Err(ParseError::NoMatch))].map(tester(alpha()));
  }
  #[test]
  fn test_char() {
    [("_test", Ok("test")), ("test", Err(ParseError::NoMatch))].map(tester(char('_')));
  }
  #[test]
  fn test_str() {
    [("return", Ok("")), ("12345", Err(ParseError::NoMatch))]
      .map(tester(str("return".to_string())));
  }

  #[test]
  fn test_mul() {
    assert_eq!(mul(alpha())("abcde12345"), Ok("12345"));
    assert_eq!(mul(space())(" hi"), Ok("hi"));
    assert_eq!(mul(num())("abcde"), Err(ParseError::NoMatch));
  }
  #[test]
  fn test_seq() {
    let seq = seq(vec![alpha(), space(), num()]);
    assert_eq!(seq("a 0"), Ok(""));
    assert_eq!(seq("1"), Err(ParseError::NoMatch));
  }
  #[test]
  fn test_or() {
    let or = or(vec![space(), num(), alpha()]);
    assert_eq!(or("  aa"), Ok(" aa"));
    assert_eq!(or("012"), Ok("12"));
    assert_eq!(or("test"), Ok("est"));
    assert_eq!(or("++a"), Err(ParseError::NoMatch));
  }
  #[test]
  fn test_optional() {
    let input = "1234";
    // Ensure the premise that `alpha("1234")` should return ParseError::NoMatch
    #[rustfmt::skip] 
    assert_eq!(        (alpha())(input), Err(ParseError::NoMatch));
    assert_eq!(optional(alpha())(input), Ok(input));
  }
  #[test]
  fn test_consumed() {
    assert_eq!(consumed("test", alpha()), Ok(("t", "est")));
    assert_eq!(
      consumed("helloworld", str("hello".to_string())),
      Ok(("hello", "world"))
    );
    assert_eq!(consumed("test", num()), Err(ParseError::NoMatch));
  }
}
