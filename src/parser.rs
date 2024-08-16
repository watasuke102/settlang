use crate::{error::ParseError, source_code::SourceCode};

type Expecter = Box<dyn Fn(&mut SourceCode) -> Result<(), ParseError>>;

/// expect {char which has White_Space property in Unicode, comment}
pub fn space() -> Expecter {
  fn skip_comment(code: &mut SourceCode) -> Result<(), ParseError> {
    // block comment
    let mut is_block = false;
    if code.pop().ok_or(ParseError::EmptyInput)? == '*' {
      is_block = true
    }
    loop {
      match code.pop() {
        None => return Err(ParseError::EmptyInput),
        Some('\n') => {
          if !is_block {
            return Ok(());
          }
        }
        Some('*') => {
          if is_block {
            if code.pop().ok_or(ParseError::EmptyInput)? == '#' {
              return Ok(());
            }
          }
        }
        _ => (),
      }
    }
  }

  Box::new(|code| {
    let c = code.current().ok_or(ParseError::EmptyInput)?;
    if c == '#' {
      code.next();
      return skip_comment(code);
    } else if c.is_whitespace() {
      code.next();
      Ok(())
    } else {
      Err(ParseError::NoMatch)
    }
  })
}

pub fn num() -> Expecter {
  Box::new(|code| {
    let c = code.current().ok_or(ParseError::EmptyInput)?;
    match c {
      '0'..='9' => {
        code.next();
        Ok(())
      }
      _ => Err(ParseError::NoMatch),
    }
  })
}
pub fn alpha() -> Expecter {
  Box::new(|code| {
    let c = code.current().ok_or(ParseError::EmptyInput)?;
    match c {
      'A'..='Z' | 'a'..='z' => {
        code.next();
        Ok(())
      }
      _ => Err(ParseError::NoMatch),
    }
  })
}

pub fn char(expect: char) -> Expecter {
  Box::new(move |code| {
    let c = code.current().ok_or(ParseError::EmptyInput)?;
    if c == expect {
      code.next();
      Ok(())
    } else {
      Err(ParseError::NoMatch)
    }
  })
}
pub fn str(expect: &'static str) -> Expecter {
  Box::new(move |code| {
    let mut matched = String::new();
    let mut expect_input = expect.chars();
    loop {
      let Some(expect_char) = expect_input.next() else {
        return Ok(());
      };
      let code_char = code.current().ok_or(ParseError::EmptyInput)?;
      if code_char == expect_char {
        matched.push(code_char);
        code.next();
      } else {
        if matched.len() == 0 {
          return Err(ParseError::NoMatch);
        } else {
          return Err(ParseError::PartialMatch(matched));
        }
      }
    }
  })
}

pub fn mul(expecter: Expecter) -> Expecter {
  Box::new(move |code| {
    let initial_pos = code.pos();
    while let Ok(()) = expecter(code) {}
    if code.pos() == initial_pos {
      return Err(ParseError::NoMatch);
    } else {
      return Ok(());
    }
  })
}
pub fn seq(expecters: Vec<Expecter>) -> Expecter {
  Box::new(move |code| {
    for expecter in &expecters {
      if let Err(e) = expecter(code) {
        return Err(e);
      }
    }
    Ok(())
  })
}
pub fn or(expecters: Vec<Expecter>) -> Expecter {
  Box::new(move |code| {
    for expecter in &expecters {
      match expecter(code) {
        Ok(()) => return Ok(()),
        Err(ParseError::NoMatch) => (),
        Err(e) => return Err(e),
      }
    }
    Err(ParseError::NoMatch)
  })
}
/// ignore ParseError::NoMatch
pub fn optional(expecter: Expecter) -> Expecter {
  Box::new(move |code| match expecter(code) {
    Ok(consumed) => return Ok(consumed),
    Err(ParseError::NoMatch) => return Ok(()),
    Err(e) => return Err(e),
  })
}

/// return (remaining code, retrieved string)
pub fn consumed(code: &mut SourceCode, expecter: Expecter) -> Result<String, ParseError> {
  let begin = code.pos();
  expecter(code)?;
  Ok(code.substr(begin, code.pos()))
}

#[cfg(test)]
mod test {
  use super::*;
  /// [( input code, Ok(consumed) or Err(error kind) )].map( tester(Expecter) )
  fn tester(f: Expecter) -> impl Fn((&str, Result<&str, ParseError>)) {
    move |(code_str, expect)| {
      let mut code = SourceCode::new(code_str);
      match f(&mut code) {
        Ok(()) => {
          let expect_remaining = expect.expect("returned Ok but expected Err");
          assert_eq!(code.remaining_code(), expect_remaining);
        }
        Err(err) => {
          let expect_err = expect.expect_err("returned Err but expected Ok");
          assert_eq!(err, expect_err, "input: {}", code_str);
        }
      }
    }
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
      (
        r"# one-line comment
end",
        Ok("end"),
      ),
      (
        r"#* this is 
         block comment
         *#end",
        Ok("end"),
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
    [
      ("return", Ok("")),
      ("12345", Err(ParseError::NoMatch)),
      ("retval", Err(ParseError::PartialMatch("ret".to_string()))),
    ]
    .map(tester(str("return")));
  }

  #[test]
  fn test_mul() {
    tester(mul(alpha()))(("abcde12345", Ok("12345")));
    tester(mul(space()))((" hi", Ok("hi")));
    tester(mul(num()))(("abcde", Err(ParseError::NoMatch)));
  }
  #[test]
  fn test_seq() {
    [("a 0", Ok("")), ("1", Err(ParseError::NoMatch))].map(tester(seq(vec![
      alpha(),
      space(),
      num(),
    ])));
  }
  #[test]
  fn test_or() {
    [
      ("  aa", Ok(" aa")),
      ("012", Ok("12")),
      ("test", Ok("est")),
      ("++a", Err(ParseError::NoMatch)),
    ]
    .map(tester(or(vec![space(), num(), alpha()])));
  }
  #[test]
  fn test_optional() {
    let code = "1234";
    #[rustfmt::skip] {
      // Ensure the premise that `alpha("1234")` should return ParseError::NoMatch
      tester(         alpha() ) ((code, Err(ParseError::NoMatch)));
      tester(optional(alpha())) ((code, Ok(code)));
    }
  }
  #[test]
  #[rustfmt::skip]
  fn test_consumed() {
    assert_eq!(consumed(&mut SourceCode::new("test"),       alpha()),      Ok("t".to_string()));
    assert_eq!(consumed(&mut SourceCode::new("helloworld"), str("hello")), Ok("hello".to_string()));
    assert_eq!(consumed(&mut SourceCode::new("test"),       num()),        Err(ParseError::NoMatch));
  }
}
