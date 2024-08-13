#[derive(Debug)]
pub struct SourceCode {
  code:        Vec<char>,
  current_pos: usize,
}

impl SourceCode {
  pub fn new(code: &str) -> Self {
    SourceCode {
      code:        code.chars().collect(),
      current_pos: 0,
    }
  }
  pub fn pos(&self) -> usize {
    self.current_pos
  }
  pub fn remaining_len(&self) -> usize {
    self.code.len() - self.current_pos
  }
  /// This panics when current_pos is pointing end of code
  pub fn next(&mut self) {
    if self.current_pos >= self.code.len() {
      panic!("Called next() but code is terminated");
    }
    self.current_pos += 1;
  }
  pub fn current(&self) -> Option<char> {
    if self.current_pos >= self.code.len() {
      return None;
    }
    Some(self.code[self.current_pos])
  }
  /// call peak() and next()
  pub fn pop(&mut self) -> Option<char> {
    let Some(c) = self.current() else {
      return None;
    };
    self.next();
    Some(c)
  }
  pub fn skip_space(&mut self) -> &mut Self {
    use crate::parser::*;
    optional(mul(space()))(self).unwrap();
    self
  }
  /// return code[begin, end)
  pub fn substr(&self, begin: usize, end: usize) -> String {
    String::from_iter(&self.code[begin..end])
  }
  #[cfg(test)]
  pub fn remaining_code(&self) -> String {
    self.substr(self.current_pos, self.code.len())
  }
}
