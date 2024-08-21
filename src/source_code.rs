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
  /// just set current_pos but expected to go back
  pub fn unwind(&mut self, prev_pos: usize) {
    self.current_pos = prev_pos;
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
  /// show specified lines of code with '^' mark pointing specified cols
  pub fn pointed_string(&self, pos: &Position) -> String {
    let line_str = pos.lines.to_string();
    [
      format!(
        "  {} | {}",
        line_str,
        String::from_iter(self.code.iter())
          .lines()
          .nth(pos.lines - 1)
          .map(|l| l.to_string())
          .unwrap_or_default()
      ),
      format!(
        "  {} | {}^",
        " ".repeat(line_str.len()),
        " ".repeat(if pos.cols > 1 { pos.cols - 1 } else { 0 })
      ),
    ]
    .join("\n")
  }
  pub fn ranged_string(&self, begin: &Position, end: &Position) -> String {
    assert!(begin.lines <= end.lines);
    let max_line_number_len = end.lines.to_string().len();
    let lines: Vec<String> = String::from_iter(self.code.iter())
      .lines()
      .map(|l| l.to_string())
      .collect();
    (begin.lines..=end.lines)
      .map(|i| {
        format!(
          "  {:width$} | {}",
          i,
          lines.get(i - 1).unwrap_or(&"".to_string()),
          width = max_line_number_len
        )
      })
      .collect::<Vec<String>>()
      .join("\n")
  }
  /// return current position as lines and rows (1-indexed!)
  pub fn lines_and_cols(&self) -> Position {
    let mut pos = Position { lines: 1, cols: 1 };
    for i in 0..self.current_pos {
      pos.cols += 1;
      if self.code[i] == '\n' {
        pos.cols = 1;
        pos.lines += 1;
      }
    }
    pos
  }
  #[cfg(test)]
  pub fn remaining_code(&self) -> String {
    self.substr(self.current_pos, self.code.len())
  }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Position {
  pub lines: usize,
  pub cols:  usize,
}
impl std::fmt::Display for Position {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{}:{}", self.lines, self.cols)
  }
}
