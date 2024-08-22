/// convert given number to signed LEB128 formatted byte array
pub fn to_signed_leb128(mut input: i64) -> Vec<u8> {
  let mut res = Vec::new();
  loop {
    let lower7: u8 = (input & 0b111_1111) as u8;
    input >>= 7;
    if (input == 0 && (lower7 & 0b100_0000) == 0) || (input == -1 && (lower7 & 0b100_0000) != 0) {
      res.push(lower7);
      return res;
    } else {
      res.push(lower7 | 0b1000_0000);
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  #[test]
  fn test_to_signed_leb128() {
    for (input, expect) in [
      // [LEB128 - Wikipedia](https://en.wikipedia.org/wiki/LEB128)
      (624485, vec![0xE5, 0x8E, 0x26]),
      (-123456, vec![0xC0, 0xBB, 0x78]),
      // [DWARF Debugging Information Format Version 5](https://dwarfstd.org/doc/DWARF5.pdf)
      // from Table 7.8 in Section 7.6
      (2, vec![2]),
      (-2, vec![0x7e]),
      (127, vec![127 + 0x80, 0]),
      (-127, vec![1 + 0x80, 0x7f]),
      (128, vec![0 + 0x80, 1]),
      (-128, vec![0 + 0x80, 0x7f]),
      (129, vec![1 + 0x80, 1]),
      (-129, vec![0x7f + 0x80, 0x7e]),
    ] {
      assert_eq!(to_signed_leb128(input), expect, "input: {}", input);
    }
  }
}
